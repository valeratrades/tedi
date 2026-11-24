#!/usr/bin/env node
// An edit shows before it lands, lands as shown, and is taken back when it can't: the real client over the seeded
// mock, with a proxy between them that holds every edit back, or answers it in the server's place.
// pending.mjs <port of a running e2e server>
import assert from "node:assert/strict";
import { spawn } from "node:child_process";
import { mkdtempSync, rmSync } from "node:fs";
import http from "node:http";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { setTimeout as sleep } from "node:timers/promises";

const server = Number(process.argv[2]);
assert(server, "pending.mjs <port of a running e2e server>");
const ISSUE = "https://github.com/service-arb/review_archive/issues/46";
const HELD_MS = 1500;

const sent = []; // each edit's body, in the order they left the client
let answer = null; // in the server's place: {status, body}
const proxy = http.createServer(async (req, res) => {
  const chunks = [];
  for await (const c of req) chunks.push(c);
  const body = Buffer.concat(chunks);
  if (/^\/api\/(edit|move_card)\d+/.test(req.url)) {
    sent.push(JSON.parse(body));
    const instead = answer;
    await sleep(HELD_MS);
    if (instead) {
      res.writeHead(instead.status, { "content-type": "application/json" });
      return res.end(JSON.stringify(instead.body));
    }
  }
  const up = http.request({ port: server, path: req.url, method: req.method, headers: req.headers }, (r) => {
    res.writeHead(r.statusCode, r.headers);
    res.flushHeaders(); // the change stream is open before its first tick
    r.pipe(res);
  });
  up.on("error", () => res.destroy());
  res.on("close", () => up.destroy()); // the change stream never ends by itself
  up.end(body);
});
await new Promise((r) => proxy.listen(0, "127.0.0.1", r));

// what a step below 7 takes a card out of
const viewed = { name: "Triage", query: "is:issue is:open i:>=7", group_by: "state:kanban", sort: "importance", shown: ["repo", "number", "importance"] };
const kept = await fetch("http://127.0.0.1:" + server + "/api/library/edit", {
  method: "POST",
  headers: { "content-type": "application/json" },
  body: JSON.stringify({ edit: { Viewed: viewed } }),
});
assert(kept.ok, await kept.text());

const profile = mkdtempSync(join(tmpdir(), "tedi-e2e-"));
const chromium = spawn("chromium", ["--headless=new", "--no-sandbox", "--disable-gpu", "--window-size=1440,900", "--remote-debugging-port=0", "--user-data-dir=" + profile, "about:blank"], { stdio: ["ignore", "ignore", "pipe"] });
const debugPort = await new Promise((found) => {
  let said = "";
  chromium.stderr.on("data", (d) => {
    const at = /ws:\/\/127\.0\.0\.1:(\d+)/.exec((said += d));
    if (at) found(at[1]);
  });
});
const page = await (await fetch("http://127.0.0.1:" + debugPort + "/json/new?" + encodeURIComponent("http://127.0.0.1:" + proxy.address().port + "/"), { method: "PUT" })).json();
const ws = new WebSocket(page.webSocketDebuggerUrl);
await new Promise((r) => (ws.onopen = r));
let calls = 0;
const waiting = new Map();
ws.onmessage = ({ data }) => {
  const { id, result } = JSON.parse(data);
  waiting.get(id)?.(result);
};
const cdp = (method, params) => new Promise((r) => (waiting.set(++calls, r), ws.send(JSON.stringify({ id: calls, method, params }))));
const js = async (expression) => {
  const { result, exceptionDetails } = await cdp("Runtime.evaluate", { expression, returnByValue: true, awaitPromise: true });
  assert(!exceptionDetails, exceptionDetails?.exception?.description);
  return result.value;
};
const until = async (what, holds, ms = 15000) => {
  const end = Date.now() + ms;
  while (Date.now() < end) {
    if (await holds()) return;
    await sleep(25);
  }
  assert.fail("never: " + what + "\n" + (await js("document.body.innerText")));
};
const shows = (text) => js("document.body.innerText.includes(" + JSON.stringify(text) + ")");

// the issue's importance on every card and tab showing it: they never disagree
const STEPPERS = '[...document.querySelectorAll("[aria-label=\'set i\'],[aria-label=\'set importance\']")].map((t) => t.closest("span.inline-flex"))';
const PANEL = STEPPERS + '.filter((s) => s.closest(".min-h-\\\\[30px\\\\]"))[0]';
const importance = async () => (await js(PANEL + "?.innerText.match(/i\\s*([0-9?])/)?.[1]")) ?? null;
const click = (title) => js(PANEL + '.querySelector("button[title*=' + JSON.stringify(title).replaceAll('"', "'") + ']").click()');
const stored = async () => (await (await fetch("http://127.0.0.1:" + server + "/api/board")).text()).trim().split("\n").flatMap((l) => JSON.parse(l).Read?.cards ?? []).find((c) => c.link === ISSUE);
const UNDO = '[...document.querySelectorAll("button")].find((b) => b.innerText.trim() === "Undo" && b.parentElement.innerText.includes("labels on #46"))';
const undo = () => js(UNDO + ".click()");

let failed = null;
try {
  await until("the issue's tab loads", async () => (await importance()) !== null);
  const { importance: before } = await stored();
  assert(before <= 6, "seeded at 6");

  // a run of clicks shows at once, and is undone as fast, a click at a time; it lands as its end, in as few edits as it can
  const start = Date.now();
  for (let i = before + 1; i <= 9; i++) {
    await click("raise");
    await until("shows i " + i, async () => (await importance()) === String(i), 400);
  }
  assert(Date.now() - start < HELD_MS, "every click showed while the first edit was still held");
  assert(await js(PANEL + '.querySelector("button[title=\'already the highest\']")?.disabled'), "9 is the highest: nothing to raise to");
  assert(await shows("Edited labels on #46"), "told, and offered to be undone, at the click");
  await undo();
  await until("undone: shows i 8, what the click saw", async () => (await importance()) === "8", 400);
  assert.equal(sent.length, 1, "the first click is still held");
  await until("it lands", async () => (await stored()).importance === 8);
  assert.equal(await importance(), "8");
  assert.equal(sent.length, 2, "the first click, then the rest as one: " + JSON.stringify(sent));
  for (let i = 7; i >= before; i--) {
    await undo();
    await until("back to i " + i + ", landed as it is", async () => (await importance()) === String(i), 400);
  }
  assert(!(await js("!!" + UNDO)), "every click is undone: none left");
  await until("it lands", async () => (await stored()).importance === before);
  for (let i = before + 1; i <= 9; i++) {
    await click("raise");
    await until("shows i " + i, async () => (await importance()) === String(i), 400);
  }
  await until("it lands", async () => (await stored()).importance === 9);

  // refused: the value comes back, and what didn't go through is told
  await sleep(HELD_MS);
  answer = { status: 500, body: { ServerError: { message: "GitHub is down", code: 500 } } };
  await click("lower");
  await until("shows i 8", async () => (await importance()) === "8", 400);
  await until("the failure is told", () => shows("didn't go through"));
  assert.equal(await importance(), "9", "taken back");

  // clashed, with a click queued behind: both are taken back, and yours is the last value asked for
  answer = { status: 200, body: { Conflict: { link: ISSUE, files: [], clashes: [{ node: ISSUE, field: "labels", consensus: "i:9", local: "i:8", remote: "i:2, theirs" }] } } };
  sent.length = 0;
  await click("lower");
  await until("shows i 8", async () => (await importance()) === "8", 400);
  await until("the first left", async () => sent.length === 1);
  await click("lower");
  await until("shows i 7", async () => (await importance()) === "7", 400);
  await until("the clash is told", () => shows("changed on GitHub"));
  assert(!(await shows("Edited labels on #46")), "nothing left to undo");
  assert.equal(await importance(), "9", "both taken back");
  await until("yours and theirs are offered", async () => (await shows("i:2, theirs")) && (await shows("i:7")));
  await sleep(HELD_MS);
  assert.equal(sent.length, 1, "what queued behind a clash never leaves");

  // every property shows the same way: here, the state
  answer = null;
  const button = (text) => '[...document.querySelectorAll("button")].find((b) => b.innerText.trim() === ' + JSON.stringify(text) + ")";
  await js(button("Close") + ".click()");
  await until("shows closed", async () => js("!!" + button("Reopen")), 400);
  await js(button("Reopen") + ".click()");
  await until("shows open", async () => js("!!" + button("Close")), 400);
  await until("both land", async () => sent.length === 3 && (await stored()).state === "Open");

  // a step takes its card out of the view, the stepper it was made on with it: it still lands
  sent.length = 0;
  const card = STEPPERS + '.filter((s) => !s.closest(".min-h-\\\\[30px\\\\]") && s.parentElement.parentElement.innerText.includes("#46"))[0]';
  const onCard = async () => (await js(card + "?.innerText.match(/i\\s*([0-9?])/)?.[1]")) ?? null;
  for (const to of ["8", "7"]) {
    await js(card + ".querySelector('button[title=lower]').click()");
    await until("the card shows i " + to, async () => (await onCard()) === to, 400);
  }
  await js(card + ".querySelector('button[title=lower]').click()");
  await until("the card leaves", async () => (await onCard()) === null, 400);
  assert.equal(await importance(), "6", "its tab, still open, agrees");
  await undo();
  await until("undone: the card is back", async () => (await onCard()) === "7", 400);
  await undo();
  await until("and the click before", async () => (await onCard()) === "8", 400);
  await until("it lands", async () => (await stored()).importance === 8);
  console.log("ok");
} catch (e) {
  failed = e;
} finally {
  const gone = new Promise((r) => chromium.on("exit", r));
  cdp("Browser.close"); // a kill would leave its children writing to the profile
  await gone;
  proxy.closeAllConnections();
  proxy.close();
  rmSync(profile, { recursive: true, force: true });
}
if (failed) {
  console.error(failed);
  process.exit(1);
}
