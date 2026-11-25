import { execFileSync } from "node:child_process";
import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import http from "node:http";

import type { FullConfig } from "@playwright/test";

import { PORT } from "./playwright.config";

const here = new URL("..", import.meta.url).pathname;
const SERVER = PORT + 1;
const HOME = "/tmp/tedi-e2e-specs";
const VIEWERS = 128; // more than a run has tests: none is signed in as anew with its tab still open, and the library's counts of viewers hold still

const read = (name: string) => JSON.parse(readFileSync(`${here}libraries/${name}.json`, "utf8"));

/** The seeded board, built as the sources are, behind a proxy signing each browser context in as a viewer of its own. */
export default async function stack({ metadata }: FullConfig) {
  const env = { ...process.env, E2E_HOME: HOME, E2E_PORT: String(SERVER), E2E_RESEED: "1", E2E_REBUILD: "1" };
  execFileSync(`${here}serve.sh`, ["--viewer-header", "X-Auth-User"], { env, stdio: ["ignore", 2, 2] });

  const library = read("04-workspace");
  library.workspaces["Release week"].tiles.Sm = read("06a-mobile").workspaces["Phone: inbox"].tiles.Sm;
  const dir = `${HOME}/data/tedi/web`;
  mkdirSync(dir, { recursive: true });
  const sign = (n: number) => writeFileSync(`${dir}/viewer-${n}.json`, JSON.stringify(library));
  for (let n = 0; n < VIEWERS; n++) sign(n);

  let next = 0;
  const proxy = http.createServer((req, res) => {
    if (metadata.still && /^\/api\/changes/.test(req.url!)) return void res.writeHead(200, { "content-type": "text/event-stream" }).end();
    let viewer = /(?:^|; )viewer=([\w-]+)/.exec(req.headers.cookie ?? "")?.[1]; // a test's own has no library until it edits one
    const fresh = viewer === undefined;
    if (viewer === undefined) {
      const n = next++ % VIEWERS;
      sign(n);
      viewer = String(n);
    }
    const up = http.request({ port: SERVER, path: req.url, method: req.method, headers: { ...req.headers, "x-auth-user": `viewer-${viewer}` } }, (r) => {
      res.writeHead(r.statusCode!, { ...r.headers, ...(fresh ? { "set-cookie": `viewer=${viewer}; Path=/` } : {}) });
      res.flushHeaders(); // the change stream is open before its first tick
      r.pipe(res);
    });
    up.on("error", () => res.destroy());
    res.on("close", () => up.destroy()); // a closed page's change stream
    req.pipe(up);
  });
  await new Promise<void>((on) => proxy.listen(PORT, "127.0.0.1", on));
  return () => {
    proxy.closeAllConnections();
    proxy.close();
    execFileSync(`${here}serve.sh`, ["stop"], { env });
  };
}
