import { expect, type Page, test } from "@playwright/test";

test.skip(({ viewport }) => viewport!.width < 768, "the shelves are behind a sheet");

const SETTLED = 1500; // past the autosave
const at = (page: Page) => page.evaluate(() => location.pathname + location.search);
const shelf = (page: Page, name: string) => page.locator("aside").getByRole("link", { name, exact: true });
const scope = (page: Page, name: string) => page.locator("aside").getByRole("button", { name, exact: true });
const line = (page: Page) => page.getByRole("group", { name: "Scope", exact: true });
const search = (page: Page) => page.getByRole("textbox", { name: "Search", exact: true });
const sort = (page: Page) => page.getByRole("group", { name: "Sort", exact: true }).locator("[draggable=true]");
const named = (page: Page) => page.getByRole("button", { name: "Workspace menu" });
const tiles = (page: Page) => page.locator(".tl-tile");
const view = (page: Page, name: string) => page.locator(`.tl-tab[data-panel="view:${name}"]`);
const column = (page: Page, name: string) => page.locator(`.tl-tab[data-panel="col:${name}"]`);
const toast = (page: Page, text: string) => page.locator("div").filter({ hasText: text }).last();
/** Whether the tiles end at the dock's bottom, whatever the bars above it and the window leave. */
const fits = (page: Page) => page.locator(".tl-packed").first().evaluate((dock) => dock.scrollHeight - dock.clientHeight);
const library = async (page: Page) => (await page.request.get("/api/library")).json();
const edit = async (page: Page, edit: unknown) => {
  const kept = await page.request.post("/api/library/edit", { data: { edit } });
  expect(kept.ok(), await kept.text()).toBe(true);
};
const open = async (page: Page, name: string) => {
  await shelf(page, name).click();
  await expect(named(page)).toHaveText(name);
};
/** A viewer who never opened the board. */
const fresh = (page: Page, baseURL: string) => page.context().addCookies([{ name: "viewer", value: `fresh-${Math.random().toString(36).slice(2)}`, url: baseURL }]);

test("ws 1 is there from the first open, and holds nothing until something is opened in it", async ({ page, baseURL }) => {
  await fresh(page, baseURL!);
  await page.goto("/");
  await expect(named(page)).toHaveText("ws 1");
  await expect.poll(() => at(page)).toMatch(/^\/\?ws=\w{8}$/);
  await expect(search(page)).toHaveValue("");
  await expect(sort(page)).toHaveText(["rank ↓"]);
  await expect(tiles(page)).toHaveCount(0);
  const first = await at(page);
  await page.waitForTimeout(SETTLED);
  expect((await library(page)).workspaces["ws 1"].tiles).toEqual({});

  await shelf(page, "Triage").click();
  await expect(view(page, "Triage")).toBeVisible();
  await page.waitForTimeout(SETTLED);
  await page.goto("/");
  await expect(named(page)).toHaveText("ws 1");
  await expect(view(page, "Triage")).toBeVisible();
  expect(new URL(page.url()).searchParams.get("ws")).toBe(new URL(first, page.url()).searchParams.get("ws"));
});

test("with none open nothing is shown where views go, and opening one only says so", async ({ page, baseURL }) => {
  await fresh(page, baseURL!);
  await page.goto("/");
  await shelf(page, "Triage").click();
  await expect(view(page, "Triage")).toBeVisible();
  const row = page.locator("aside .group\\/row").filter({ hasText: "ws 1" });
  await row.hover();
  await row.getByRole("button", { name: "Delete" }).click();
  await row.getByText("delete?").click();
  for (const reloaded of [false, true]) {
    if (reloaded) await page.goto("/");
    await expect(named(page)).toHaveText("Workspace");
    await expect(tiles(page)).toHaveCount(0);
    await expect(search(page)).toHaveCount(0);
    await shelf(page, "Triage").click();
    await expect(page.getByText("no workspace is open")).toBeVisible();
    await expect(tiles(page)).toHaveCount(0);
  }
  await page.locator("aside").getByRole("button", { name: "New workspace" }).click();
  await expect(named(page)).toHaveText("ws 1");
});

test("each workspace has its own lens, over every tile of it", async ({ page }) => {
  await page.goto("/");
  await open(page, "Triage desk");
  await expect(search(page)).toHaveValue(/^is:issue is:open state:/);
  await search(page).fill("is:open t:bug");
  await page.locator("aside").getByRole("button", { name: "New workspace" }).click(); // before it settled
  await expect(named(page)).toHaveText("ws 1");
  await expect(search(page)).toHaveValue("");
  await expect(line(page)).not.toContainText("repo:");
  await search(page).fill("is:pr");
  await open(page, "Triage desk");
  await expect(search(page)).toHaveValue("is:open t:bug");
  await expect(line(page)).toContainText("repo:service-arb/*");
  await search(page).fill("is:open i:many"); // doesn't parse: not saved
  await page.waitForTimeout(SETTLED);
  await page.reload();
  await expect(search(page)).toHaveValue("is:open t:bug");
  await open(page, "ws 1");
  await expect(search(page)).toHaveValue("is:pr");
});

test("two tabs on two workspaces leave each other's alone", async ({ page, context }) => {
  await page.goto("/");
  await open(page, "Triage desk");
  const other = await context.newPage();
  await other.goto("/?ws=SprintPl");
  await expect(named(other)).toHaveText("Sprint planning");
  await search(page).fill("is:open t:bug");
  await page.waitForTimeout(SETTLED);
  const { workspaces } = await library(page);
  expect(workspaces["Triage desk"].lens.search).toBe("is:open t:bug");
  expect(workspaces["Sprint planning"].lens.search).toBe("is:issue is:open");
  await expect(named(page)).toHaveText("Triage desk");
});

test("a scope is a starting point: its terms are copied, undone, edited apart", async ({ page }) => {
  await page.goto("/");
  await open(page, "Triage desk");
  await page.getByRole("button", { name: "Start from" }).click();
  await page.getByRole("button", { name: /^core/ }).last().click();
  await expect(line(page)).toContainText("repo:valeratrades/tedi,service-arb/*");
  await toast(page, "Started from core").getByRole("button", { name: "Undo" }).click();
  await expect(line(page)).toContainText("repo:service-arb/*");

  await scope(page, "core").click();
  await expect(line(page)).toContainText("repo:valeratrades/tedi,service-arb/*");
  await line(page).getByRole("button", { name: "Add a term to the scope" }).click();
  await page.getByPlaceholder("repo:owner/*, is:issue, t:bug").fill("t:bug");
  await page.keyboard.press("Enter");
  await expect(line(page)).toContainText("t:bug");
  await line(page).getByRole("button", { name: "Drop repo:valeratrades/tedi,service-arb/*" }).click();
  await expect(line(page)).not.toContainText("repo:");
  await page.waitForTimeout(SETTLED);
  await page.reload();
  await expect(line(page)).toContainText("t:bug");
  await expect(line(page)).not.toContainText("repo:");
});

test("a point of the chain is a scope: down by the picker, up by its crumbs, and left", async ({ page }) => {
  await page.goto("/");
  await open(page, "Triage desk");
  const crumbs = line(page).getByRole("navigation");
  for (const [at, shown] of [["ev-main", "ev-main"], ["service-arb", "ev-main/service-arb"], ["review_archive", "ev-main/service-arb/review_archive"]]) {
    await page.getByRole("button", { name: "Start from" }).click();
    await page.getByRole("button", { name: new RegExp(`^${at}`) }).last().click();
    await expect(crumbs).toHaveText(shown);
  }
  await expect(line(page).getByRole("button", { name: "Add a term to the scope" })).toHaveCount(0);
  await crumbs.getByRole("button", { name: "service-arb" }).click();
  await expect(crumbs).toHaveText("ev-main/service-arb");
  await page.waitForTimeout(SETTLED);
  expect((await library(page)).workspaces["Triage desk"].lens.scope).toBe("https://github.com/service-arb");
  await page.reload();
  await expect(crumbs).toHaveText("ev-main/service-arb");
  await line(page).getByRole("button", { name: "Leave this scope" }).click();
  await expect(crumbs).toHaveCount(0);
  await expect(line(page)).not.toContainText("repo:");
});

test("a column's × and the checklist hide the same, and it stays hidden", async ({ page }) => {
  await page.goto("/");
  await open(page, "Triage desk");
  await expect(column(page, "done")).toHaveCount(0); // the team hides it
  await expect(column(page, "in_review")).toBeVisible();
  await column(page, "in_review").getByTitle("Close tab").click();
  await expect(column(page, "in_review")).toHaveCount(0);
  const checklist = page.getByRole("button", { name: "4 of 8" });
  await checklist.click();
  await page.locator("label").filter({ hasText: /^icebox$/ }).getByRole("checkbox").click();
  await expect(column(page, "icebox")).toHaveCount(0);
  await expect(page.getByText("team · read-only")).toBeVisible();
  await page.waitForTimeout(SETTLED);
  await page.reload();
  await expect(column(page, "triage")).toBeVisible();
  for (const hidden of ["in_review", "icebox", "done"]) await expect(column(page, hidden)).toHaveCount(0);
  await page.getByRole("button", { name: "Revert" }).click();
  for (const back of ["in_review", "icebox"]) await expect(column(page, back)).toBeVisible();
  await expect(column(page, "done")).toHaveCount(0);
  await expect(page.getByText("team · read-only")).toHaveCount(0);
});

test("a card's chip narrows the workspace", async ({ page }) => {
  await page.goto("/");
  await open(page, "Triage desk");
  await page.locator('button[title^="repo:service-arb/"]:visible').first().click();
  await expect(search(page)).toHaveValue(/ repo:service-arb\/\w+$/);
});

test("showing is not editing: reopened on a shorter window, nothing is modified", async ({ page }) => {
  await page.goto("/");
  await expect(view(page, "Release checklist")).toBeVisible();
  const cell = (id: number, tab: string, x: number, w: number) => ({ group: { id, tabs: [tab], active: 0 }, x, y: 0, w, h: 60, desired_w: null, desired_x: null, desired_h: null, rank: id });
  const layout = (...cells: unknown[]) => JSON.stringify({ version: 5, grid: { cells, floats: [], state: "Settled", next_group_id: cells.length } });
  const columns = layout(cell(0, "col:draft", 0, 9), cell(1, "col:review", 9, 20)); // two of its groups, too tall: the rest are seeded
  const arrangement = { Sm: columns, Md: columns, Xl: columns };
  const { views, workspaces } = await library(page);
  await edit(page, { Keep: { View: { ...views["Release checklist"], arrangement } } });
  const tall = layout(cell(0, "view:Release checklist", 0, 42), cell(1, "sprint:ev-main/devops/3", 42, 22));
  const kept = { ...workspaces["Release week"], tiles: { ...workspaces["Release week"].tiles, Xl: tall } };
  await edit(page, { Keep: { Workspace: { name: "Release week", workspace: kept } } });

  await page.setViewportSize({ width: 1300, height: 620 });
  await page.reload();
  await expect(column(page, "draft")).toBeVisible();
  await page.waitForTimeout(SETTLED);
  await expect.poll(() => fits(page)).toBeLessThanOrEqual(1);
  await page.setViewportSize({ width: 1440, height: 560 });
  await page.waitForTimeout(SETTLED);
  await expect.poll(() => fits(page)).toBeLessThanOrEqual(1);
  await expect(page.getByText("modified")).toHaveCount(0);
  const after = await library(page);
  expect(after.drafts).toEqual({ views: {}, workspaces: {} });
  expect(after.workspaces["Release week"]).toEqual(kept);

  await column(page, "review").hover(); // a drag is an edit
  await page.mouse.down();
  const to = (await column(page, "draft").boundingBox())!;
  await page.mouse.move(to.x + 20, to.y + 10, { steps: 10 });
  await page.mouse.move(to.x + 10, to.y + 10, { steps: 5 });
  await page.mouse.up();
  await expect(page.getByText("modified")).toBeVisible();
  await page.waitForTimeout(SETTLED);
  expect(Object.keys((await library(page)).drafts.views)).toEqual(["Release checklist"]);
});

test("what is edited right before leaving is kept, and nothing asks", async ({ page, context }) => {
  for (const leave of [(p: Page) => p.goto("about:blank"), (p: Page) => p.close({ runBeforeUnload: true })]) {
    await page.goto("/");
    await open(page, "Triage desk");
    await page.waitForTimeout(SETTLED);
    page.on("dialog", (asks) => expect(asks.type(), "asked on leaving").toBe(""));
    await page.route("**/api/library/edit", (route) => route.abort()); // what settles is too late
    const asked = `is:open i:>=${await search(page).inputValue() === "is:open i:>=3" ? 4 : 3}`;
    await column(page, "icebox").getByTitle("Close tab").click();
    await search(page).fill(asked);
    await leave(page);
    await expect.poll(async () => (await (await context.request.get("/api/library")).json()).workspaces["Triage desk"].lens.search).toBe(asked);
    expect((await (await context.request.get("/api/library")).json()).drafts.views.Triage.hidden).toContain("icebox");
    const back = await context.request.post("/api/library/edit", { data: { edit: { Revert: { View: "Triage" } } } });
    expect(back.ok(), await back.text()).toBe(true);
  }
});
