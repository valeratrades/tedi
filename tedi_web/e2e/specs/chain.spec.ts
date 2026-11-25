import { expect, type Page, test } from "@playwright/test";

const SETTLED = 1500; // past the autosave
const node = (page: Page, name: string) => page.getByRole("region", { name, exact: true });
const tabs = (page: Page, name: string) => node(page, name).getByRole("tablist").first().getByRole("tab");
const leaf = (page: Page, at: string) => page.locator(`.tl-tab[data-panel="leaf:https://github.com/ev-estate/${at}"]`);
const edit = async (page: Page, edit: unknown) => {
  const kept = await page.request.post("/api/library/edit", { data: { edit } });
  expect(kept.ok(), await kept.text()).toBe(true);
};
/** The board in a workspace of one view by the chain, scoped to `scope`. */
const nested = async (page: Page, scope: string, search = "is:open") => {
  await page.goto("/");
  await edit(page, { Keep: { View: { name: "Nested", group_by: "chain", shown: ["repo", "number", "importance"] } } });
  const cell = { group: { id: 0, tabs: ["view:Nested"], active: 0 }, x: 0, y: 0, w: 64, h: 20, desired_w: null, desired_x: null, desired_h: null, rank: 0 };
  const layout = JSON.stringify({ version: 5, grid: { cells: [cell], floats: [], state: "Settled", next_group_id: 1 } });
  const workspace = { tiles: { Sm: layout, Md: layout, Xl: layout }, lens: { scope, search, sort: "importance" } };
  await edit(page, { Keep: { Workspace: { name: "nest", workspace } } });
  await edit(page, { Open: "nest" });
  await page.goto("/");
};
const MILESTONE = "https://github.com/ev-estate/site/milestone/1";

test("a point nests all the way down, a tab's inside made as it is first opened and kept", async ({ page }) => {
  await nested(page, MILESTONE);
  await expect(node(page, "v1.0")).toContainText("A buyer finds a flat and books a viewing.");
  await expect(node(page, "v1.0")).toContainText("due 2026-12-15");
  await expect(tabs(page, "v1.0")).toHaveText(["deeds · stable", "#4 booking a viewing"]);
  await expect(leaf(page, "site/issues/3")).toBeVisible();
  await expect(leaf(page, "site/issues/8")).toBeVisible();
  await expect(leaf(page, "site/issues/5")).toHaveCount(0); // the epic's
  await expect(node(page, "stable")).toBeVisible();
  await expect(leaf(page, "deeds/issues/1")).toBeVisible();
  await expect(node(page, "#4 booking a viewing")).toHaveCount(0);

  await tabs(page, "v1.0").nth(1).click();
  await expect(leaf(page, "site/issues/5")).toBeVisible();
  await expect(leaf(page, "site/issues/7")).toBeVisible(); // two epics down
  await expect(node(page, "stable")).toBeHidden();
  await leaf(page, "site/issues/7").evaluate((tab) => ((tab as HTMLElement).dataset.same = "yes"));
  await tabs(page, "v1.0").nth(0).click();
  await expect(leaf(page, "site/issues/7")).toBeHidden();
  await tabs(page, "v1.0").nth(1).click();
  await expect(leaf(page, "site/issues/7")).toHaveAttribute("data-same", "yes");
});

test("a loop GitHub holds is told, not followed", async ({ page }) => {
  await nested(page, MILESTONE);
  await tabs(page, "stable").filter({ hasText: "site · v1.0" }).click();
  await expect(page.getByText("It is in itself")).toBeVisible();
});

test("terms are no point to nest from", async ({ page }) => {
  await nested(page, "repo:ev-estate/*");
  await expect(page.getByText("No point to nest from")).toBeVisible();
});

test("a repo holds its milestones, and the issues outside them", async ({ page }) => {
  await nested(page, "https://github.com/ev-estate");
  await expect(tabs(page, "ev-estate")).toHaveText(["deeds", "site"]);
  await expect(tabs(page, "deeds")).toHaveText(["stable"]);
});

test("the search narrows the leaves, never what holds them", async ({ page }) => {
  await nested(page, MILESTONE, "is:open i:5");
  await expect(leaf(page, "site/issues/3")).toBeVisible();
  await expect(leaf(page, "site/issues/8")).toHaveCount(0);
  await expect(tabs(page, "v1.0")).toHaveText(["deeds · stable", "#4 booking a viewing"]);
});

test("what doesn't fit runs off the tile, which scrolls to it", async ({ page, viewport }) => {
  test.skip(viewport!.width < 768, "stacked");
  await page.setViewportSize({ width: 1060, height: 420 }); // narrower is a phone's: stacked
  await nested(page, MILESTONE);
  await tabs(page, "v1.0").nth(1).click();
  await expect(leaf(page, "site/issues/7")).toBeAttached();
  const over = await node(page, "v1.0").evaluate((root) => {
    const tile = root.parentElement!;
    return { right: tile.scrollWidth - tile.clientWidth, bottom: tile.scrollHeight - tile.clientHeight };
  });
  expect(over.right).toBeGreaterThan(0);
  expect(over.bottom).toBeGreaterThan(0);
  await leaf(page, "site/issues/7").scrollIntoViewIfNeeded();
  await expect(leaf(page, "site/issues/7")).toBeInViewport();
});

test("a node's leaves stay as arranged; a closed one comes back, as a tree misses none", async ({ page, viewport }) => {
  test.skip(viewport!.width < 768, "stacked");
  await nested(page, MILESTONE);
  const order = () => page.locator('.tl-tab[data-panel^="leaf:https://github.com/ev-estate/site/issues/"]').evaluateAll((tabs) =>
    tabs.map((t) => ({ at: (t as HTMLElement).dataset.panel!.split("/").pop(), y: t.getBoundingClientRect().y })).sort((a, b) => a.y - b.y).map((t) => t.at));
  await expect.poll(order).toEqual(["3", "8"]); // by importance
  await page.waitForTimeout(SETTLED);
  const untouched = await (await page.request.get("/api/library")).json();
  expect(untouched.drafts.views.Nested).toBeUndefined(); // showing is not editing
  await leaf(page, "site/issues/3").getByTitle("Close tab").click();
  await expect.poll(order).toEqual(["8", "3"]);
  await page.waitForTimeout(SETTLED);
  const held = await (await page.request.get("/api/library")).json();
  expect(Object.keys(held.drafts.views.Nested.nodes)).toEqual([MILESTONE]);
  await page.reload();
  await expect.poll(order).toEqual(["8", "3"]);

  const [from, onto] = [await leaf(page, "site/issues/8").boundingBox(), await leaf(page, "site/issues/3").boundingBox()];
  await page.mouse.move(from!.x + 130, from!.y + 14); // its header, past the tab
  await page.mouse.down();
  await page.mouse.move(onto!.x + 130, onto!.y + 120, { steps: 8 }); // its body: lands below it
  await page.mouse.up();
  await expect.poll(order).toEqual(["3", "8"]);
  await page.waitForTimeout(SETTLED);
  await page.reload();
  await expect.poll(order).toEqual(["3", "8"]);
});
