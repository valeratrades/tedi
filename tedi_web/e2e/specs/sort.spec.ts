import { expect, type Page, test } from "@playwright/test";

test.skip(({ viewport }) => viewport!.width < 768, "the order is in a popover");

const strip = (page: Page) => page.getByRole("group", { name: "Sort", exact: true });
const labels = (page: Page) => strip(page).locator("[draggable=true]");
const add = async (page: Page, ...keys: string[]) => {
  for (const key of keys) {
    await strip(page).getByRole("button", { name: "Add a sort key" }).click();
    await strip(page).getByRole("button", { name: key, exact: true }).click();
  }
};
const triage = async (page: Page) => {
  await page.goto("/");
  await page.locator("aside").getByRole("link", { name: "Triage desk", exact: true }).click();
  await expect(labels(page)).toHaveText(["importance ↓"]);
};

test("an order holds every key there is", async ({ page }) => {
  await triage(page);
  await add(page, "created", "hours", "updated", "state:kanban", "state:pull_requests", "number", "rank");
  await expect(labels(page)).toHaveText(["importance ↓", "created ↓", "hours ↑", "updated ↓", "state:kanban ↑", "state:pull_requests ↑", "number ↑", "rank ↓"]);
  await expect(strip(page).getByRole("button", { name: "Add a sort key" })).toHaveCount(0);
});

test("a click reverses a label, a drag moves it, and both are there on reload", async ({ page }) => {
  await triage(page);
  await add(page, "created", "hours");
  const card = page.locator('a[href*="issue="]');
  const shown = () => card.evaluateAll((all) => all.map((a) => a.getAttribute("href")));

  await labels(page).nth(1).dragTo(labels(page).nth(0));
  await expect(labels(page)).toHaveText(["created ↓", "importance ↓", "hours ↑"]);
  const newest = await shown();
  await labels(page).nth(0).getByRole("button", { name: "created: newest first" }).click();
  await expect(labels(page)).toHaveText(["created ↑", "importance ↓", "hours ↑"]);
  await expect.poll(shown).not.toEqual(newest);

  await labels(page).nth(0).dragTo(labels(page).nth(2));
  await expect(labels(page)).toHaveText(["importance ↓", "hours ↑", "created ↑"]);
  await labels(page).nth(2).getByRole("button", { name: "created: oldest first" }).press("ArrowLeft");
  await expect(labels(page)).toHaveText(["importance ↓", "created ↑", "hours ↑"]);

  await page.waitForTimeout(1500); // the order is saved as it settles
  await page.reload();
  await expect(labels(page)).toHaveText(["importance ↓", "created ↑", "hours ↑"]);
});

for (const path of [["ev-main"], ["ev-main", "service-arb"]])
  test(`a blocker from an earlier step, or from outside ${path.at(-1)}, is above what it blocks, edged red`, async ({ page }) => {
    await triage(page);
    for (const at of path) {
      await page.getByRole("button", { name: "Start from" }).click();
      await page.getByRole("button", { name: new RegExp(`^${at}`) }).last().click();
    }
    await expect(page.getByRole("navigation", { name: "Scope" })).toHaveText(path.join("/"));
    const card = (title: string) => page.locator("[draggable=true]").filter({ has: page.getByRole("link", { name: title, exact: true }) });
    const titles = () => page.locator("[draggable=true] > div").locator('a.font-medium[href*="issue="]').allTextContents();
    const [blocker, blocked] = ["rotate staging certs", "Epic: a video verification Google accepts, submitted from the PC"]; // ev-main's in triage, service-arb's in selected
    const red = await card(blocked).getByText("blocked by").evaluate((e) => getComputedStyle(e).color);

    for (const key of [null, "created"]) {
      if (key) await add(page, key);
      await expect.poll(async () => (await titles()).indexOf(blocked) - (await titles()).indexOf(blocker)).toBe(1);
      await expect(card(blocker)).toHaveCount(1);
      await expect(card(blocker).locator("> div")).toHaveCSS("border-top-color", red);
      await expect(card(blocked).locator("> div")).not.toHaveCSS("border-top-color", red);
    }
  });

test("a label is dropped, but never the last; what is added goes ahead of a closing rank", async ({ page }) => {
  await triage(page);
  await add(page, "rank", "created");
  await expect(labels(page)).toHaveText(["importance ↓", "created ↓", "rank ↓"]);
  for (const key of ["importance", "rank"]) await strip(page).getByRole("button", { name: `Drop ${key}` }).click();
  await expect(labels(page)).toHaveText(["created ↓"]);
  await expect(strip(page).getByRole("button", { name: /^Drop/ })).toHaveCount(0);
});
