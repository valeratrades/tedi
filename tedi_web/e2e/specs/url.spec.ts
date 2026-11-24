import { expect, type Page, test } from "@playwright/test";

test.skip(({ viewport }) => viewport!.width < 768, "the shelves are behind a sheet");

const at = (page: Page) => page.evaluate(() => location.pathname + location.search);
const shelf = (page: Page, name: string) => page.locator("aside").getByRole("link", { name, exact: true });
const tab = (page: Page, name: string) => page.locator(`.tl-tab[data-panel="view:${name}"]`);
const crumb = (page: Page, name: string) => page.locator("div:has(> div button[aria-haspopup])").getByRole("link", { name, exact: true });
const ISSUE = /[?&]issue=service-arb\/[\w.-]+\/\d+/;
const card = (page: Page) => page.locator('a[href*="issue=service-arb/"]').first();

test("a bare address is the screen as left, then names it", async ({ page }) => {
  await page.goto("/");
  await expect.poll(() => at(page)).toMatch(/^\/view\/\w{8}\?ws=\w{8}&scope=https%3A%2F%2Fgithub\.com%2Fservice-arb$/);
  await expect(tab(page, "Release checklist")).toBeVisible();
});

test("a view has an address: its link, Back, and a tab of its own", async ({ page, context }) => {
  await page.goto("/");
  await expect.poll(() => at(page)).toMatch(/^\/view\//);
  const first = await at(page);
  await shelf(page, "Triage").click();
  await expect.poll(() => at(page)).not.toBe(first);
  const triage = await at(page);
  expect(triage.split("?")[0]).not.toBe(first.split("?")[0]);
  await page.goBack();
  await expect.poll(() => at(page)).toBe(first);
  await page.goForward();
  await expect.poll(() => at(page)).toBe(triage);

  const [own] = await Promise.all([context.waitForEvent("page"), shelf(page, "Triage").click({ modifiers: ["ControlOrMeta"] })]);
  await expect(tab(own, "Triage")).toBeVisible();
  expect((await at(own)).split("?")[0]).toBe(triage.split("?")[0]);
});

test("a picked issue is in the address, over the view it was picked from", async ({ page }) => {
  await page.goto("/");
  await expect.poll(() => at(page)).toMatch(/^\/view\//);
  const before = await at(page);
  await card(page).click();
  await expect.poll(() => at(page)).toMatch(ISSUE);
  const with_issue = await at(page);
  expect(with_issue.split("?")[0]).toBe(before.split("?")[0]);
  const float = page.locator(".tl-tab", { hasText: /#\d+ / });
  await expect(float).toHaveCount(1);

  await page.reload();
  await expect(float).toHaveCount(1);
  expect(await at(page)).toBe(with_issue);

  await page.goBack();
  await expect.poll(() => at(page)).toBe(before);
  await expect(float).toHaveCount(0);

  await page.waitForTimeout(1200); // past the autosave
  await page.goto("/");
  await expect.poll(() => at(page)).toBe(before);
  await expect(float, "what the address held is not what the screen was left as").toHaveCount(0);
});

test("a column layout is under its view", async ({ page }) => {
  await page.goto("/");
  await shelf(page, "Triage").click();
  await expect.poll(() => at(page)).toMatch(/^\/view\/\w{8}\?/);
  const view = (await at(page)).split("?")[0];
  await shelf(page, "triage focus").click();
  await expect.poll(async () => (await at(page)).split("?")[0]).toMatch(new RegExp(`^${view}/\\w{8}$`));
  const focus = await at(page);
  const dot = page.locator("span", { hasText: /^triage\d*$/ }).locator("span").first();
  await expect(dot).toHaveAttribute("style", /--tint-orange/);

  await shelf(page, "kanban · notion tints").click();
  await expect.poll(() => at(page)).not.toBe(focus);
  await page.goBack();
  await expect.poll(() => at(page)).toBe(focus);

  await page.reload();
  await expect(dot).toHaveAttribute("style", /--tint-orange/);
  expect(await at(page)).toBe(focus);
});

test("a workspace and a scope are in the query", async ({ page }) => {
  await page.goto("/");
  await expect.poll(() => at(page)).toMatch(/ws=/);
  const before = await at(page);
  await shelf(page, "Triage desk").click();
  await expect.poll(async () => new URL(page.url()).searchParams.get("ws")).not.toBe(new URL(before, page.url()).searchParams.get("ws"));
  const scope = async () => new URL(page.url()).searchParams.get("scope");
  await crumb(page, "ev-main").click();
  await expect.poll(scope).toBe("https://github.com/ev-main");
  await shelf(page, "core").click();
  await expect.poll(scope).toBe("core");
  for (const was of ["https://github.com/ev-main", "https://github.com/service-arb"]) {
    await page.goBack();
    await expect.poll(scope).toBe(was);
  }
  await page.goBack();
  await expect.poll(() => at(page)).toBe(before);
});

test("the scope bar goes down the chain, and each crumb back up it", async ({ page }) => {
  await page.goto("/");
  const scope = async () => new URL(page.url()).searchParams.get("scope");
  await expect.poll(scope).toBe("https://github.com/service-arb");
  const under = async (at: string, name: string) => {
    await page.getByRole("button", { name: at, exact: true }).click();
    await page.getByRole("link", { name: new RegExp(`^${name}\\d+$`) }).click();
  };
  await under("service-arb", "playbook");
  await expect.poll(scope).toBe("https://github.com/service-arb/playbook");
  await expect(crumb(page, "ev-main")).toBeVisible();
  await crumb(page, "service-arb").click();
  await expect.poll(scope).toBe("https://github.com/service-arb");
  await under("service-arb", "All repos");
  await expect.poll(scope).toBe(null);
  await under("All repos", "ev-main");
  await expect.poll(scope).toBe("https://github.com/ev-main");
  await under("ev-main", "core · mine");
  await expect.poll(scope).toBe("core");
});

test("a share link carries the scope, of either kind", async ({ page }) => {
  await page.goto("/");
  await expect.poll(() => at(page)).toMatch(/ws=/);
  const shared = async () => {
    const [answer] = await Promise.all([page.waitForResponse(/\/api\/share$/), page.getByRole("button", { name: "Copy a link to this screen" }).click()]);
    return new URL(await answer.json(), page.url()).searchParams.get("scope");
  };
  expect(await shared()).toBe("https://github.com/service-arb");
  await shelf(page, "core").click();
  await expect.poll(async () => new URL(page.url()).searchParams.get("scope")).toBe("core");
  expect(await shared()).toBe("core");
});

test("a point nothing of which is on the board says so", async ({ page }) => {
  await page.goto("/?scope=https%3A%2F%2Fgithub.com%2Fnobody");
  await expect(page.getByText("nothing of it is on the board")).toBeVisible();
});

test("an address nothing has says so", async ({ page }) => {
  await page.goto("/view/nowhere1");
  await expect(page.getByText("no view nowhere1 here")).toBeVisible();
});
