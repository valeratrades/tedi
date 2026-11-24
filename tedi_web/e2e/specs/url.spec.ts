import { expect, type Page, test } from "@playwright/test";

test.skip(({ viewport }) => viewport!.width < 768, "the shelves are behind a sheet");

const at = (page: Page) => page.evaluate(() => location.pathname + location.search);
const shelf = (page: Page, name: string) => page.locator("aside").getByRole("link", { name, exact: true });
const tab = (page: Page, name: string) => page.locator(".tl-tab", { hasText: name });
const ISSUE = /[?&]issue=service-arb\/[\w.-]+\/\d+/;
const card = (page: Page) => page.locator('a[href*="issue=service-arb/"]').first();

test("a bare address is the screen as left, then names it", async ({ page }) => {
  await page.goto("/");
  await expect.poll(() => at(page)).toMatch(/^\/view\/\w{8}\?ws=\w{8}&scope=EV%20%2F%20EV-Service-Arb$/);
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
  await shelf(page, "EV-Main").click();
  await expect.poll(async () => new URL(page.url()).searchParams.get("scope")).toBe("EV / EV-Main");
  await page.goBack();
  await expect.poll(async () => new URL(page.url()).searchParams.get("scope")).toBe("EV / EV-Service-Arb");
  await page.goBack();
  await expect.poll(() => at(page)).toBe(before);
});

test("an address nothing has says so", async ({ page }) => {
  await page.goto("/view/nowhere1");
  await expect(page.getByText("no view nowhere1 here")).toBeVisible();
});
