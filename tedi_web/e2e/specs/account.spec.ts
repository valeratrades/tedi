import { expect, type Page, test } from "@playwright/test";

test.skip(({ viewport }) => viewport!.width < 768, "the shelves are behind a sheet");

const panel = (page: Page) => page.getByRole("dialog", { name: "Account" });
const row = (page: Page, text: string) => panel(page).locator("div.h-7", { hasText: text });

test("a viewer brings keys and scopes, and drops them", async ({ page }) => {
  await page.goto("/");
  await page.locator("aside").getByRole("button", { name: "Account" }).click();
  const p = panel(page);

  await p.getByLabel("Key's owner").fill("ann");
  await p.getByLabel("Token").fill("nonsense");
  await p.getByRole("button", { name: "Add key" }).click();
  await expect(p.getByRole("alert")).toContainText("GitHub takes no such token");

  await p.getByLabel("Token").fill("tok-ann");
  await p.getByRole("button", { name: "Add key" }).click();
  await expect(row(page, "as ann")).toBeVisible();
  await expect(p.getByRole("alert")).toHaveCount(0);
  await expect(p.getByLabel("Token")).toHaveValue("");

  await p.getByLabel("Scope", { exact: true }).fill("ann");
  await p.getByRole("button", { name: "Add scope" }).click();
  await expect(row(page, "1 repo")).toContainText("pulled", { timeout: 20_000 });

  await p.getByRole("button", { name: "Close" }).click();
  await expect(page.locator("aside").getByRole("button", { name: "Account" })).not.toContainText("Bring your keys");
  await page.keyboard.press("Control+k");
  await page.keyboard.type("Private note");
  await expect(page.getByText("#1 Private note")).toBeVisible();
  await page.keyboard.press("Escape");

  await page.locator("aside").getByRole("button", { name: "Account" }).click();
  await p.getByRole("button", { name: "Drop the scope ann" }).click();
  await p.getByRole("button", { name: "Drop the key on ann" }).click();
  await expect(p.locator("div.h-7")).toHaveCount(0);
});
