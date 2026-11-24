import { expect, type Page, test } from "@playwright/test";

const bar = (page: Page) => page.getByRole("progressbar", { name: "Loading" });
const card = (page: Page) => page.locator('a[href*="issue=service-arb/"]').first();

/** What `url` names answers only once let through. */
const hold = async (page: Page, url: RegExp) => {
  let release!: () => void;
  const held = new Promise<void>((on) => (release = on));
  await page.route(url, async (route) => {
    await held;
    await route.continue();
  });
  return release;
};
const shown = async (page: Page) => {
  await expect(card(page)).toBeVisible();
  await expect(bar(page)).toHaveCount(0);
};

test("the board's wait is in the middle of the screen, from before the client runs", async ({ page }) => {
  const client = await hold(page, /\.wasm$/);
  await page.goto("/");
  await expect(bar(page)).toBeVisible();
  const { x, y, width, height } = (await page.getByTestId("loading").boundingBox())!;
  const { width: w, height: h } = page.viewportSize()!;
  expect(Math.abs(x + width / 2 - w / 2)).toBeLessThan(1);
  expect(Math.abs(y + height / 2 - h / 2)).toBeLessThan(1);
  client();
  await shown(page);
});

test("a first wait has no length to show; the next is as long as it was", async ({ page }) => {
  let release = await hold(page, /\/api\/board$/);
  await page.goto("/");
  await expect(bar(page)).not.toHaveAttribute("aria-valuenow");
  await expect(bar(page)).not.toContainText("%");
  const grips = page.locator("[class*=tl-resize]");
  await expect.poll(() => grips.count()).toBeGreaterThan(0);
  const over = (e: Element) => {
    const { x, y, width, height } = e.getBoundingClientRect();
    return e.contains(document.elementFromPoint(x + width / 2, y + height / 2));
  };
  expect(await grips.evaluateAll((all, over) => all.filter(eval(over)).length, over.toString()), "the dock is under it, its grips included").toBe(0);
  await page.waitForTimeout(1500);
  release();
  await shown(page);

  await page.unroute(/\/api\/board$/);
  release = await hold(page, /\/api\/board$/);
  await page.reload();
  await expect(bar(page)).toContainText("%");
  const at = async () => Number(await bar(page).getAttribute("aria-valuenow"));
  const first = await at();
  await expect.poll(at).toBeGreaterThan(first);
  await page.waitForTimeout(4000); // past the first one's
  expect(await at(), "not done").toBeLessThan(100);
  release();
  await shown(page);
});

test("the theme's mark is over the bar", async ({ page }) => {
  const svg = encodeURIComponent('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 2 1"><path d="M0 0h2v1H0z"/></svg>');
  await page.route(/\/assets\/.*\.css$/, async (route) => {
    const sheet = await route.fetch();
    await route.fulfill({ response: sheet, body: `${await sheet.text()} :root { --brand-mark: url("data:image/svg+xml,${svg}"); --brand-aspect: 2 / 1 }` });
  });
  const release = await hold(page, /\/api\/board$/);
  await page.goto("/");
  const mark = page.getByTestId("loading").locator('[data-slot="logo"]');
  await expect(mark).toBeVisible();
  const [m, b] = [(await mark.boundingBox())!, (await bar(page).boundingBox())!];
  expect(m.y + m.height).toBeLessThan(b.y);
  expect(m.width).toBeCloseTo(2 * m.height);
  release();
  await shown(page);
});

test("a wait that fails says why", async ({ page }) => {
  await page.route(/\/api\/board$/, (route) => route.fulfill({ status: 500, json: { ServerError: { message: "the store is gone", code: 500 } } }));
  await page.goto("/");
  await expect(page.getByText("The board didn't load")).toBeVisible();
  await expect(bar(page)).toHaveCount(0);
});

test("a client that doesn't start says so", async ({ page }) => {
  await page.route(/\.wasm$/, (route) => route.abort());
  await page.goto("/");
  await expect(page.getByRole("alert")).toBeVisible();
});
