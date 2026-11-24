import { defineConfig, devices } from "@playwright/test";

export const PORT = 7361; // the proxy's (stack.ts); the server's the next one

// Run through the flake, which supplies `@playwright/test` with its pinned browsers.
/** `still`: the store's change stream ends at once, so the network goes idle. */
export const config = (testMatch: string, still: boolean) =>
  defineConfig({
    testDir: ".",
    testMatch,
    fullyParallel: true,
    outputDir: "../out/playwright",
    reporter: [["list"]],
    globalSetup: "./stack.ts",
    metadata: { still },
    use: { baseURL: `http://127.0.0.1:${PORT}`, deviceScaleFactor: 1 },
    projects: [
      { name: "desktop", use: { ...devices["Desktop Chrome"], viewport: { width: 1440, height: 900 } } },
      { name: "mobile", use: { ...devices["Desktop Chrome"], viewport: { width: 390, height: 844 } } },
    ],
  });

export default config("{sort,url}.spec.ts", false);
