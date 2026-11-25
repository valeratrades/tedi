import { test } from "@playwright/test";
import { defineFigmaSuite } from "@evinvest/figma-render";

const SHELVES = 'aside a[href*="/view/"] >> nth=0'; // the first view: setup and the library arrived
const LEFT = "ws=RelsWeek"; // where the seeded viewer left off (stack.ts)
const at = (path: string, query = "") => ({ name: path, url: `${path}?${query}${LEFT}`, selector: SHELVES });

// Every view, column layout, sprint and library page in the workspace the viewer left, and every workspace: what a link
// changes of the rest is the same screen over other cards.
defineFigmaSuite(
  { test },
  [
    { name: "as left", url: "/", selector: SHELVES },
    at("/view/RelsList", "issue=service-arb/review_archive/46&"),
    at("/builder"),
    ...["workspaces", "views", "scopes", "columns"].map((kind) => at(`/library/${kind}`)),
  ],
  {
    hide: ["#__dx-toast"], // the debug bundle's
    exclude: [/[?&]issue=/, new RegExp(`^/.+\\?(?!${LEFT}$)`), /^\/\?(?!ws=\w+$)/],
  },
);
