import { Elm } from "../Main.elm";
import rinkeby from "../../whitelist/rinkeby.json";

import * as images from "url:../../image/*.svg";
import * as tokenImages from "url:../../image/tokens/*.svg";

import { init } from "./init";

export declare let window: any;

const app = Elm.Main.init({
  node: document.getElementById("elm")!,
  flags: {
    width: window.innerWidth,
    time: Date.now(),
    hasBackdropSupport:
      CSS.supports("-webkit-backdrop-filter: none") ||
      CSS.supports("backdrop-filter: none"),
    images: Object.entries(images),
    tokenImages: Object.entries(tokenImages),
    whitelist: rinkeby,
    user : null
  },
});

init(app);
