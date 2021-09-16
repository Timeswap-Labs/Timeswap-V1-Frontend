import { Elm } from "../Main.elm";
import rinkeby from "../../whitelist/rinkeby.json";

import images from "../../image/*.svg";
import tokenImages from "../../image/tokens/*.svg";

import { Web3Provider } from "@ethersproject/providers";

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
  },
});

init(app);
