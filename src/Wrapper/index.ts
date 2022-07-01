import { Elm } from "../Main.elm";

import * as images from "url:../../image/*.svg";
import * as gifs from "url:../../image/gifs/*.gif";
import * as tokenImages from "url:../../image/tokens/*.svg";
import * as chainImages from "url:../../image/chains/*.svg";
import * as walletImages from "url:../../image/wallets/*.svg";

import { elmUser, init } from "./init";
import { whitelistChains } from "./chains";
import { sentry } from "./sentry";

export declare let window: any;

async function elmInit() {
  const { gp, user } = await elmUser();

  let apiEndpoint = "";
  if (process.env.PARCEL_PUBLIC_ENVIRONMENT === "production")
    apiEndpoint = "https://ts-bico-mainnet.herokuapp.com/v1";
  else
    apiEndpoint = "https://ts-bico-mainnet.herokuapp.com/v1";

  const app = Elm.Main.init({
    node: document.getElementById("elm")!,
    flags: {
      time: Date.now(),
      offset: new Date().getTimezoneOffset(),
      zoneName: Intl.DateTimeFormat().resolvedOptions().timeZone,
      chosenZone: window.localStorage.getItem("chosen-zone"),
      width: window.innerWidth,
      hasBackdropSupport:
        CSS.supports("-webkit-backdrop-filter: none") ||
        CSS.supports("backdrop-filter: none"),
      theme: window.localStorage.getItem("theme"),
      images: Object.entries(images),
      gifs: Object.entries(gifs),
      tokenImages: Object.entries(tokenImages),
      chainImages: Object.entries(chainImages),
      walletImages: Object.entries(walletImages),
      slippage: Number(window.localStorage.getItem("slippage")) || null,
      deadline: Number(window.localStorage.getItem("deadline")) || null,
      priceFeed: window.localStorage.getItem("priceFeed"),
      wallets: window.ethereum
        ? ["metamask", "walletConnect"]
        : ["walletConnect"],
      chains: whitelistChains,
      user,
      endPoint: apiEndpoint,
    },
  });

  init(app, gp, user);

  gp.biconomy
    .onEvent(gp.biconomy.READY, () => {
      // Initialize the dapp here
      gp.isBiconomyReady = true;
    })
    .onEvent(gp.biconomy.ERROR, () => {
      // Handle error while initializing mexa
      gp.isBiconomyReady = false;
    });
}

sentry();
elmInit();
