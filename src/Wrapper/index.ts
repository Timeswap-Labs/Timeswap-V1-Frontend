import { Elm } from "../Main.elm";
import rinkeby from "../../whitelist/rinkeby.json";

import * as images from "url:../../image/*.svg";
import * as tokenImages from "url:../../image/tokens/*.svg";
import * as chainImages from "url:../../image/chains/*.svg";
import * as walletImages from "url:../../image/wallets/*.svg";

import { elmUser, init } from "./init";

export declare let window: any;

async function elmInit() {
  const { gp, user } = await elmUser();

  const app = Elm.Main.init({
    node: document.getElementById("elm")!,
    flags: {
      time: Date.now(),
      offset: new Date().getTimezoneOffset(),
      zoneName: Intl.DateTimeFormat().resolvedOptions().timeZone,
      chosenZone: null,
      width: window.innerWidth,
      hasBackdropSupport:
        CSS.supports("-webkit-backdrop-filter: none") ||
        CSS.supports("backdrop-filter: none"),
      theme: null,
      images: Object.entries(images),
      tokenImages: Object.entries(tokenImages),
      chainImages: Object.entries(chainImages),
      walletImages: Object.entries(walletImages),
      slippage: null,
      deadline: null,
      spot: null,
      wallets: ["metamask"],
      chains: {
        default: {
          chainId: 0x4,
          name: "Rinkeby",
          etherscan: "https://rinkeby.etherscan.io",
          native: {
            name: "Ether",
            symbol: "ETH",
            decimals: 18,
          },
          whitelist: [
            {
              address: "0xb83a6d7f5dc224e241989511ea3e2b7f4f263ede",
              name: "DAI TEST TOKEN",
              symbol: "DAI",
              decimals: 18,
            },
            {
              address: "0xec23daeab1deeb3587eeb3453d4e95db128b0e62",
              name: "Matic TEST TOKEN",
              symbol: "MATIC",
              decimals: 18,
            },
          ],
          custom: [
            {
              "address": "0xa0b86991c6218b36c1d19d4a2e9eb0ce3606eb48",
              "name": "USDC TEST TOKEN with a long name",
              "symbol": "USDC",
              "decimals": 18
            }
          ]
        },
        others: [],
      },
      user: null,
    },
  });

  init(app, gp);
}

elmInit();
