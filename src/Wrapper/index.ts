import { Elm } from "../Main.elm";
import rinkeby from "../../whitelist/rinkeby.json";

import * as images from "url:../../image/*.svg";
import * as tokenImages from "url:../../image/tokens/*.svg";
import * as chainImages from "url:../../image/chains/*.svg";
import * as walletImages from "url:../../image/wallets/*.svg";

import { elmUser, init } from "./init";
import { getCustomTokens } from "./helper";

export declare let window: any;

async function elmInit() {
  const { gp, user } = await elmUser();

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
      tokenImages: Object.entries(tokenImages),
      chainImages: Object.entries(chainImages),
      walletImages: Object.entries(walletImages),
      slippage: Number(window.localStorage.getItem("slippage")) || null,
      deadline: Number(window.localStorage.getItem("deadline")) || null,
      priceFeed: window.localStorage.getItem("priceFeed"),
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
              address: "0x559a70084eb180d649080048a7e966602eacea9e",
              name: "DAI TEST TOKEN",
              symbol: "DAI",
              decimals: 18,
            },
            {
              address: "0x521c1c37fdd245c313457fcbc6d5ed156647c2f3",
              name: "Matic TEST TOKEN",
              symbol: "MATIC",
              decimals: 18,
            }
          ],
          custom: [
            {
              address: "0xa0b86991c6218b36c1d19d4a2e9eb0ce3606eb48",
              name: "USDC TEST TOKEN with a long name",
              symbol: "USDC",
              decimals: 18
            },
            {
              address: "0xdac17f958d2ee523a2206206994597c13d831ec7",
              name: "USDT",
              symbol: "USDT",
              decimals: 18,
            },
            {
              address: "0xdac17f958d2ee523a2206206994597c13d831ff9",
              name: "DDDT",
              symbol: "DDDT",
              decimals: 18,
            }
          ]
        },
        others: [
          {
            chainId: 0x3,
            name: "Ropsten",
            etherscan: "https://ropsten.etherscan.io",
            native: {
              name: "Ether",
              symbol: "ETH",
              decimals: 18,
            },
            whitelist : [],
            custom: getCustomTokens(String(0x3)),
          }
        ],
      },
      user,
    },
  });

  init(app, gp);
}

elmInit();
