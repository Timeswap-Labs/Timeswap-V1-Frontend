import rinkeby from "../../whitelist/rinkeby.json";

import { WhiteList } from "./whitelist";
import { getProvider } from "./provider";
import { pool } from "./pool";
import { lend, lendSigner } from "./lend";
import { GlobalParams } from "./global";
import { balancesInit } from "./balances";
import { lendPositionsInit, borrowPositionsInit } from "./positions";
import { borrow, borrowSigner } from "./borrow";
import { pay, paySigner } from "./pay";
import { withdrawSigner } from "./withdraw";
import { faucetSigner } from "./faucet";
// import { pending } from "./pending";
import { wallet } from "./wallet";

export declare let window: any;

export async function elmUser(): Promise<{
  gp: GlobalParams;
  user?: { wallet: string; chainId: number; address: string };
}> {
  const gp = new GlobalParams();

  if (window.ethereum && ethereum) {
    gp.metamaskProvider = window.ethereum;
    gp.network = await gp.metamaskProvider.send("eth_chainId", []);

    if (gp.network !== "0x4") {
      gp.metamaskProvider.send("wallet_switchEthereumChain", [
        { chain: "0x4" },
      ]);
      return { gp };
    }

    const accounts: string[] = await gp.metamaskProvider.send(
      "eth_accounts",
      []
    );

    if (accounts[0]) {
      const wallet = "metamask";
      const chainId = 4;
      return { gp, user: { wallet, chainId, address: accounts[0] } };
    }
  }

  return { gp };
}

export async function init(app: ElmApp<Ports>, gp: GlobalParams) {
  const provider = await getProvider(gp);
  const network = await provider.getNetwork();
  gp.provider = provider;

  const whitelist = new WhiteList(rinkeby, gp.provider, network);

  // pool(app, whitelist, gp);

  // lend(app, whitelist);
  // lendSigner(app, whitelist, gp);

  // withdrawSigner(app, whitelist, gp);

  // borrow(app, whitelist);
  // borrowSigner(app, whitelist, gp);

  // pay(app);
  // paySigner(app, whitelist, gp);

  // faucetSigner(app, gp);

  portsInit(app, whitelist, gp);
  metamaskConnected(app, whitelist, gp);
  metamaskChainChange(app, whitelist, gp);
  metamaskAccountsChange(app, whitelist, gp);
}

function portsInit(app: ElmApp<Ports>, whitelist: WhiteList, gp: GlobalParams) {
  app.ports.connect.subscribe((walletName) => {
    console.log("Wrapper", walletName);
    console.log("Wrapper", wallet);
    gp.metamaskProvider = wallet[walletName];
    receiveUser(app, whitelist, gp, walletName);
    // if (wallet == "metamask" && window.ethereum && ethereum) {
    //   gp.metamaskProvider = window.ethereum;
    //   receiveUser(app, whitelist, gp);
    // } else {
    //   app.ports.noMetamask.send();
    // }
  });

  // app.ports.disconnect.subscribe(() => {
  //   app.ports.receiveUser.send(null);
  // });

  window.addEventListener("scroll", () => {
    console.log("scrolling");
    app.ports.scroll.send;
  });
}

function receiveUser(
  app: ElmApp<Ports>,
  whitelist: WhiteList,
  gp: GlobalParams,
  walletName: string
) {
  gp.metamaskProvider
    .send("eth_requestAccounts", [])
    .then((accounts: string[]) => {
      console.log("Wrapper", accounts);
      app.ports.receiveUser.send({
        chainId: Number(ethereum.chainId),
        wallet: walletName,
        address: accounts[0],
        txns: {
          confirmed: [],
          uncomfirmed: [],
        },
      });
      gp.metamaskSigner = gp.metamaskProvider.getSigner();

      userInit(app, whitelist, gp, accounts[0]);
    })
    .catch(() => {
      console.log("Wrapper", "Failed");
      app.ports.receiveNoConnect.send(walletName);
    });
}

function metamaskConnected(
  app: ElmApp<Ports>,
  whitelist: WhiteList,
  gp: GlobalParams
) {
  gp.metamaskProvider.send("eth_accounts", []).then((accounts: string[]) => {
    if (accounts[0]) {
      app.ports.receiveUser.send({
        chainId: Number(ethereum.chainId),
        wallet: "metamask",
        address: accounts[0],
        txns: {
          confirmed: [],
          uncomfirmed: [],
        },
      });
      gp.metamaskSigner = gp.metamaskProvider.getSigner();

      userInit(app, whitelist, gp, accounts[0]);
    }
  });
}

function metamaskChainChange(
  app: ElmApp<Ports>,
  whitelist: WhiteList,
  gp: GlobalParams
) {
  ethereum.on("chainChanged", (chainId: string) => {
    gp.metamaskProvider!.send("eth_accounts", []).then((accounts: string[]) => {
      if (accounts[0]) {
        app.ports.receiveUser.send({
          chainId: Number(chainId),
          wallet: "metamask",
          address: accounts[0],
          txns: {
            confirmed: [],
            uncomfirmed: [],
          },
        });

        gp.provider.removeAllListeners();
        gp.metamaskProvider.removeAllListeners();

        gp.metamaskProvider = window.ethereum;
        gp.metamaskSigner = gp.metamaskProvider.getSigner();

        chainInit(app, whitelist, gp, accounts[0]);
      }
    });
  });
}

function metamaskAccountsChange(
  app: ElmApp<Ports>,
  whitelist: WhiteList,
  gp: GlobalParams
) {
  ethereum.on("accountsChanged", (accounts: string[]) => {
    if (accounts[0]) {
      app.ports.receiveUser.send({
        chainId: Number(ethereum.chainId),
        wallet: "metamask",
        address: accounts[0],
        txns: {
          confirmed: [],
          uncomfirmed: [],
        },
      });

      gp.metamaskProvider.removeAllListeners();

      gp.metamaskProvider = window.ethereum;
      gp.metamaskSigner = gp.metamaskProvider.getSigner();

      userInit(app, whitelist, gp, accounts[0]);
    } else {
      app.ports.receiveUser.send(null);
    }
  });
}

async function chainInit(
  app: ElmApp<Ports>,
  whitelist: WhiteList,
  gp: GlobalParams,
  account: string
) {
  gp.provider = await getProvider(gp);
  gp.network = await gp.metamaskProvider.send("eth_chainId", []);
  userInit(app, whitelist, gp, account);
}

function userInit(
  app: ElmApp<Ports>,
  whitelist: WhiteList,
  gp: GlobalParams,
  account: string
) {
  if (gp.network === "0x4") {
    // balancesInit(app, whitelist, gp, account);
    // lendPositionsInit(app, whitelist, gp, account);
    // borrowPositionsInit(app, whitelist, gp, account);
    // pending(gp, account);
  }
}
