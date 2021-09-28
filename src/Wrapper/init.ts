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

export declare let window: any;

export async function elmUser(): Promise<{
  gp: GlobalParams;
  user?: { chainId: string; user: string };
}> {
  const gp = new GlobalParams();

  if (window.ethereum && ethereum) {
    gp.metamask = window.ethereum;
    const accounts: string[] = await gp.metamask.send("eth_accounts", []);

    if (accounts[0]) {
      const chainId = ethereum.chainId;
      return { gp, user: { chainId, user: accounts[0] } };
    }
  }

  return { gp };
}

export async function init(app: ElmApp<Ports>, gp: GlobalParams) {
  const provider = await getProvider();
  const network = await provider.getNetwork();
  gp.provider = provider;

  const whitelist = new WhiteList(rinkeby, provider, network);

  pool(app, whitelist, gp);

  lend(app, whitelist);
  lendSigner(app, whitelist, gp);

  withdrawSigner(app, whitelist, gp);

  borrow(app, whitelist);
  borrowSigner(app, whitelist, gp);

  pay(app);
  paySigner(app, whitelist, gp);

  faucetSigner(app, gp);

  portsInit(app, whitelist, gp);
  metamaskConnected(app, whitelist, gp);
  metamaskChainChange(app, whitelist, gp);
  metamaskAccountsChange(app, whitelist, gp);
}

function portsInit(app: ElmApp<Ports>, whitelist: WhiteList, gp: GlobalParams) {
  app.ports.connectMetamask.subscribe(() => {
    if (window.ethereum && ethereum) {
      gp.metamask = window.ethereum;
      metamaskConnect(app, whitelist, gp);
    } else {
      app.ports.noMetamask.send();
    }
  });

  app.ports.disconnect.subscribe(() => {
    app.ports.metamaskMsg.send(null);
  });
}

function metamaskConnect(
  app: ElmApp<Ports>,
  whitelist: WhiteList,
  gp: GlobalParams
) {
  gp.metamask.send("eth_requestAccounts", []).then((accounts: string[]) => {
    app.ports.metamaskMsg.send({
      chainId: ethereum.chainId,
      user: accounts[0],
    });
    gp.metamaskSigner = gp.metamask.getSigner();

    balancesInit(app, whitelist, gp.metamaskProvider, accounts[0]);
    lendPositionsInit(app, whitelist, gp.metamaskProvider, accounts[0]);
    borrowPositionsInit(app, whitelist, gp.metamaskProvider, accounts[0]);
  });
}

function metamaskConnected(
  app: ElmApp<Ports>,
  whitelist: WhiteList,
  gp: GlobalParams
) {
  gp.metamask.send("eth_accounts", []).then((accounts: string[]) => {
    if (accounts[0]) {
      app.ports.metamaskMsg.send({
        chainId: ethereum.chainId,
        user: accounts[0],
      });
      gp.metamaskSigner = gp.metamask.getSigner();

      balancesInit(app, whitelist, gp.metamaskProvider, accounts[0]);
      lendPositionsInit(app, whitelist, gp.metamaskProvider, accounts[0]);
      borrowPositionsInit(app, whitelist, gp.metamaskProvider, accounts[0]);
    }
  });
}

function metamaskChainChange(
  app: ElmApp<Ports>,
  whitelist: WhiteList,
  gp: GlobalParams
) {
  ethereum.on("chainChanged", (chainId: string) => {
    gp.metamask!.send("eth_accounts", []).then((accounts: string[]) => {
      if (accounts[0]) {
        app.ports.metamaskMsg.send({
          chainId,
          user: accounts[0],
        });

        gp.metamask = window.ethereum;
        gp.metamaskSigner = gp.metamask.getSigner();

        balancesInit(app, whitelist, gp.metamaskProvider, accounts[0]);
        lendPositionsInit(app, whitelist, gp.metamaskProvider, accounts[0]);
        borrowPositionsInit(app, whitelist, gp.metamaskProvider, accounts[0]);
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
      app.ports.metamaskMsg.send({
        chainId: ethereum.chainId,
        user: accounts[0],
      });

      gp.metamask = window.ethereum;
      gp.metamaskSigner = gp.metamask.getSigner();
      balancesInit(app, whitelist, gp.metamaskProvider, accounts[0]);
      lendPositionsInit(app, whitelist, gp.metamaskProvider, accounts[0]);
      borrowPositionsInit(app, whitelist, gp.metamaskProvider, accounts[0]);
    } else {
      app.ports.metamaskMsg.send(null);
    }
  });
}
