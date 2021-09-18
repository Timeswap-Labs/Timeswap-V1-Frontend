import rinkeby from "../../whitelist/rinkeby.json";

import { Web3Provider } from "@ethersproject/providers";

import "regenerator-runtime/runtime";
import { WhiteList } from "./whitelist";
import { getProvider } from "./provider";
import { pool } from "./pool";
import { lend, lendSigner } from "./lend";
import { GlobalParams } from "./global";
import { balancesInit } from "./balances";
import { positionsInit } from "./positions";
import { borrow, borrowSigner } from "./borrow";
import { pay, paySigner } from "./pay";

export declare let window: any;

export async function init(app: ElmApp<Ports>) {
  const provider = await getProvider();
  const network = await provider.getNetwork();
  const globalParams = new GlobalParams(provider);

  const whitelist = new WhiteList(rinkeby, provider, network);

  pool(app, whitelist);

  lend(app, whitelist);
  lendSigner(app, whitelist, globalParams);

  borrow(app, whitelist);
  borrowSigner(app, whitelist, globalParams);

  pay(app);
  paySigner(app, whitelist, globalParams);

  portsInit(app, whitelist, globalParams);
}

function portsInit(app: ElmApp<Ports>, whitelist: WhiteList, gp: GlobalParams) {
  app.ports.connectMetamask.subscribe(() => {
    if (window.ethereum && ethereum) {
      gp.metamaskProvider = new Web3Provider(window.ethereum);

      gp.metamaskProvider
        .send("eth_requestAccounts", [])
        .then((accounts: string[]) => {
          app.ports.metamaskMsg.send({
            chainId: ethereum.chainId,
            user: accounts[0],
          });

          balancesInit(app, whitelist, gp.metamaskProvider!, accounts[0]);
          positionsInit(app, whitelist, gp.metamaskProvider!, accounts[0]);

          gp.metamaskSigner = gp.metamaskProvider!.getSigner();
        });

      ethereum.on("chainChanged", (chainId: string) => {
        ethereum
          .request({ method: "eth_requestAccounts" })
          .then((accounts: string[]) => {
            app.ports.metamaskMsg.send({
              chainId,
              user: accounts[0],
            });

            gp.metamaskProvider = new Web3Provider(window.ethereum);
            gp.metamaskSigner = gp.metamaskProvider.getSigner();

            balancesInit(app, whitelist, gp.metamaskProvider!, accounts[0]);
            positionsInit(app, whitelist, gp.metamaskProvider!, accounts[0]);
          });
      });

      ethereum.on("accountsChanged", (accounts: string[]) => {
        if (accounts[0]) {
          app.ports.metamaskMsg.send({
            chainId: ethereum.chainId,
            user: accounts[0],
          });

          gp.metamaskProvider = new Web3Provider(window.ethereum);
          gp.metamaskSigner = gp.metamaskProvider.getSigner();
          balancesInit(app, whitelist, gp.metamaskProvider, accounts[0]);
          positionsInit(app, whitelist, gp.metamaskProvider!, accounts[0]);
        } else {
          app.ports.metamaskMsg.send(null);
        }
      });

      app.ports.disconnect.subscribe(() => {
        app.ports.metamaskMsg.send(null);
      });
    } else {
      app.ports.noMetamask.send();
    }
  });
}
