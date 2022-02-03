import { getProvider } from "./provider";
import { pool } from "./pool";
import { lend, lendSigner } from "./lend";
import { GlobalParams } from "./global";
import { balancesAllowancesInit, fetchAllowancesOf, fetchBalancesOf } from "./balances";
import { positionsInit } from "./positions";
import { borrow, borrowSigner } from "./borrow";
import { pay, paySigner } from "./pay";
import { withdrawSigner } from "./withdraw";
import { faucetSigner } from "./faucet";
import { wallet } from "./wallet";
import { getChainData, getTokenList } from "./chains";
import { approveSigner } from "./approve";
import { listenForPendingTxns } from "./helper";

export declare let window: any;

export async function elmUser(): Promise<{
  gp: GlobalParams;
  user: ReceiveUser | null;
}> {
  const gp = new GlobalParams();

  if (window.ethereum && ethereum) {
    gp.metamaskProvider = window.ethereum;
    gp.network = await gp.metamaskProvider.send("eth_chainId", []);

    // if (gp.network !== "0x4") {
    //   gp.metamaskProvider.send("wallet_switchEthereumChain", [
    //     { chainId: "0x4" },
    //   ]);
    //   return { gp, user: null };
    // }

    const accounts: string[] = await gp.metamaskProvider.send(
      "eth_accounts",
      []
    );

    if (accounts[0]) {
      let txns: Txns = { confirmed: [], uncomfirmed: [] };
      const wallet = "metamask";
      const chainId = Number(gp.network);
      const storedTxns = window.localStorage.getItem("txns");

      if (storedTxns) {
        try {
          const parsedTxns: { chain: Chain; address: string; txns: Txns } =
            JSON.parse(storedTxns);
          txns =
            parsedTxns.address === accounts[0] && parsedTxns.chain.chainId === chainId
              ? parsedTxns.txns
              : txns;
        } catch (err) {
          txns = { confirmed: [], uncomfirmed: [] };
        }
      }

      return { gp, user: { wallet, chainId, address: accounts[0], txns } };
    }
  }

  return { gp, user: null };
}

export async function init(
  app: ElmApp<Ports>,
  gp: GlobalParams,
  user: ReceiveUser | null
) {
  const provider = await getProvider(gp);
  gp.provider = provider;

  listenForPendingTxns(app, gp);

  approveSigner(app, gp);
  // pool(app, whitelist, gp);

  lend(app);
  lendSigner(app, gp);
  withdrawSigner(app, gp);

  borrow(app);
  borrowSigner(app, gp);

  pay(app);
  paySigner(app, gp);

  // faucetSigner(app, gp);

  portsInit(app, gp);

  if (gp.metamaskProvider) {
    metamaskConnected(app, gp);
    metamaskChainChange(app, gp);
    metamaskAccountsChange(app, gp);
  }
}

function portsInit(app: ElmApp<Ports>, gp: GlobalParams) {
  app.ports.connect.subscribe((walletName) => {
    if (wallet[walletName]) {
      gp.metamaskProvider = wallet[walletName]!; //TODO: Fix null val
      receiveUser(app, gp, walletName);
    } else {
      app.ports.receiveNoConnect.send(walletName);
    }

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

  app.ports.balancesOf.subscribe((balancesOf) => {
    fetchBalancesOf(app, gp, balancesOf);
  });

  app.ports.allowancesOf.subscribe((allowancesOf) => {
    fetchAllowancesOf(app, gp, allowancesOf);
  });

  app.ports.copyToClipboard.subscribe((copyStr) => {
    navigator.clipboard.writeText(copyStr);
  });

  app.ports.cacheSlippage.subscribe((slippage) => {
    window.localStorage.setItem("slippage", slippage);
  });

  app.ports.cacheDeadline.subscribe((deadline) => {
    window.localStorage.setItem("deadline", deadline);
  });

  app.ports.cachePriceFeed.subscribe((priceFeed) => {
    window.localStorage.setItem("priceFeed", priceFeed);
  });

  app.ports.cacheChosenZone.subscribe((zone) => {
    window.localStorage.setItem("chosen-zone", zone);
  });

  app.ports.cacheTheme.subscribe((theme) => {
    window.localStorage.setItem("theme", theme);
  });

  app.ports.cacheCustom.subscribe((customTokens) => {
    window.localStorage.setItem("custom-tokens", JSON.stringify(customTokens));
  });

  app.ports.cacheTxns.subscribe((txns) => {
    window.localStorage.setItem("txns", JSON.stringify(txns));
  });

  app.ports.changeChain.subscribe((chain) => {
    if (window.ethereum && ethereum) {
      gp.metamaskProvider = window.ethereum;

      gp.metamaskProvider.send("wallet_switchEthereumChain", [
        {
          chainId: chain.chainId.toString().startsWith("0x")
            ? chain.chainId
            : "0x" + chain.chainId,
        },
      ]);
    }
  });

  window.addEventListener("scroll", () => {
    app.ports.scroll.send;
  });
}

function receiveUser(
  app: ElmApp<Ports>,
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

      userInit(app, gp, accounts[0]);
    })
    .catch(() => {
      console.log("Wrapper", "Failed");
      app.ports.receiveNoConnect.send(walletName);
    });
}

function metamaskConnected(
  app: ElmApp<Ports>,
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

      userInit(app, gp, accounts[0]);
    }
  });
}

function metamaskChainChange(
  app: ElmApp<Ports>,
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

        chainInit(app, gp, accounts[0]);
      }
    });
  });
}

function metamaskAccountsChange(
  app: ElmApp<Ports>,
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

      userInit(app, gp, accounts[0]);
    } else {
      app.ports.receiveUser.send(null);
    }
  });
}

async function chainInit(
  app: ElmApp<Ports>,
  gp: GlobalParams,
  account: string
) {
  gp.provider = await getProvider(gp);
  gp.network = await gp.metamaskProvider.send("eth_chainId", []);
  userInit(app, gp, account);
}

function userInit(
  app: ElmApp<Ports>,
  gp: GlobalParams,
  userAddress: string
) {
  const wlChain = getChainData(Number(gp.network));

  if (userAddress && wlChain) {
    const balancesOf: BalancesOf = {
      chain: {
        chainId: wlChain.chainId,
        name: wlChain.name,
        etherscan: wlChain.etherscan,
      },
      address: userAddress,
      tokens: getTokenList(Number(gp.network)),
    };

    balancesAllowancesInit(app, gp, balancesOf);
    positionsInit(app, gp);
  }
}
