import WalletConnectProvider from '@walletconnect/web3-provider';
import { Provider, Web3Provider } from '@ethersproject/providers';
import { providers } from "ethers";
import { lend, lendSigner } from "./lend";
import { GlobalParams } from "./global";
import {
  balancesAllowancesInit,
  fetchAllowancesOf,
  fetchBalancesOf,
} from "./balances";
import { positionsInit } from "./positions";
import { borrow, borrowSigner } from "./borrow";
import { pay, paySigner } from "./pay";
import { withdraw, withdrawSigner } from "./withdraw";
import { faucetSigner } from "./faucet";
import { wallet } from "./wallet";
import { getChainData, getTokenList } from "./chains";
import { approveSigner } from "./approve";
import { listenForPendingTxns } from "./helper";
import { swapSigner } from "./swap";

export declare let window: any;

export async function elmUser(): Promise<{
  gp: GlobalParams;
  user: ReceiveUser | null;
}> {
  const gp = new GlobalParams();

  if (window.ethereum && ethereum) {
    gp.walletProvider = window.ethereum;
    gp.network = await gp.walletProvider.send("eth_chainId", []);

    const accounts: string[] = await gp.walletProvider.send(
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
            parsedTxns.address === accounts[0] &&
            parsedTxns.chain.chainId === chainId
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
  // gp.provider = await getProvider(gp);

  listenForPendingTxns(app, gp);

  approveSigner(app, gp);
  // pool(app, whitelist, gp);

  lend(app);
  lendSigner(app, gp);

  withdraw(app);
  withdrawSigner(app, gp);

  borrow(app);
  borrowSigner(app, gp);

  pay(app);
  paySigner(app, gp);

  // faucetSigner(app, gp);

  portsInit(app, gp);

  swapSigner(app, gp);

  if (gp.walletProvider) {
    metamaskConnected(app, gp);
    metamaskChainChange(app, gp);
    metamaskAccountsChange(app, gp);
  }
}

function portsInit(app: ElmApp<Ports>, gp: GlobalParams) {
  app.ports.connect.subscribe(async (walletName) => {
    if (wallet[walletName]) {
      gp.walletProvider = wallet[walletName]!; //TODO: Fix null val

      if (walletName === "walletConnect") {
        //  Enable session (triggers QR Code modal)
        await (gp.walletProvider.provider as WalletConnectProvider).enable();
      }
      receiveUser(app, gp, walletName);
    } else {
      app.ports.receiveNoConnect.send(walletName);
    }
  });

  // app.ports.disconnect.subscribe(() => {
  //   app.ports.receiveUser.send(null);
  // });

  // FIXME: This causes an issue during chain change
  app.ports.balancesOf.subscribe((balancesOf) => {
    fetchBalancesOf(app, gp, balancesOf);
  });

  // FIXME: This causes an issue during chain change
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

  app.ports.changeChain.subscribe(async (chain) => {
    if (window.ethereum && ethereum) {
      gp.walletProvider = window.ethereum;
      const chainId = chain.chainId.toString().startsWith("0x")
        ? chain.chainId
        : "0x" + Number(chain.chainId.toString().trim()).toString(16);

      try {
        await gp.walletProvider.send("wallet_switchEthereumChain", [
          { chainId },
        ]);
      } catch (error: any) {
        if ((error.code = 4902)) {
          await gp.walletProvider.send("wallet_addEthereumChain", [
            {
              chainId,
              chainName: chain.name,
              rpcUrls: [chain.rpcUrl],
              blockExplorerUrls: [chain.blockExplorerUrl],
            },
          ]);
        }
      }
    }
  });

  window.addEventListener("scroll", () => {
    app.ports.scroll.send;
  });
}

function receiveUser(app: ElmApp<Ports>, gp: GlobalParams, walletName: string) {
  let userAccountsPromise: Promise<any>;

  if (walletName === "walletConnect") {
    userAccountsPromise = gp.walletProvider.listAccounts();

  } else {
    userAccountsPromise = gp.walletProvider.send("eth_requestAccounts", []);
  }

  userAccountsPromise.then((accounts: string[]) => {
    app.ports.receiveUser.send({
      chainId: Number(ethereum.chainId),
      wallet: walletName,
      address: accounts[0],
      txns: {
        confirmed: [],
        uncomfirmed: [],
      },
    });

    gp.walletSigner = gp.walletProvider.getSigner();
    userInit(app, gp, accounts[0]);
  }).catch(() => {
    app.ports.receiveNoConnect.send(walletName);
  });
}

function metamaskConnected(app: ElmApp<Ports>, gp: GlobalParams) {
  gp.walletProvider.send("eth_accounts", []).then((accounts: string[]) => {
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
      gp.walletSigner = gp.walletProvider.getSigner();

      userInit(app, gp, accounts[0]);
    }
  });
}

function metamaskChainChange(app: ElmApp<Ports>, gp: GlobalParams) {
  (gp.walletProvider.provider as Provider).on("chainChanged", (chainId: string) => {
    gp.walletProvider!.send("eth_accounts", []).then((accounts: string[]) => {
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
        gp.walletProvider.removeAllListeners();

        gp.walletProvider = window.ethereum;
        gp.walletSigner = gp.walletProvider.getSigner();

        chainInit(app, gp, accounts[0]);
      }
    });
  });
}

function metamaskAccountsChange(app: ElmApp<Ports>, gp: GlobalParams) {
  (gp.walletProvider.provider as Provider).on("accountsChanged", (accounts: string[]) => {
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

      gp.walletProvider.removeAllListeners();

      gp.walletProvider = window.ethereum;
      gp.walletSigner = gp.walletProvider.getSigner();

      userInit(app, gp, accounts[0]);
    } else {
      app.ports.receiveUser.send(null);
    }
  });

  (gp.walletProvider.provider as Provider).on("disconnect", (code: number, reason: string) => {
    console.log(code, reason);
    app.ports.receiveUser.send(null);
  });
}

async function chainInit(
  app: ElmApp<Ports>,
  gp: GlobalParams,
  account: string
) {
  // gp.provider = await getProvider(gp);
  gp.network = await gp.walletProvider.send("eth_chainId", []);
  userInit(app, gp, account);
}

function userInit(app: ElmApp<Ports>, gp: GlobalParams, userAddress: string) {
  const wlChain = getChainData(Number(gp.network));

  if (userAddress && wlChain) {
    const balancesOf: BalancesOf = {
      chain: {
        chainId: wlChain.chainId,
        name: wlChain.name,
        rpcUrl: wlChain.rpcUrl,
        blockExplorerUrl: wlChain.blockExplorerUrl,
        nftExplorerUrl: wlChain.nftExplorerUrl,
      },
      address: userAddress,
      tokens: getTokenList(Number(gp.network)),
    };

    balancesAllowancesInit(app, gp, balancesOf);
    positionsInit(app, gp);
  }
}
