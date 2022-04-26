import WalletConnectProvider from '@walletconnect/web3-provider';
import { Provider } from '@ethersproject/providers';
import { lend, lendSigner } from "./lend";
import { GlobalParams } from "./global";
import {
  balancesAllowancesInit,
  fetchAllowancesOf,
  fetchBalancesOf,
} from "./balances";
import { positionsInit } from "./positions";
import { borrow, borrowSigner } from "./borrow";
import { paySigner } from "./pay";
import { withdraw, withdrawSigner } from "./withdraw";
import { wallet } from "./wallet";
import { getChainData, getNativeToken, getTokenList } from "./chains";
import { approveSigner } from "./approve";
import { listenForPendingTxns, fetchRecentTxns } from "./helper";
import { liquidity, liquiditySigner } from './liquidity';
import { burn } from './burn';

export declare let window: any;

export async function elmUser(): Promise<{
  gp: GlobalParams;
  user: ReceiveUser | null;
}> {
  const gp = new GlobalParams();
  const prevUsedWallet = window.localStorage.getItem("wallet") || "metamask";

  if (wallet[prevUsedWallet]) {
    gp.walletProvider = wallet[prevUsedWallet]!;

    if (prevUsedWallet === "walletConnect") {
      try {
        await (gp.walletProvider.provider as WalletConnectProvider).enable();
      } catch (error) {
        return { gp, user: null };
      }
    }

    gp.network = await gp.walletProvider.send("eth_chainId", []);
    const accounts: string[] = await gp.walletProvider.send("eth_accounts", []);

    if (accounts[0]) {
      const wallet = prevUsedWallet;
      const chainId = Number(gp.network);
      const txns: Txns = fetchRecentTxns(gp, accounts[0]);

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
  portsInit(app, gp);

  listenForPendingTxns(app, gp);

  approveSigner(app, gp);

  lend(app);
  lendSigner(app, gp);

  withdraw(app);
  withdrawSigner(app, gp);

  borrow(app);
  borrowSigner(app, gp);

  paySigner(app, gp);

  liquidity(app);
  liquiditySigner(app, gp);

  burn(app);

  if (gp.walletProvider) {
    walletConnected(app, gp, user);
    walletChainChange(app, gp);
    walletAccountsChange(app, gp);
  }
}

function portsInit(app: ElmApp<Ports>, gp: GlobalParams) {
  app.ports.connect.subscribe(async (walletName) => {
    if (wallet[walletName]) {
      gp.walletProvider = wallet[walletName]!; //TODO: Fix null val

      receiveUser(app, gp, walletName);
      window.localStorage.setItem("wallet", walletName);
    } else {
      app.ports.receiveNoConnect.send(walletName);
    }
  });

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

  app.ports.changeChain.subscribe(async (chain) => {
    if (gp.walletProvider) {
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
              nativeCurrency: getNativeToken(chain.chainId),
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
    userAccountsPromise = (gp.walletProvider.provider as WalletConnectProvider).enable();
  } else {
    userAccountsPromise = gp.walletProvider.send("eth_requestAccounts", []);
  }

  userAccountsPromise.then((accounts: string[]) => {
    app.ports.receiveUser.send({
      chainId: Number(ethereum.chainId),
      wallet: walletName,
      address: accounts[0],
      txns: fetchRecentTxns(gp, accounts[0]),
    });

    gp.walletSigner = gp.walletProvider.getSigner();
    userInit(app, gp, accounts[0]);
  }).catch(() => {
    app.ports.receiveNoConnect.send(walletName);
  });
}

function walletConnected(app: ElmApp<Ports>, gp: GlobalParams, user: ReceiveUser | null) {
  gp.walletProvider.send("eth_accounts", []).then((accounts: string[]) => {
    if (accounts[0]) {
      app.ports.receiveUser.send(user);
      gp.walletSigner = gp.walletProvider.getSigner();

      userInit(app, gp, accounts[0]);
    }
  });
}

function walletChainChange(app: ElmApp<Ports>, gp: GlobalParams) {
  (gp.walletProvider.provider as Provider).on("chainChanged", (chainId: string) => {
    const selectedWallet = window.localStorage.getItem("wallet") || "metamask";
    gp.network = chainId;

    gp.walletProvider!.send("eth_accounts", []).then((accounts: string[]) => {
      if (accounts[0]) {
        app.ports.receiveUser.send({
          chainId: Number(chainId),
          wallet: selectedWallet,
          address: accounts[0],
          txns: {
            confirmed: [],
            uncomfirmed: [],
          },
        });

        gp.walletProvider.removeAllListeners();

        gp.walletProvider = wallet[selectedWallet]!;
        gp.walletSigner = gp.walletProvider.getSigner();

        chainInit(app, gp, accounts[0]);
      }
    });
  });
}

function walletAccountsChange(app: ElmApp<Ports>, gp: GlobalParams) {
  (gp.walletProvider.provider as Provider).on("accountsChanged", async (accounts: string[]) => {
    const selectedWallet = window.localStorage.getItem("wallet") || "metamask";
    gp.network = await gp.walletProvider.send("eth_chainId", []);

    if (accounts[0]) {
      app.ports.receiveUser.send({
        chainId: Number(gp.network),
        wallet: selectedWallet,
        address: accounts[0],
        txns: fetchRecentTxns(gp, accounts[0]),
      });

      gp.walletProvider.removeAllListeners();

      gp.walletProvider = wallet[selectedWallet]!;
      gp.walletSigner = gp.walletProvider.getSigner();

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
