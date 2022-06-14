import { GlobalParams } from "./global";
import {
  ERC20TokenCore,
  NativeTokenCore,
  PoolCore,
  Uint,
  Uint256,
} from "@timeswap-labs/timeswap-v1-biconomy-sdk";
import { Pool as SDKPool } from "@timeswap-labs/timeswap-v1-biconomy-sdk";

import { Contract } from "@ethersproject/contracts";
import { ethers } from 'ethers';

export function updateTransferEventBalance(
  contract: Contract,
  address: string,
  updateBalance: () => void
) {
  const balancesInFilter = contract.filters.Transfer(null, address);
  const balancesOutFilter = contract.filters.Transfer(address);

  contract.on(balancesInFilter, updateBalance);
  contract.on(balancesOutFilter, updateBalance);
}

export function getCurrentTime(): Uint256 {
  return new Uint256(Date.now()).div(1000);
}

export function calculateMinValue(value: Uint, slippage: number): Uint256 {
  return new Uint256(value)
    .mul(Math.round(100000 * (1 - slippage)))
    .div(100000);
}

export function calculateMaxValue(value: Uint, slippage: number): Uint256 {
  return new Uint256(value)
    .mul(Math.round(100000 * (1 + slippage)))
    .div(100000);
}

export function getCustomTokens(chainId: string): ERC20Token[] {
  const storeData = window.localStorage.getItem("custom-tokens");
  let customTokens: ERC20Token[] = [];

  if (storeData) {
    try {
      const parsedData: { [key: string]: ERC20Token[] } = JSON.parse(storeData);
      customTokens = parsedData[chainId] || [];
    } catch (error) {
      return [];
    }
  }

  return customTokens;
}

export function getPool(
  query: LendQuery | BorrowQuery | LiquidityQuery | NewLiquidityQuery | LiqReturn,
  fee: number,
  protocolFee: number,
): PoolCore {
  let asset, collateral;

  if ((query.pool.asset as ERC20Token).address) {
    asset = new ERC20TokenCore(
      query.chain.chainId,
      query.pool.asset.decimals,
      (query.pool.asset as ERC20Token).address
    );
  } else {
    asset = new NativeTokenCore(
      query.chain.chainId,
      query.pool.asset.decimals,
      query.pool.asset.symbol
    );
  }

  if ((query.pool.collateral as ERC20Token).address) {
    collateral = new ERC20TokenCore(
      query.chain.chainId,
      query.pool.collateral.decimals,
      (query.pool.collateral as ERC20Token).address
    );
  } else {
    collateral = new NativeTokenCore(
      query.chain.chainId,
      query.pool.collateral.decimals,
      query.pool.collateral.symbol
    );
  }

  return new PoolCore(
    asset,
    collateral,
    query.pool.maturity,
    fee,
    protocolFee
  );
}

export function getPoolSDK(
  gp: GlobalParams,
  asset: ERC20Token | NativeToken,
  collateral: ERC20Token | NativeToken,
  maturity: number | string,
  chain: Chain
): SDKPool {
  let assetToken, collateralToken;

  if ((asset as ERC20Token).address) {
    assetToken = new ERC20TokenCore(
      chain.chainId,
      asset.decimals,
      (asset as ERC20Token).address
    );
  } else {
    assetToken = new NativeTokenCore(
      chain.chainId,
      asset.decimals,
      asset.symbol
    );
  }

  if ((collateral as ERC20Token).address) {
    collateralToken = new ERC20TokenCore(
      chain.chainId,
      collateral.decimals,
      (collateral as ERC20Token).address
    );
  } else {
    collateralToken = new NativeTokenCore(
      chain.chainId,
      collateral.decimals,
      collateral.symbol
    );
  }

  return new SDKPool(
    gp.biconomyProvider,
    chain.chainId,
    assetToken,
    collateralToken,
    new Uint256(maturity)
  );
}

export function fetchRecentTxns(gp: GlobalParams, accountAddr: string): Txns {
  const chainId = Number(gp.network);
  const storedTxns = window.localStorage.getItem("txns");
  let txns: Txns = { confirmed: [], uncomfirmed: [] };

  if (storedTxns) {
    try {
      const parsedTxns: { chain: Chain; address: string; txns: Txns } =
        JSON.parse(storedTxns);
      txns =
        parsedTxns.address === accountAddr &&
        parsedTxns.chain.chainId === chainId
          ? parsedTxns.txns
          : txns;
    } catch (err) {
      txns = { confirmed: [], uncomfirmed: [] };
    }
  }

  return txns;
}

export function updateCachedTxns(txnReceipt: ReceiveReceipt) {
  const storedTxns = window.localStorage.getItem("txns");

  if (storedTxns) {
    try {
      const parsedTxns: { chain: Chain; address: string; txns: Txns } =
        JSON.parse(storedTxns);

      if (
        parsedTxns.address === txnReceipt.address &&
        parsedTxns.chain.chainId === txnReceipt.chain.chainId
      ) {
        const txnIndex = parsedTxns.txns.confirmed.findIndex(
          (txn) => txn.hash === txnReceipt.hash
        );

        if (txnIndex >= 0) {
          parsedTxns.txns.confirmed[txnIndex].state = txnReceipt.state;
          window.localStorage.setItem("txns", JSON.stringify(parsedTxns));
        }
      }
    } catch {
      // parse error
    }
  }
}

export function removeFromCachedTxns(txnReceipt: ReceiveReceipt) {
  const storedTxns = window.localStorage.getItem("txns");

  if (storedTxns) {
    try {
      const parsedTxns: { chain: Chain; address: string; txns: Txns } =
        JSON.parse(storedTxns);

      if (
        parsedTxns.address === txnReceipt.address &&
        parsedTxns.chain.chainId === txnReceipt.chain.chainId
      ) {
        const txnIndex = parsedTxns.txns.confirmed.findIndex(
          (txn) => txn.hash === txnReceipt.hash
        );

        if (txnIndex >= 0) {
          parsedTxns.txns.confirmed.splice(txnIndex, 1);
          window.localStorage.setItem("txns", JSON.stringify(parsedTxns));
        }
      }
    } catch {
      // parse error
    }
  }
}

export function replaceInCachedTxns(txnReceipt: ReceiveReceipt, oldTxnHash: string) {
  const storedTxns = window.localStorage.getItem("txns");

  if (storedTxns) {
    try {
      const parsedTxns: { chain: Chain; address: string; txns: Txns } =
        JSON.parse(storedTxns);

      if (
        parsedTxns.address === txnReceipt.address &&
        parsedTxns.chain.chainId === txnReceipt.chain.chainId
      ) {
        const txnIndex = parsedTxns.txns.confirmed.findIndex(
          (txn) => txn.hash === oldTxnHash
        );

        if (txnIndex >= 0) {
          parsedTxns.txns.confirmed[txnIndex].hash = txnReceipt.hash;
          parsedTxns.txns.confirmed[txnIndex].state = txnReceipt.state;
          window.localStorage.setItem("txns", JSON.stringify(parsedTxns));
        }
      }
    } catch {
      // parse error
    }
  }
}

export function listenForPendingTxns(app: ElmApp<Ports>, gp: GlobalParams) {
  const storedTxns = window.localStorage.getItem("txns");

  if (storedTxns) {
    const parsedTxnData: { chain: Chain; address: string; txns: Txns } =
      JSON.parse(storedTxns);
    const pendingTxns = parsedTxnData.txns.confirmed.filter(
      (txn) => txn.state === "pending"
    );

    pendingTxns.forEach(async (pendingTxn) => {
      const txnReceipt = await gp.walletProvider.waitForTransaction(pendingTxn.hash);
      const receiveReceipt = {
        chain: parsedTxnData.chain,
        address: parsedTxnData.address,
        hash: pendingTxn.hash,
        state: txnReceipt.status ? "success" : "failed",
      };
      app.ports.receiveReceipt.send(receiveReceipt);
      updateCachedTxns(receiveReceipt);
    });
  }
}

export function handleTxnErrors(
  error: any,
  app: ElmApp<Ports>,
  gp: GlobalParams,
  params: Lend | Borrow | Approve | Burn | Pay | Withdraw | Liquidity | NewLiquidity
) {
  // If txn is canceled or sped-up, new txn-hash is created
  if (error.code === ethers.utils.Logger.errors.TRANSACTION_REPLACED) {
    if (error.cancelled) {
      const receiveReceipt = {
        chain: params.chain,
        address: params.address,
        hash: error.hash,
        state: "failed"
      }
      removeFromCachedTxns(receiveReceipt);
    } else {
      // The user used "speed up" or something similar
      const receiveReceipt = {
        chain: params.chain,
        address: params.address,
        hash: error.replacement.hash,
        state: error.receipt.status ? "success" : "failed"
      }
      replaceInCachedTxns(receiveReceipt, error.hash);
    }

    // Update the UI with the replaced txns
    const recentTxns = fetchRecentTxns(gp, params.address);
    app.ports.receiveUpdatedTxns.send({
      chain: params.chain,
      address: params.address,
      txns: recentTxns
    });
  } else {
    app.ports.receiveConfirm.send({
      id: params.id,
      chain: params.chain,
      address: params.address,
      hash: null
    });
  }
}

export function compareConvAddress(convAddress: string, chainId: number) {
    const now = Date.now();

    // if (CONVENIENCE[chainId] && (CONVENIENCE[chainId].toLowerCase() !== convAddress.toLowerCase())) {
    //   const lastRefreshTime = window.localStorage.getItem("lastRefresh");

    //   // Reload pg only if more than 2-mins have passed since last refresh
    //   if (!lastRefreshTime
    //     || (lastRefreshTime && (now - Number(lastRefreshTime) > 120000) )) {
    //     window.localStorage.setItem("lastRefresh", now.toString());
    //     window.location.reload();
    //   }
    // }
}
