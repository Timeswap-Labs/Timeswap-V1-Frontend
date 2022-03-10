import { GlobalParams } from "./global";
import {
  ERC20Token as SDKCoreERC20Token,
  NativeToken as SDKCoreNativeToken,
  Pool,
  Uint256,
} from "@timeswap-labs/timeswap-v1-sdk-core";
import { Pool as SDKPool } from "@timeswap-labs/timeswap-v1-sdk";
import { Contract } from "@ethersproject/contracts";

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

export function getPool(query: LendQuery | BorrowQuery): Pool {
  let asset, collateral;

  if ((query.pool.asset as ERC20Token).address) {
    asset = new SDKCoreERC20Token(
      query.chain.chainId,
      query.pool.asset.decimals,
      (query.pool.asset as ERC20Token).address
    );
  } else {
    asset = new SDKCoreNativeToken(
      query.chain.chainId,
      query.pool.asset.decimals,
      query.pool.asset.symbol
    );
  }

  if ((query.pool.collateral as ERC20Token).address) {
    collateral = new SDKCoreERC20Token(
      query.chain.chainId,
      query.pool.collateral.decimals,
      (query.pool.collateral as ERC20Token).address
    );
  } else {
    collateral = new SDKCoreNativeToken(
      query.chain.chainId,
      query.pool.collateral.decimals,
      query.pool.collateral.symbol
    );
  }

  return new Pool(
    asset,
    collateral,
    query.pool.maturity,
    query.poolInfo.fee,
    query.poolInfo.protocolFee
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
    assetToken = new SDKCoreERC20Token(
      chain.chainId,
      asset.decimals,
      (asset as ERC20Token).address
    );
  } else {
    assetToken = new SDKCoreNativeToken(
      chain.chainId,
      asset.decimals,
      asset.symbol
    );
  }

  if ((collateral as ERC20Token).address) {
    collateralToken = new SDKCoreERC20Token(
      chain.chainId,
      collateral.decimals,
      (collateral as ERC20Token).address
    );
  } else {
    collateralToken = new SDKCoreNativeToken(
      chain.chainId,
      collateral.decimals,
      collateral.symbol
    );
  }

  return new SDKPool(
    gp.walletProvider,
    chain.chainId,
    assetToken,
    collateralToken,
    new Uint256(maturity)
  );
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
