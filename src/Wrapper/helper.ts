import { GlobalParams } from './global';
import { ERC20Token, NativeToken, Pool, Uint256 } from "@timeswap-labs/timeswap-v1-sdk-core";
import { Pool as SDKPool } from "@timeswap-labs/timeswap-v1-sdk";
import { Contract } from "@ethersproject/contracts";
import { BorrowQuery, LendQuery, ERC20Token as ERC20, NativeToken as Native } from "./declaration";

export function updateErc20Balance(
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
      const parsedData : {[key: string] : ERC20Token[]}  = JSON.parse(storeData);
      customTokens = parsedData[chainId];
    } catch (error) {
      return [];
    }
  }

  return customTokens;
}

export function getPool(query: LendQuery | BorrowQuery): Pool {
  let asset, collateral;

  if ((query.pool.asset as ERC20Token).address) {
    asset = new ERC20Token(
      query.chain.chainId,
      query.pool.asset.decimals,
      (query.pool.asset as ERC20Token).address
    );
  }
  else {
    asset = new NativeToken(
      query.chain.chainId,
      query.pool.asset.decimals,
      query.pool.asset.symbol
    );
  }

  if ((query.pool.collateral as ERC20Token).address) {
    collateral = new ERC20Token(
      query.chain.chainId,
      query.pool.collateral.decimals,
      (query.pool.collateral as ERC20Token).address
    );
  } else {
    collateral = new NativeToken(
      query.chain.chainId,
      query.pool.collateral.decimals,
      query.pool.collateral.symbol
    );
  }

  return new Pool(asset, collateral, query.pool.maturity, query.poolInfo.fee, query.poolInfo.protocolFee);
}

export function getPoolSDK(
  gp: GlobalParams,
  asset: ERC20 | Native,
  collateral: ERC20 | Native,
  maturity: number | string,
  chain: Chain,
): SDKPool {
  let assetToken, collateralToken;

  if ((asset as ERC20).address) {
    assetToken = new ERC20Token(
      chain.chainId,
      asset.decimals,
      (asset as ERC20).address
    );
  }
  else {
    assetToken = new NativeToken(
      chain.chainId,
      asset.decimals,
      asset.symbol
    );
  }

  if ((collateral as ERC20).address) {
    collateralToken = new ERC20Token(
      chain.chainId,
      collateral.decimals,
      (collateral as ERC20).address
    );
  } else {
    collateralToken = new NativeToken(
      chain.chainId,
      collateral.decimals,
      collateral.symbol
    );
  }

  return new SDKPool(gp.metamaskProvider, chain.chainId, assetToken, collateralToken, new Uint256(maturity));
}

export function updateCachedTxns(txnReceipt: ReceiveReceipt) {
  const storedTxns = window.localStorage.getItem("txns");

  if (storedTxns) {
    try {
      const parsedTxns: { chain: Chain; address: string; txns: Txns } = JSON.parse(storedTxns);

      if (parsedTxns.address === txnReceipt.address && parsedTxns.chain.chainId === txnReceipt.chain.chainId) {
        const txnIndex = parsedTxns.txns.confirmed.findIndex(txn => txn.hash === txnReceipt.hash);

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
