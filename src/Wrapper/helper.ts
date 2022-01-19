import { ERC20Token, NativeToken, Pair, Pool, Uint16, Uint256 } from "@timeswap-labs/timeswap-v1-sdk-core";
import { Contract } from "@ethersproject/contracts";

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

  const pair = new Pair(asset as ERC20Token, collateral as ERC20Token, new Uint16(50), new Uint16(50));

  return new Pool(pair, new Uint256(query.pool.maturity) );
}
