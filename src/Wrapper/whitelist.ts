import { Network, Provider } from "@ethersproject/providers";
import { ERC20Token, NativeToken, Pool } from "@timeswap-labs/timeswap-v1-sdk";
import { Uint256 } from "@timeswap-labs/timeswap-v1-sdk-core";

export class WhiteList {
  provider: Provider;
  network: Network;
  convenience: string;

  private tokens: Map<string, ERC20Token | NativeToken>;
  private pairs: Map<string, string>;
  private pools: Map<
    string,
    {
      pool: Pool;
      liquidity: ERC20Token;
      bond: ERC20Token;
      insurance: ERC20Token;
    }
  >;

  constructor(whitelist: WhitelistInput, provider: Provider, network: Network) {
    this.provider = provider;
    this.network = network;
    this.convenience = whitelist.convenience;

    this.tokens = whitelist.erc20s.reduce(
      (map, { address, name, symbol, decimals }) =>
        map.set(
          address,
          new ERC20Token(
            provider,
            network.chainId,
            decimals,
            address,
            symbol,
            name
          )
        ),
      new Map<string, ERC20Token | NativeToken>()
    );
    this.tokens.set(
      "ETH",
      new NativeToken(provider, network.chainId, 18, "ETH", "Ether")
    );

    this.pairs = whitelist.pairs.reduce(
      (map, { asset, collateral, pair }) =>
        map.set(JSON.stringify({ asset, collateral }), pair),
      new Map<string, string>()
    );

    this.pools = new Map();
    for (const { asset, collateral, pools } of whitelist.pairs) {
      for (const { maturity, liquidity, bond, insurance } of pools) {
        const pool = new Pool(
          provider,
          this.tokens.get(asset)!,
          this.tokens.get(collateral)!,
          new Uint256(maturity),
          this.convenience,
          this.pairs.get(JSON.stringify({ asset, collateral }))!
        );

        this.pools.set(JSON.stringify({ asset, collateral, maturity }), {
          pool,
          liquidity: new ERC20Token(provider, network.chainId, 18, liquidity),
          bond: new ERC20Token(provider, network.chainId, 18, bond),
          insurance: new ERC20Token(provider, network.chainId, 18, insurance),
        });
      }
    }
  }

  getToken(address: string): ERC20Token | NativeToken {
    return this.tokens.get(address)!;
  }

  getPairAddress(asset: string, collateral: string): string {
    return this.pairs.get(JSON.stringify({ asset, collateral }))!;
  }

  getPool(asset: string, collateral: string, maturity: number): Pool {
    return this.pools.get(JSON.stringify({ asset, collateral, maturity }))!
      .pool;
  }

  getLiquidity(
    asset: string,
    collateral: string,
    maturity: number
  ): ERC20Token {
    return this.pools.get(JSON.stringify({ asset, collateral, maturity }))!
      .liquidity;
  }

  getBond(asset: string, collateral: string, maturity: number): ERC20Token {
    return this.pools.get(JSON.stringify({ asset, collateral, maturity }))!
      .bond;
  }

  getInsurance(
    asset: string,
    collateral: string,
    maturity: number
  ): ERC20Token {
    return this.pools.get(JSON.stringify({ asset, collateral, maturity }))!
      .insurance;
  }

  tokenEntries() {
    return this.tokens.entries();
  }

  poolEntries() {
    return this.pools.entries();
  }
}

interface WhitelistInput {
  chain: string;
  chainId: string;
  convenience: string;
  erc20s: {
    address: string;
    name: string;
    symbol: string;
    decimals: number;
  }[];
  pairs: {
    pair: string;
    asset: string;
    collateral: string;
    pools: {
      maturity: number;
      liquidity: string;
      bond: string;
      insurance: string;
      collateralizedDebt: string;
    }[];
  }[];
}
