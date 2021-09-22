import { BaseProvider, Network } from "@ethersproject/providers";
import { ERC20Token, NativeToken, Pool } from "@timeswap-labs/timeswap-v1-sdk";
import { Uint256 } from "@timeswap-labs/timeswap-v1-sdk-core";
import type { CollateralizedDebt } from "./typechain";
import { CollateralizedDebt__factory } from "./typechain";

export class WhiteList {
  provider: BaseProvider;
  network: Network;
  convenience: string;

  private tokens: Map<string, ERC20Token | NativeToken>;
  private pairs: Map<string, Map<string, string>>;
  private pools: Map<
    string,
    Map<
      string,
      Map<
        number,
        {
          pool: Pool;
          liquidity: ERC20Token;
          bond: ERC20Token;
          insurance: ERC20Token;
          collateralizedDebt: CollateralizedDebt;
        }
      >
    >
  >;
  private tokenIds?: Map<
    string,
    Map<number, { debt: string; collateral: string }>
  >;

  constructor(
    whitelist: WhitelistInput,
    provider: BaseProvider,
    network: Network
  ) {
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
        map.set(asset, (map.get(asset) ?? new Map()).set(collateral, pair)),
      new Map<string, Map<string, string>>()
    );

    this.pools = new Map();
    for (const { asset, collateral, pools } of whitelist.pairs) {
      for (const {
        maturity,
        liquidity,
        bond,
        insurance,
        collateralizedDebt,
      } of pools) {
        const pool = new Pool(
          provider,
          this.tokens.get(asset)!,
          this.tokens.get(collateral)!,
          new Uint256(maturity),
          this.convenience,
          this.pairs.get(asset)!.get(collateral)!
        );

        this.pools.set(
          asset,
          (this.pools.get(asset) ?? new Map()).set(
            collateral,
            (
              (this.pools.get(asset) ?? new Map()).get(collateral) ?? new Map()
            ).set(maturity, {
              pool,
              liquidity: new ERC20Token(
                provider,
                network.chainId,
                18,
                liquidity
              ),
              bond: new ERC20Token(provider, network.chainId, 18, bond),
              insurance: new ERC20Token(
                provider,
                network.chainId,
                18,
                insurance
              ),
              collateralizedDebt: CollateralizedDebt__factory.connect(
                collateralizedDebt,
                provider
              ),
            })
          )
        );
      }
    }
  }

  getToken(address: string): ERC20Token | NativeToken {
    return this.tokens.get(address)!;
  }

  getPairAddress(asset: string, collateral: string): string {
    return this.pairs.get(asset)!.get(collateral)!;
  }

  getPool(asset: string, collateral: string, maturity: number): Pool {
    return this.pools.get(asset)!.get(collateral)!.get(maturity)!.pool;
  }

  getLiquidity(
    asset: string,
    collateral: string,
    maturity: number
  ): ERC20Token {
    return this.pools.get(asset)!.get(collateral)!.get(maturity)!.liquidity;
  }

  getBond(asset: string, collateral: string, maturity: number): ERC20Token {
    return this.pools.get(asset)!.get(collateral)!.get(maturity)!.bond;
  }

  getInsurance(
    asset: string,
    collateral: string,
    maturity: number
  ): ERC20Token {
    return this.pools.get(asset)!.get(collateral)!.get(maturity)!.insurance;
  }

  tokenEntries() {
    return this.tokens.entries();
  }

  poolEntries(): PoolEntry[] {
    const pools: PoolEntry[] = [];

    for (const [asset, assetMap] of this.pools.entries()) {
      for (const [collateral, collateralMap] of assetMap.entries()) {
        for (const [
          maturity,
          { pool, liquidity, bond, insurance, collateralizedDebt },
        ] of collateralMap.entries()) {
          pools.push({
            asset,
            collateral,
            maturity,
            pool,
            liquidity,
            bond,
            insurance,
            collateralizedDebt,
          });
        }
      }
    }

    return pools;
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

interface PoolEntry {
  asset: string;
  collateral: string;
  maturity: number;
  pool: Pool;
  liquidity: ERC20Token;
  bond: ERC20Token;
  insurance: ERC20Token;
  collateralizedDebt: CollateralizedDebt;
}
