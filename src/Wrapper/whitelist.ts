import { BaseProvider, Network } from "@ethersproject/providers";
import { ERC20Token, NativeToken, Pool } from "@timeswap-labs/timeswap-v1-biconomy-sdk";
import { Uint256 } from "@timeswap-labs/timeswap-v1-biconomy-sdk";
import { Contract } from "@ethersproject/contracts";
import cdTokenAbi from "./abi/cdToken";

export class WhiteList {
  provider: BaseProvider;
  network: Network;
  convenience: string;

  private tokens: { [erc20: string]: ERC20Token | NativeToken } = {};
  private pairs: { [asset: string]: { [collateral: string]: string } } = {};
  private pools: {
    [asset: string]: {
      [collateral: string]: { [maturity: number]: PoolValue };
    };
  } = {};
  private tokenIds: Map<
    string,
    Map<string, { debt: string; collateral: string }>
  > = new Map();

  constructor(
    whitelist: WhitelistInput,
    provider: BaseProvider,
    network: Network
  ) {
    this.provider = provider;
    this.network = network;
    this.convenience = whitelist.convenience;

    whitelist.erc20s.forEach(({ address, name, symbol, decimals }) => {
      this.tokens[address] = new ERC20Token(
        provider,
        network.chainId,
        decimals,
        address,
        symbol,
        name
      );
    });
    this.tokens["ETH"] = new NativeToken(
      provider,
      network.chainId,
      18,
      "ETH",
      "Ether"
    );

    whitelist.pairs.forEach(({ asset, collateral, pair }) => {
      this.pairs[asset] = this.pairs[asset] ?? {};
      this.pairs[asset][collateral] = pair;
    });

    whitelist.pairs.forEach(({ asset, collateral, pools }) => {
      pools.forEach(
        ({ maturity, liquidity, bond, insurance, collateralizedDebt }) => {
          const pool = new Pool(
            provider,
            this.tokens[asset],
            this.tokens[collateral],
            new Uint256(maturity),
            this.convenience,
            this.pairs[asset][collateral]
          );

          this.pools[asset] = this.pools[asset] ?? {};
          this.pools[asset][collateral] = this.pools[asset][collateral] ?? {};
          this.pools[asset][collateral][maturity] = {
            pool,
            liquidity: new ERC20Token(provider, network.chainId, 18, liquidity),
            bond: new ERC20Token(provider, network.chainId, 18, bond),
            insurance: new ERC20Token(provider, network.chainId, 18, insurance),
            collateralizedDebt: new Contract(
              collateralizedDebt,
              cdTokenAbi,
              provider
            ),
          };
        }
      );
    });
  }

  getToken(address: string): ERC20Token | NativeToken {
    return this.tokens[address];
  }

  getPairAddress(asset: string, collateral: string): string {
    return this.pairs[asset][collateral];
  }

  getPool(asset: string, collateral: string, maturity: number): Pool {
    return this.pools[asset][collateral][maturity].pool;
  }

  getLiquidity(
    asset: string,
    collateral: string,
    maturity: number
  ): ERC20Token {
    return this.pools[asset][collateral][maturity].liquidity;
  }

  getBond(asset: string, collateral: string, maturity: number): ERC20Token {
    return this.pools[asset][collateral][maturity].bond;
  }

  getInsurance(
    asset: string,
    collateral: string,
    maturity: number
  ): ERC20Token {
    return this.pools[asset][collateral][maturity].insurance;
  }

  getCDToken(asset: string, collateral: string, maturity: number): Contract {
    return this.pools[asset][collateral][maturity].collateralizedDebt;
  }

  getTokenIds(cdAddress: string) {
    return this.tokenIds.get(cdAddress)!.entries();
  }

  pushTokenId(cdAddress: string, id: string, debt: string, collateral: string) {
    this.tokenIds.set(cdAddress, this.tokenIds.get(cdAddress) ?? new Map());
    this.tokenIds.get(cdAddress)!.set(id, { debt, collateral });
  }

  popTokenId(cdAddress: string, id: string) {
    this.tokenIds.get(cdAddress)!.delete(id);
  }

  tokenEntries() {
    return Object.entries(this.tokens);
  }

  poolEntries() {
    return Object.entries(this.pools).flatMap(([asset, assetMap]) =>
      Object.entries(assetMap).flatMap(([collateral, collateralMap]) =>
        Object.entries(collateralMap).map(
          ([
            maturity,
            { pool, liquidity, bond, insurance, collateralizedDebt },
          ]) => {
            return {
              asset,
              collateral,
              maturity: Number(maturity),
              pool,
              liquidity,
              bond,
              insurance,
              collateralizedDebt,
            };
          }
        )
      )
    );
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

interface PoolValue {
  pool: Pool;
  liquidity: ERC20Token;
  bond: ERC20Token;
  insurance: ERC20Token;
  collateralizedDebt: Contract;
}
