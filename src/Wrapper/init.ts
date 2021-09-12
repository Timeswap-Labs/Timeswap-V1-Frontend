import rinkeby from "../../whitelist/rinkeby.json";

import {
  FallbackProvider,
  InfuraProvider,
  Provider,
  Web3Provider,
  getDefaultProvider,
  Network,
} from "@ethersproject/providers";
import { Pool, ERC20Token, NativeToken } from "@timeswap-labs/timeswap-v1-sdk";

import "regenerator-runtime/runtime";
import { Uint256 } from "@timeswap-labs/timeswap-v1-sdk-core";

export declare let window: any;

function getToken(
  address: string,
  provider: Provider,
  network: Network
): ERC20Token | NativeToken {
  if (address === "ETH") return new NativeToken(provider, network.chainId, 18);
  else return new ERC20Token(provider, network.chainId, 18, address);
}

export async function init(app: ElmApp<Ports>, providerParam?: Provider) {
  const provider = await getProvider(providerParam);
  console.log(provider);

  const network = await provider.getNetwork();

  const params = await Promise.all(
    rinkeby.pairs
      .flatMap((pairParams) => {
        const asset = getToken(pairParams.asset, provider, network);
        const collateral = getToken(pairParams.collateral, provider, network);

        return pairParams.pools.map((poolParams) => {
          return {
            asset: pairParams.asset,
            collateral: pairParams.collateral,
            pool: new Pool(
              provider,
              asset,
              collateral,
              new Uint256(poolParams.maturity),
              rinkeby.convenience,
              pairParams.pair
            ),
          };
        });
      })
      .map(async ({ asset, collateral, pool }) => {
        // const reserves = await pool.getTotalReserves();
        // const apr = await pool.calculateApr();
        // const cf = await pool.calculateCf();

        // return {
        //   asset: asset,
        //   collateral: collateral,
        //   maturity: Number(pool.maturity.value),
        //   assetLiquidity: reserves.asset.value.toString(),
        //   collateralLiquidity: reserves.collateral.value.toString(),
        //   apr: Number(apr.value),
        //   cf: cf.value.toString(),
        // };

        return {
          asset: asset,
          collateral: collateral,
          maturity: Number(pool.maturity.value),
          assetLiquidity: "12988437928347923874",
          collateralLiquidity: "9875498792387403284",
          apr: 2.34,
          cf: "7387983",
        };
      })
  );

  app.ports.sdkPoolsMsg.send(params);
}

export async function getProvider(provider?: Provider): Promise<Provider> {
  if (window.ethereum) {
    const metamaskProvider = new Web3Provider(window.ethereum, "rinkeby");
    const metamaskConfig = {
      provider: metamaskProvider,
      priority: 1,
      stallTimeout: 0,
      weight: 1,
    };

    // const infuraProvider = new InfuraProvider(
    //   // await metamaskProvider.getNetwork(),
    //   "rinkeby",
    //   "099fc58e0de9451d80b18d7c74caa7c1"
    // );
    // const infuraConfig = {
    //   provider: infuraProvider,
    //   priority: 1,
    //   stallTimeout: 8,
    //   weight: 1,
    // };

    return new FallbackProvider([metamaskConfig]);
  } else if (provider) {
    const infuraProvider = new InfuraProvider(
      await provider.getNetwork(),
      "099fc58e0de9451d80b18d7c74caa7c1"
    );
    const infuraConfig = {
      provider: infuraProvider,
      priority: 1,
      stallTimeout: 8,
      weight: 1,
    };

    return new FallbackProvider([infuraConfig]);
  } else {
    return getDefaultProvider("rinkeby");
  }
}
