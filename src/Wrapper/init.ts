import rinkeby from "../../whitelist/rinkeby.json";

import {
  FallbackProvider,
  InfuraProvider,
  Provider,
  Web3Provider,
  getDefaultProvider,
} from "@ethersproject/providers";
import { Pair, ERC20Token } from "@timeswap-labs/timeswap-v1-sdk";

import "regenerator-runtime/runtime";

export declare let window: any;

export async function init(providerParam?: Provider) {
  const provider = await getProvider(providerParam);
  console.log(provider);

  // rinkeby.pairs.map((pairParams) => {
  //   const asset = new ERC20Token(provider, 2, 18, pairParams.asset);
  //   const collateral = new ERC20Token(provider, 2, 18, pairParams.collateral);

  //   const pair = new Pair(provider, asset, collateral, rinkeby.convenience);

  //   //   return pair.
  // });
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

    const infuraProvider = new InfuraProvider(
      // await metamaskProvider.getNetwork(),
      "rinkeby",
      "099fc58e0de9451d80b18d7c74caa7c1"
    );
    const infuraConfig = {
      provider: infuraProvider,
      priority: 1,
      stallTimeout: 8,
      weight: 1,
    };

    return new FallbackProvider([metamaskConfig, infuraConfig]);
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
