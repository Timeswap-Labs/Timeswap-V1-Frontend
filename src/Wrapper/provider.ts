import {
  FallbackProvider,
  getDefaultProvider,
  InfuraProvider,
  Provider,
  Web3Provider,
} from "@ethersproject/providers";

export declare let window: any;

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
