import {
  BaseProvider,
  FallbackProvider,
  InfuraProvider,
  Web3Provider,
} from "@ethersproject/providers";

export declare let window: any;

export async function getProvider(
  provider?: BaseProvider
): Promise<BaseProvider> {
  if (window.ethereum) {
    // const metamaskProvider = new Web3Provider(window.ethereum, "rinkeby");
    // const metamaskConfig = {
    //   provider: metamaskProvider,
    //   priority: 1,
    //   stallTimeout: 0,
    //   weight: 1,
    // };

    const infuraProvider = new InfuraProvider(
      // await metamaskProvider.getNetwork(),
      "rinkeby",
      "ccbd29f5d62547e1ac0cde02f1366cb5"
    );
    const infuraConfig = {
      provider: infuraProvider,
      priority: 1,
      stallTimeout: 8,
      weight: 1,
    };

    return new FallbackProvider([infuraConfig]);
  } else if (provider) {
    const infuraProvider = new InfuraProvider(
      // await provider.getNetwork(),
      "rinkeby",
      "ccbd29f5d62547e1ac0cde02f1366cb5"
    );
    const infuraConfig = {
      provider: infuraProvider,
      priority: 1,
      stallTimeout: 8,
      weight: 1,
    };

    return new FallbackProvider([infuraConfig]);
  } else {
    // return getDefaultProvider("rinkeby");

    const infuraProvider = new InfuraProvider(
      // await provider.getNetwork(),
      "rinkeby",
      "ccbd29f5d62547e1ac0cde02f1366cb5"
    );
    const infuraConfig = {
      provider: infuraProvider,
      priority: 1,
      stallTimeout: 8,
      weight: 1,
    };

    return new FallbackProvider([infuraConfig]);
  }
}
