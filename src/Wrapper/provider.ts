import {
  AlchemyProvider,
  BaseProvider,
  FallbackProvider,
  Web3Provider,
} from "@ethersproject/providers";
import { GlobalParams } from "./global";

export declare let window: any;

export async function getProvider(gp: GlobalParams): Promise<BaseProvider> {
  if (gp.metamaskProvider && gp.network === "0x4") {
    const metamaskProvider = new Web3Provider(window.ethereum, "rinkeby");
    const metamaskConfig = {
      provider: metamaskProvider,
      priority: 1,
      stallTimeout: 100,
      weight: 1,
    };

    const alchemyProvider = AlchemyProvider.getWebSocketProvider(
      // await metamaskProvider.getNetwork(),
      "rinkeby",
      "FtqSyYbn24pFMUERB6wwZAqiPer7Q83Q"
    );
    const alchemyConfig = {
      provider: alchemyProvider,
      priority: 2,
      stallTimeout: 500,
      weight: 1,
    };

    return new FallbackProvider([metamaskConfig, alchemyConfig]);
  } else {
    const alchemyProvider = AlchemyProvider.getWebSocketProvider(
      // await metamaskProvider.getNetwork(),
      "rinkeby",
      "FtqSyYbn24pFMUERB6wwZAqiPer7Q83Q"
    );

    return alchemyProvider;
  }
}
