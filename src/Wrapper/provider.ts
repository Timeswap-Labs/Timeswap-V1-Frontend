import {
  AlchemyProvider,
  BaseProvider,
  FallbackProvider,
  InfuraProvider,
  Web3Provider,
} from "@ethersproject/providers";
import { GlobalParams } from "./global";

export declare let window: any;

export async function getProvider(gp: GlobalParams): Promise<BaseProvider> {
  if (gp.metamaskProvider && gp.network === "0x4") {
    return new Web3Provider(window.ethereum, "rinkeby");
  } else {
    const alchemyProvider = new AlchemyProvider(
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

    const infuraProvider = new InfuraProvider(
      // await metamaskProvider.getNetwork(),
      "rinkeby",
      "ccbd29f5d62547e1ac0cde02f1366cb5"
    );
    const infuraConfig = {
      provider: infuraProvider,
      priority: 3,
      stallTimeout: 1000,
      weight: 1,
    };

    return new FallbackProvider([alchemyConfig, infuraConfig]);
  }
}
