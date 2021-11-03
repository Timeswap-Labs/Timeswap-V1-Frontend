import { AlchemyProvider, BaseProvider } from "@ethersproject/providers";
import { GlobalParams } from "./global";

export declare let window: any;

export async function getProvider(gp: GlobalParams): Promise<BaseProvider> {
  if (gp.metamaskProvider && gp.network === "0x4") {
    return gp.metamaskProvider;
  } else {
    const alchemyProvider = AlchemyProvider.getWebSocketProvider(
      // await metamaskProvider.getNetwork(),
      "rinkeby",
      "FtqSyYbn24pFMUERB6wwZAqiPer7Q83Q"
    );

    return alchemyProvider;
  }
}
