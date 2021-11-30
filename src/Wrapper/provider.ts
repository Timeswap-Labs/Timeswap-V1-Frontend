import { BaseProvider, JsonRpcProvider } from "@ethersproject/providers";
import { GlobalParams } from "./global";

export declare let window: any;

export async function getProvider(gp: GlobalParams): Promise<BaseProvider> {
  if (gp.metamaskProvider && gp.network === "0x4") {
    return gp.metamaskProvider;
  } else {
    const quicknodeProvider = new JsonRpcProvider("https://muddy-solitary-tree.rinkeby.quiknode.pro/3773ee69a4e703ddcc82f84b5a66c2bdb33af89f/")
    return quicknodeProvider;
  }
}
