import { AlchemyProvider, BaseProvider } from "@ethersproject/providers";
import WalletConnectProvider from "@walletconnect/web3-provider";
import { GlobalParams } from "./global";

export declare let window: any;

export async function getProvider(gp: GlobalParams): Promise<BaseProvider | WalletConnectProvider> {
  if (gp.walletProvider) {
    return gp.walletProvider;
  } else {
    const wcProvider = new WalletConnectProvider({
      infuraId: "9aa3d95b3bc440fa88ea12eaa4456161",
      rpc: {
        4: "https://rinkeby.mycustomnode.com",
        80001: "https://rpc-mumbai.matic.today",
      }
    });

    return wcProvider;
  }
}
