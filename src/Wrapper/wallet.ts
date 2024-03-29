import WalletConnectProvider from "@walletconnect/web3-provider";
import { Web3Provider } from "@ethersproject/providers";

type Wallet = { [name: string]: Web3Provider | WalletConnectProvider | null };

export declare let window: any;

export const wallet: Wallet = {
  metamask: window.ethereum || null,
  walletConnect: new WalletConnectProvider({
    infuraId: "9aa3d95b3bc440fa88ea12eaa4456161",
    rpc: {
      137: "https://rpc-mainnet.matic.network",
      80001: "https://rpc-mumbai.matic.today",
    }})
};
