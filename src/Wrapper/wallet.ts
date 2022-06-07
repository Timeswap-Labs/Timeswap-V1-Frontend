import WalletConnectProvider from "@walletconnect/web3-provider";
import { Web3Provider } from "@ethersproject/providers";
import { Biconomy } from "@biconomy/mexa";


type Wallet = { [name: string]: Web3Provider | null };

export declare let window: any;

export const wallet: Wallet = {
  metamask: window.ethereum ? new Web3Provider(window.ethereum, "any") : null,
  walletConnect: new Web3Provider(new WalletConnectProvider({
    infuraId: "9aa3d95b3bc440fa88ea12eaa4456161",
    rpc: {
      137: "https://rpc-mainnet.matic.network",
      80001: "https://rpc-mumbai.matic.today",
    }}))
};
