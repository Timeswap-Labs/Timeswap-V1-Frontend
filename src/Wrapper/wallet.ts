import { Web3Provider } from "@ethersproject/providers";

export declare let window: any;

export const wallet: Wallet = {
  metamask: window.ethereum ? new Web3Provider(window.ethereum) : null,
};

type Wallet = { [name: string]: Web3Provider | null };
