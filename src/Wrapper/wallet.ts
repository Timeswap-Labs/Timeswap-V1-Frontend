import { Web3Provider } from "@ethersproject/providers";

export declare let window: any;

export const wallet: Wallet = { metamask: new Web3Provider(window.ethereum) };

type Wallet = { [name: string]: Web3Provider };
