import { Signer } from "@ethersproject/abstract-signer";
import { Web3Provider, Provider } from "@ethersproject/providers";

export class GlobalParams {
  provider?: Provider;

  metamaskProvider?: Web3Provider;
  metamaskSigner?: Signer;

  constructor(
    provider?: Provider,
    metamaskProvider?: Web3Provider,
    metamaskSigner?: Signer
  ) {
    this.provider = provider;

    this.metamaskProvider = metamaskProvider;
    this.metamaskSigner = metamaskSigner;
  }
}
