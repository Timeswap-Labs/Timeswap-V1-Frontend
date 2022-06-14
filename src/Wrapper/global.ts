import { Signer } from "@ethersproject/abstract-signer";
import {
  BaseProvider,
  ExternalProvider,
  JsonRpcFetchFunc,
  Web3Provider,
} from "@ethersproject/providers";
import { providers } from "@0xsequence/multicall";
import { Biconomy } from "@biconomy/mexa";

const { MulticallProvider } = providers;

const API_KEY = "oeTYJXHrB.d0b9a0de-030f-48dc-adb6-7b3ac9d06722";

export class GlobalParams {
  private _walletProvider?: Web3Provider;
  private _walletProviderMulti?: BaseProvider;
  private _walletSigner?: Signer;
  private _biconomyProvider?: Web3Provider;

  public biconomy?: Biconomy;

  network?: string;

  public get walletProvider(): Web3Provider {
    return this._walletProvider!;
  }

  public set walletProvider(
    value: ExternalProvider | JsonRpcFetchFunc | Web3Provider
  ) {
    if (value instanceof Web3Provider) {
      this._walletProvider = value;
    } else {
      this._walletProvider = new Web3Provider(value);
    }

    this.biconomy = new Biconomy(this._walletProvider, {
      walletProvider: window.ethereum,
      apiKey: API_KEY,
      debug: true,
    });

    // Handle the error

    this._biconomyProvider = this.biconomy.getEthersProvider();
    this._walletProviderMulti = new MulticallProvider(this._walletProvider);
  }

  public get biconomyProvider(): Web3Provider {
    return this._biconomyProvider!;
  }

  public get walletProviderMulti(): BaseProvider {
    return this._walletProviderMulti!;
  }

  public get walletSigner(): Signer {
    return this._walletSigner!;
  }

  public set walletSigner(value: Signer) {
    this._walletSigner = value;
  }
}
