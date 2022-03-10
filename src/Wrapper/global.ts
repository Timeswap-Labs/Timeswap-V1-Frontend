import { Signer } from "@ethersproject/abstract-signer";
import {
  BaseProvider,
  ExternalProvider,
  JsonRpcFetchFunc,
  Web3Provider,
} from "@ethersproject/providers";
import { providers } from "@0xsequence/multicall";

const { MulticallProvider } = providers;

export class GlobalParams {
  private _provider?: BaseProvider;
  private _providerMulti?: BaseProvider;

  private _walletProvider?: Web3Provider;
  private _walletProviderMulti?: BaseProvider;

  private _walletSigner?: Signer;

  network?: string;

  public get provider(): BaseProvider {
    return this._provider!;
  }

  public set provider(value: BaseProvider) {
    this._provider = value;
    this._providerMulti = new MulticallProvider(this._provider);
  }

  public get providerMulti(): BaseProvider {
    return this._providerMulti!;
  }

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
    this._walletProviderMulti = new MulticallProvider(this._walletProvider);
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
