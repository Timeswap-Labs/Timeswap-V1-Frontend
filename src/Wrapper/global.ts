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

  private _metamaskProvider?: Web3Provider;
  private _metamaskProviderMulti?: BaseProvider;

  private _metamaskSigner?: Signer;

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

  public get metamaskProvider(): Web3Provider {
    return this._metamaskProvider!;
  }

  public set metamaskProvider(
    value: ExternalProvider | JsonRpcFetchFunc | Web3Provider
  ) {
    if (value instanceof Web3Provider) {
      this._metamaskProvider = value;
    } else {
      this._metamaskProvider = new Web3Provider(value);
    }
    this._metamaskProviderMulti = new MulticallProvider(this._metamaskProvider);
  }

  public get metamaskProviderMulti(): BaseProvider {
    return this._metamaskProviderMulti!;
  }

  public get metamaskSigner(): Signer {
    return this._metamaskSigner!;
  }

  public set metamaskSigner(value: Signer) {
    this._metamaskSigner = value;
  }
}
