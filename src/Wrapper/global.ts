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

  private _metamask?: Web3Provider;
  private _metamaskProvider?: BaseProvider;
  private _metamaskSigner?: Signer;

  public get provider(): BaseProvider {
    return this._provider!;
  }
  public set provider(value: BaseProvider) {
    this._provider = new MulticallProvider(value);
  }

  public get metamask(): Web3Provider {
    return this._metamask!;
  }
  public set metamask(
    value: ExternalProvider | JsonRpcFetchFunc | Web3Provider
  ) {
    if (value instanceof Web3Provider) {
      this._metamask = value;
    } else {
      this._metamask = new Web3Provider(value);
      this._metamaskProvider = new MulticallProvider(this._metamask);
    }
  }

  public get metamaskProvider(): BaseProvider {
    return this._metamaskProvider!;
  }
  public set metamaskProvider(
    value: ExternalProvider | JsonRpcFetchFunc | BaseProvider
  ) {
    if (value instanceof BaseProvider) {
      this._metamaskProvider = new MulticallProvider(value);
    } else {
      this._metamask = new Web3Provider(value);
      this._metamaskProvider = new MulticallProvider(this._metamask);
    }
  }

  public get metamaskSigner(): Signer {
    return this._metamaskSigner!;
  }
  public set metamaskSigner(value: Signer) {
    this._metamaskSigner = value;
  }
}
