import { Signer } from "@ethersproject/abstract-signer";
import {
  BaseProvider,
  ExternalProvider,
  Web3Provider,
} from "@ethersproject/providers";
import WalletConnectProvider from "@walletconnect/web3-provider";
import { providers } from "@0xsequence/multicall";
import { Biconomy } from "@biconomy/mexa";

const { MulticallProvider } = providers;

const API_KEY = "NS0bnWk0U.ff10032a-07a9-478e-835e-a6d0e93b7f86";

export class GlobalParams {
  private _walletProvider?: Web3Provider;
  private _walletProviderMulti?: BaseProvider;
  private _walletSigner?: Signer;
  private _biconomyProvider?: Web3Provider;

  public biconomy?: Biconomy;
  public isBiconomyReady = false;

  network?: string;

  public get walletProvider(): Web3Provider {
    return this._walletProvider!;
  }

  public set walletProvider(
    value: ExternalProvider | WalletConnectProvider | Web3Provider
  ) {
    if (value instanceof Web3Provider) {
      this._walletProvider = value;
    } else {
      this._walletProvider = new Web3Provider(value);
    }

    this.biconomy = new Biconomy(this._walletProvider, {
      walletProvider: value,
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

  public  get walletSigner(): Signer {
    return this._walletSigner!;
  }

  public set walletSigner(value: Signer) {
    this._walletSigner = value;
  }

  public async getSigner(): Promise<Signer> {
    if (this.isBiconomyReady) {
      return this.biconomy.getSignerByAddress(await this.walletSigner.getAddress())
    } else {
      return this._walletSigner!
    }
  }
}
