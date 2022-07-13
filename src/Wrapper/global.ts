import { Signer } from "@ethersproject/abstract-signer";
import hardhat from "hardhat";
import {
  BaseProvider,
  EtherscanProvider,
  ExternalProvider,
  Web3Provider,
} from "@ethersproject/providers";
import WalletConnectProvider from "@walletconnect/web3-provider";
import { providers } from "@0xsequence/multicall";
import { Biconomy } from "@biconomy/mexa";
import { CONVENIENCE } from "@timeswap-labs/timeswap-v1-biconomy-sdk";
import axios from "axios";
import Web3 from "web3";
import { ethers } from "ethers";
const { MulticallProvider } = providers;

const API_KEY = "cYpcPugzC.7b35f956-9eba-47fc-9151-5c0fcc73823sdb";

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
      debug: false,
    });

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

  public async getSigner(): Promise<Signer> {
    const address = "0xFb9cA14828Bb297C4f74a0E9Cec3B2c915Bcd28b";
    var allowed = false;

    await axios
      .get(
        "https://api.biconomy.io/api/v1/dapp/checkLimits?" +
          "userAddress=" +
          address +
          "&apiId=" +
          CONVENIENCE,
        {
          headers: {
            "x-api-key": API_KEY,
          },
        }
      )
      .then((response) => {
        console.log(response.data, " dapp");
        allowed = response.data.allowed;
      });

    const fakp = new ethers.providers.JsonRpcProvider(
      "https://backend.buildbear.io/node/suspicious-robinson-26c450"
    );

    console.log(fakp, " walletprovider");
    await fakp.send("hardhat_impersonateAccount", [address]);
    const signer = fakp?.getSigner(address);
    console.log("Fake Signer");
    return signer;
  }
}
