import { Elm } from "../Main.elm";
import rinkeby from "../../whitelist/rinkeby.json";

import images from "../../image/*.svg";
import tokenImages from "../../image/tokens/*.svg";

import { Web3Provider } from "@ethersproject/providers";

import { init } from "./init";

export declare let window: any;

const app = Elm.Main.init({
  node: document.getElementById("elm")!,
  flags: {
    width: window.innerWidth,
    time: Date.now(),
    hasBackdropSupport:
      CSS.supports("-webkit-backdrop-filter: none") ||
      CSS.supports("backdrop-filter: none"),
    images: Object.entries(images),
    tokenImages: Object.entries(tokenImages),
    whitelist: rinkeby,
  },
});

init();

// Test
app.ports.sdkPoolsMsg.send([
  {
    asset: "0xf18f57a842398Aba5420A0405f4D1cf3De8D99Ba",
    collateral: "ETH",
    maturity: 1650889815,
    assetLiquidity: "124123144124",
    collateralLiquidity: "13412312412241",
    apr: 0.0024,
    cf: "1311423134134",
  },
]);

app.ports.connectMetamask.subscribe(() => {
  if (window.ethereum && ethereum) {
    let metamaskProvider = new Web3Provider(window.ethereum);
    let metamaskSigner;

    metamaskProvider
      .send("eth_requestAccounts", [])
      .then((accounts: string[]) => {
        app.ports.metamaskMsg.send({
          chainId: ethereum.chainId,
          user: accounts[0],
        });

        metamaskSigner = metamaskProvider.getSigner();

        app.ports.sdkBalancesMsg.send([
          {
            token: "0xf18f57a842398Aba5420A0405f4D1cf3De8D99Ba",
            balance: "1342345235235",
          },
        ]);
      });

    ethereum.on("chainChanged", (chainId: string) => {
      ethereum
        .request({ method: "eth_requestAccounts" })
        .then((accounts: string[]) => {
          app.ports.metamaskMsg.send({
            chainId,
            user: accounts[0],
          });
        });

      metamaskProvider = new Web3Provider(window.ethereum);
      metamaskSigner = metamaskProvider.getSigner();
      init(metamaskProvider);
    });

    ethereum.on("accountsChanged", (accounts: string[]) => {
      if (accounts[0]) {
        app.ports.metamaskMsg.send({
          chainId: ethereum.chainId,
          user: accounts[0],
        });

        metamaskProvider = new Web3Provider(window.ethereum);
        metamaskSigner = metamaskProvider.getSigner();
        init(metamaskProvider);
      } else {
        app.ports.metamaskMsg.send(null);
      }
    });
  } else {
    app.ports.noMetamask.send();
  }

  //   ethereum.request(request).then((accounts) => {
  //     app.ports.metamaskMsg.send({
  //       chainId: ethereum.chainId,
  //       user: accounts[0],
  //     });
  //   });
  //   ethereum.autoRefreshOnNetworkChange = false;
  //   ethereum.on("accountsChanged", (accounts) => {
  //     if (typeof accounts[0] !== "undefined") {
  //       app.ports.metamaskMsg.send({
  //         chainId: ethereum.chainId,
  //         user: accounts[0],
  //       });
  //     } else {
  //       app.ports.metamaskMsg.send(null);
  //     }
  //   });
  //   ethereum.on("chainChanged", (chainId) => {
  //     ethereum.request({ method: "eth_requestAccounts" }).then((accounts) => {
  //       app.ports.metamaskMsg.send({
  //         chainId: chainId,
  //         user: accounts[0],
  //       });
  //     });
  //   });
  //     app.ports.call.subscribe(request => {
  //         ethereum
  //             .request({ method: "eth_requestAccounts" })
  //             .then(accounts => {
  //                 ethereum
  //                     .request(request.call)
  //                     .then(response => {
  //                         app.ports.response.send({
  //                             chainId: ethereum.chainId,
  //                             user: accounts[0],
  //                             id: request.id,
  //                             result: response
  //                         })
  //                     })
  //             })
  //     })
  //     app.ports.getETH.subscribe(async(request) => {})
  //     app.ports.sendTransaction.subscribe(request => {
  //         ethereum.request(request)
  //     })
  //     app.ports.faucet.subscribe(request => {
  //         ethereum
  //             .request(request.sendTransaction)
  //             .then(txHaxh => {
  //                 ethereum.request(request.watchAsset)
  //             })
  //     })
});
