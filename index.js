import { Elm } from "./src/Main.elm";
import images from "./image/*.svg";
import tokenImages from "./image/tokens/*.svg";
import rinkeby from "./whitelist/rinkeby.json";

var app = Elm.Main.init({
  node: document.getElementById("elm"),
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

app.ports.connectMetamask.subscribe(() => {
  if (typeof window.ethereum !== "undefined") {
    ethereum.request({ method: "eth_requestAccounts" }).then((accounts) => {
      app.ports.metamaskMsg.send({
        chainId: ethereum.chainId,
        user: accounts[0],
      });
    });

    ethereum.autoRefreshOnNetworkChange = false;

    ethereum.on("accountsChanged", (accounts) => {
      if (typeof accounts[0] !== "undefined") {
        app.ports.metamaskMsg.send({
          chainId: ethereum.chainId,
          user: accounts[0],
        });
      } else {
        app.ports.metamaskMsg.send(null);
      }
    });

    ethereum.on("chainChanged", (chainId) => {
      ethereum.request({ method: "eth_requestAccounts" }).then((accounts) => {
        app.ports.metamaskMsg.send({
          chainId: chainId,
          user: accounts[0],
        });
      });
    });

    app.ports.disconnect.subscribe(() => {
      app.ports.metamaskMsg.send(null);
    });

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
  } else {
    app.ports.noMetamask.send();
  }
});
