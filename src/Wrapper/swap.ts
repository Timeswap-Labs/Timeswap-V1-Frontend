import { GlobalParams } from "./global";

export function swapSigner(
  app: ElmApp<Ports>,
  gp: GlobalParams
) {
  app.ports.signSwapTxn.subscribe(async () => {
    try {
      const signedMsg = await gp.walletSigner.signMessage("I love TimeSwap");

      app.ports.swapSignatureMsg.send(signedMsg);
    } catch (error) {
      // if (error.code === 4001) {
      //   // user rejected the signing of message
      // }
    }
  });
}
