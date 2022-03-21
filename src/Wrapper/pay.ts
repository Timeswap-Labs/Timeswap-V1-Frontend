import { Uint112, Uint256 } from "@timeswap-labs/timeswap-v1-sdk-core";
import { GlobalParams } from "./global";
import { getPoolSDK, updateCachedTxns } from "./helper";

export function paySigner(app: ElmApp<Ports>, gp: GlobalParams) {
  app.ports.pay.subscribe(async (params) => {
    const pool = getPoolSDK(
      gp,
      params.send.asset,
      params.send.collateral,
      params.send.maturity,
      params.chain
    );

    try {
      const txnConfirmation = await pool.upgrade(gp.walletSigner!).repay({
        collateralTo: params.send.collateralTo,
        ids: params.send.ids.map((id) => new Uint256(id)),
        maxAssetsIn: params.send.maxAssetsIn.map(
          (maxAssetIn) => new Uint112(maxAssetIn)
        ),
        deadline: new Uint256(params.send.deadline),
      });

      if (txnConfirmation) {
        app.ports.receiveConfirm.send({
          id: params.id,
          chain: params.chain,
          address: params.address,
          hash: txnConfirmation.hash,
        });

        const txnReceipt = await txnConfirmation.wait();
        const receiveReceipt = {
          chain: params.chain,
          address: params.address,
          hash: txnConfirmation.hash,
          state: txnReceipt.status ? "success" : "failed",
        };
        app.ports.receiveReceipt.send(receiveReceipt);
        updateCachedTxns(receiveReceipt);
      }
    } catch (error) {
      app.ports.receiveConfirm.send({
        id: params.id,
        chain: params.chain,
        address: params.address,
        hash: null,
      });
    }
  });
}
