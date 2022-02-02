import { Uint112 } from "@timeswap-labs/timeswap-v1-sdk-core";
import { GlobalParams } from "./global";
import { getPoolSDK } from "./helper";

export function withdrawSigner(
  app: ElmApp<Ports>,
  gp: GlobalParams
) {
  app.ports.withdraw.subscribe(async (params) => {
    const pool = getPoolSDK(gp, params.send.asset, params.send.collateral, params.send.maturity, params.chain);

    try {
      const txnConfirmation = await pool.upgrade(gp.metamaskSigner!).collect({
        assetTo: params.send.assetTo,
        collateralTo: params.send.collateralTo,
        claimsIn: {
          bondInterest: new Uint112(params.send.claimsIn.bondInterest),
          bondPrincipal: new Uint112(params.send.claimsIn.bondPrincipal),
          insuranceInterest: new Uint112(params.send.claimsIn.insuranceInterest),
          insurancePrincipal: new Uint112(params.send.claimsIn.insurancePrincipal)
        },
      });

      if (txnConfirmation) {
        app.ports.receiveConfirm.send({
          id: params.id,
          chain: params.chain,
          address: params.address,
          hash: txnConfirmation.hash
        });

        await txnConfirmation.wait();
      }
    } catch (error) {
      app.ports.receiveConfirm.send({
        id: params.id,
        chain: params.chain,
        address: params.address,
        hash: null
      });
    }
  });
}
