import { Uint112 } from "@timeswap-labs/timeswap-v1-sdk-core";
import { GlobalParams } from "./global";
import { getPoolSDK } from "./helper";

export function withdrawSigner(
  app: ElmApp<Ports>,
  gp: GlobalParams
) {
  app.ports.withdraw.subscribe(async (params) => {
    const pool = getPoolSDK(gp, params.send.asset, params.send.collateral, params.send.maturity, params.chain);

    const txn = await pool.upgrade(gp.metamaskSigner!).collect({
      assetTo: params.send.assetTo,
      collateralTo: params.send.collateralTo,
      claimsIn: {
        bondInterest: new Uint112(params.send.claimsIn.bondInterest),
        bondPrincipal: new Uint112(params.send.claimsIn.bondPrincipal),
        insuranceInterest: new Uint112(params.send.claimsIn.insuranceInterest),
        insurancePrincipal: new Uint112(params.send.claimsIn.insurancePrincipal)
      },
    });

    await txn.wait();
  });
}
