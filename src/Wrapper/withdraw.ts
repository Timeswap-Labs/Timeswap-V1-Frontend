import { Uint128 } from "@timeswap-labs/timeswap-v1-sdk-core";
import { GlobalParams } from "./global";
import { WhiteList } from "./whitelist";

export function withdrawSigner(
  app: ElmApp<Ports>,
  whitelist: WhiteList,
  gp: GlobalParams
) {
  app.ports.withdraw.subscribe(async (params) => {
    const pool = whitelist.getPool(
      params.asset,
      params.collateral,
      params.maturity
    );

    const txn = await pool.upgrade(gp.metamaskSigner!).collect({
      assetTo: params.assetTo,
      collateralTo: params.collateralTo,
      claimsIn: {
        bond: new Uint128(params.claimsIn.bond),
        insurance: new Uint128(params.claimsIn.insurance),
      },
    });

    await txn.wait();
  });
}
