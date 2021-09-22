import { WhiteList } from "./whitelist";

export async function pool(app: ElmApp<Ports>, whitelist: WhiteList) {
  for (const { asset, collateral, maturity, pool } of whitelist.poolEntries()) {
    const reserves = await pool.getTotalReserves();
    const assetLiquidity = reserves.asset.value.toString();
    const collateralLiquidity = reserves.collateral.value.toString();
    const apr = await pool.calculateApr();
    const cf = (await pool.calculateCf()).value.toString();

    app.ports.sdkPoolsMsg.send([
      {
        asset,
        collateral,
        maturity,
        assetLiquidity,
        collateralLiquidity,
        apr,
        cf,
      },
    ]);
  }
}
