import { WhiteList } from "./whitelist";

export async function pool(app: ElmApp<Ports>, whitelist: WhiteList) {
  for (const [key, { pool }] of whitelist.poolEntries()) {
    const { asset, collateral, maturity } = JSON.parse(key);
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
