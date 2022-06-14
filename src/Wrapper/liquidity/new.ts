import { PoolCore, Uint112, Uint256 } from '@timeswap-labs/timeswap-v1-biconomy-sdk';
import { calculateApr, calculateCdp } from './common';
import { getCurrentTime } from '../helper';


export async function newLiquidityCalculate(
  app: ElmApp<Ports>,
  pool: PoolCore,
  query: NewLiquidityQuery
) {
  try {
    const state = {
      x: new Uint112(0),
      y: new Uint112(0),
      z: new Uint112(0),
    };
    const currentTime = getCurrentTime();
    const assetIn = new Uint112(query.assetIn!);
    const debtIn = new Uint112(query.debtIn!);
    const collateralIn = new Uint112(query.collateralIn!);
    const maturity = new Uint256(query.pool.maturity);

    const liquidityReturn = await pool.newLiquidity(
      state,
      new Uint256(0),
      assetIn,
      debtIn,
      collateralIn,
      currentTime,
      new Uint256(0),
    );

    const apr = calculateApr(debtIn, assetIn, maturity, currentTime);
    const cdp = calculateCdp(
      assetIn,
      query.pool.asset.decimals,
      query.price.assetSpot,
      collateralIn,
      query.pool.collateral.decimals,
      query.price.collateralSpot
    )

    app.ports.receiveNewLiqAnswer.send({
      ...query,
      result: {
        liquidityOut: liquidityReturn.liquidityOut.toString(),
        apr,
        cdp
      },
    });
  } catch (error) {
    console.log("Errrr", error);
  }
}
