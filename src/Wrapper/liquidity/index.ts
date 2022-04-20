import { getPool } from '../helper';
import { assetCalculate } from './asset';
import { collateralCalculate } from './collateral';
import { debtCalculate } from './debt';

export function liquidity(app: ElmApp<Ports>) {
  app.ports.queryLiquidity.subscribe((query) => {
    liquidityQueryCalculation(app, query)
  });

  app.ports.queryLiquidityPerSecond.subscribe((query) =>
    liquidityQueryCalculation(app, query)
  );
}

async function liquidityQueryCalculation(
  app: ElmApp<Ports>,
  query: LiquidityQuery,
) {
  // Asset Overflow check
  // try {
  //   const assetIn = new Uint112(query.assetIn);
  //   const stateX = new Uint112(query.poolInfo.x);
  //   stateX.add(assetIn);
  // } catch (error) {
  //   app.ports.receiveLendAnswer.send({
  //     ...query,
  //     result: 5,
  //   });
  // }

  const pool = getPool(query);
  if (query.assetIn !== undefined) {
    assetCalculate(app, pool, query);
  } else if (query.debtIn !== undefined) {
    await debtCalculate(app, pool, query);
  } else if (query.collateralIn !== undefined) {
    await collateralCalculate(app, pool, query);
  }
}
