import {
  Pool,
  Uint112,
  Uint256,
} from "@timeswap-labs/timeswap-v1-sdk-core";
import { Pool as SDKPool } from "@timeswap-labs/timeswap-v1-sdk";

import { GlobalParams } from "../global";
import { getCurrentTime } from "../helper";
import {
  calculateApr,
  calculateCdp,
  calculateMaxValue,
  calculatePercent,
} from "./common";

export function collateralCalculate(
  app: ElmApp<Ports>,
  pool: Pool,
  query: BorrowQuery
) {
  try {
    const maturity = new Uint256(query.pool.maturity);
    const currentTime = getCurrentTime();
    const assetOut = new Uint112(query.assetOut);
    const collateralIn = new Uint112(query.collateralIn!);
    const state = {
      x: new Uint112(query.poolInfo.x),
      y: new Uint112(query.poolInfo.y),
      z: new Uint112(query.poolInfo.z),
    };

    const { dueOut, yIncrease } = pool.borrowGivenCollateral(
      state,
      assetOut,
      collateralIn,
      currentTime
    );
    const debtIn = dueOut.debt.toString();

    const percent = calculatePercent(
      pool,
      state,
      assetOut,
      yIncrease,
      currentTime
    );

    const timeSlippage = currentTime.sub(60);
    const { dueOut: dueSlippage } = pool.borrowGivenCollateral(
      state,
      assetOut,
      collateralIn,
      timeSlippage
    );

    const maxDebt = calculateMaxValue(
      dueSlippage.debt.sub(query.assetOut),
      query.slippage
    )
      .add(query.assetOut)
      .toString();

    const apr = calculateApr(dueOut.debt, query.assetOut, maturity, currentTime);
    const cdp = calculateCdp(
      query.assetOut,
      query.pool.asset.decimals,
      query.poolInfo.assetSpot,
      collateralIn,
      query.pool.collateral.decimals,
      query.poolInfo.collateralSpot
    );

    query.pool.maturity = query.pool.maturity.toString();

    app.ports.receiveBorrowAnswer.send({
      ...query,
      result: {
        percent: Number(percent.toBigInt()),
        debtIn,
        maxDebt,
        apr,
        cdp,
      },
    });
  } catch {
    query.pool.maturity = query.pool.maturity.toString();
    app.ports.receiveBorrowAnswer.send({
      ...query,
      result: 0,
    });
  }
}

export async function collateralTransaction(
  pool: SDKPool,
  gp: GlobalParams,
  borrow: Borrow
) {
  console.log("BGC", borrow);

  return await pool.upgrade(gp.metamaskSigner!).borrowGivenCollateral({
    assetTo: borrow.assetTo,
    dueTo: borrow.dueTo,
    assetOut: new Uint112(borrow.assetOut),
    collateralIn: new Uint112(borrow.collateralIn),
    maxDebt: new Uint112(borrow.maxDebt),
    deadline: new Uint256(borrow.deadline),
  });
}

interface Borrow {
  assetTo: string;
  dueTo: string;
  assetOut: string;
  collateralIn: string;
  maxDebt: string;
  deadline: number;
}
