import {
  Pool,
  Uint112,
  Uint128,
  Uint256,
} from "@timeswap-labs/timeswap-v1-sdk-core";
import { Pool as SDKPool } from "@timeswap-labs/timeswap-v1-sdk";

import { GlobalParams } from "../global";
import { getCurrentTime, getPoolSDK } from "../helper";
import {
  calculateApr,
  calculateCdp,
  calculateMaxValue,
  calculatePercent,
} from "./common";

export async function collateralCalculate(
  gp: GlobalParams,
  app: ElmApp<Ports>,
  pool: Pool,
  query: BorrowQuery
) {
  try {
    const maturity = new Uint256(query.pool.maturity);
    // const now = (await gp.metamaskProvider.getBlock("latest")).timestamp;
    // const currentTime = new Uint256(now);
    const currentTime = getCurrentTime();
    const assetOut = new Uint112(query.assetOut);
    const collateralIn = new Uint112(query.collateralIn!);
    const state = {
      x: new Uint112(query.poolInfo.x),
      y: new Uint112(query.poolInfo.y),
      z: new Uint112(query.poolInfo.z),
    };

    // const sdkPool = getPoolSDK(gp, query.pool.asset, query.pool.collateral, query.pool.maturity, query.chain);
    // const { due, yIncrease } = await sdkPool.calculateBorrowGivenCollateral(
    //   assetOut,
    //   collateralIn,
    //   currentTime
    // );

    const { due, yIncrease } = await pool.borrowGivenCollateral(
      state,
      assetOut,
      collateralIn,
      currentTime
    );
    const debtIn = due.debt.toString();

    const percent = await calculatePercent(
      pool,
      state,
      assetOut,
      yIncrease,
      currentTime
    );

    const timeSlippage = currentTime.sub(60);
    const { due: dueSlippage } = await pool.borrowGivenCollateral(
      state,
      assetOut,
      collateralIn,
      timeSlippage
    );

    // const percent = await calculatePercent(
    //   sdkPool,
    //   state,
    //   assetOut,
    //   yIncrease,
    //   currentTime
    // );

    const maxDebt = calculateMaxValue(
      dueSlippage.debt.sub(query.assetOut),
      query.slippage
    )
      .add(query.assetOut)
      .toString();

    console.log("borrow calc", debtIn, maxDebt);

    const apr = calculateApr(due.debt, query.assetOut, maturity, currentTime);
    const cdp = calculateCdp(
      query.assetOut,
      query.pool.asset.decimals,
      query.poolInfo.assetSpot,
      collateralIn,
      query.pool.collateral.decimals,
      query.poolInfo.assetSpot
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
  console.log("borrow txn", borrow.maxDebt);

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
