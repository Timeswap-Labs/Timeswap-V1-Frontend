import { Pool, Uint112, Uint256 } from "@timeswap-labs/timeswap-v1-sdk-core";
import { Pool as SDKPool } from "@timeswap-labs/timeswap-v1-sdk";
import { GlobalParams } from "../global";
import { getCurrentTime, getPoolSDK } from "../helper";
import {
  calculateApr,
  calculateCdp,
  calculateMaxValue,
  calculatePercent,
} from "./common";

export async function debtCalculate(
  gp: GlobalParams,
  app: ElmApp<Ports>,
  pool: Pool,
  query: BorrowQuery
) {
  try {
    const maturity = new Uint256(query.pool.maturity);
    const currentTime = getCurrentTime();
    const assetOut = new Uint112(query.assetOut);
    const debtIn = new Uint112(query.debtIn!);
    const state = {
      x: new Uint112(query.poolInfo.x),
      y: new Uint112(query.poolInfo.y),
      z: new Uint112(query.poolInfo.z),
    };

    // const sdkPool = getPoolSDK(gp, query.pool.asset, query.pool.collateral, query.pool.maturity, query.chain);
    // const { due, yIncrease } = await sdkPool.calculateBorrowGivenDebt(
    //   assetOut,
    //   debtIn,
    //   currentTime
    // );

    const sdkPool = getPoolSDK(gp, query.pool.asset, query.pool.collateral, query.pool.maturity, query.chain);
    const { due, yIncrease } = await sdkPool.calculateBorrowGivenDebt(
      assetOut,
      debtIn,
      currentTime
    );

    // const { due, yIncrease } = await pool.borrowGivenDebt(
    //   state,
    //   assetOut,
    //   debtIn,
    //   currentTime
    // );

    const collateralIn = due.collateral.toString();

    const percent = await calculatePercent(
      sdkPool,
      state,
      assetOut,
      yIncrease,
      currentTime
    );

    const timeSlippage = currentTime.sub(60);
    const { due: dueSlippage } = await pool.borrowGivenDebt(
      state,
      assetOut,
      debtIn,
      timeSlippage
    );

    const maxCollateral = calculateMaxValue(
      dueSlippage.collateral,
      query.slippage
    ).toString();

    const apr = calculateApr(debtIn, query.assetOut, maturity, currentTime);
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
        collateralIn,
        maxCollateral,
        apr,
        cdp,
      },
    });
  } catch {
    app.ports.receiveBorrowAnswer.send({
      ...query,
      result: 0,
    });
  }
}

export async function debtTransaction(
  pool: SDKPool,
  gp: GlobalParams,
  borrow: Borrow
) {
  return await pool.upgrade(gp.metamaskSigner!).borrowGivenDebt({
    assetTo: borrow.assetTo,
    dueTo: borrow.dueTo,
    assetOut: new Uint112(borrow.assetOut),
    debtIn: new Uint112(borrow.debtIn),
    maxCollateral: new Uint112(borrow.maxCollateral),
    deadline: new Uint256(borrow.deadline),
  });
}

interface Borrow {
  assetTo: string;
  dueTo: string;
  assetOut: string;
  debtIn: string;
  maxCollateral: string;
  deadline: number;
}
