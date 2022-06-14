import { PoolCore, Uint112, Uint256 } from "@timeswap-labs/timeswap-v1-biconomy-sdk";
import { Pool as SDKPool } from "@timeswap-labs/timeswap-v1-biconomy-sdk";
import { GlobalParams } from "../global";
import { calculateMaxValue, getCurrentTime } from "../helper";
import {
  calculateApr,
  calculateCdp,
  calculateFuturisticApr,
  calculateFuturisticCdp,
  calculatePercent,
  percentMinMaxValues,
} from "./common";

export function debtCalculate(
  app: ElmApp<Ports>,
  pool: PoolCore,
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

    const { yMin, yMax, dueMin, dueMax } = percentMinMaxValues(
      pool,
      state,
      assetOut,
      currentTime
    );

    // Debt too low check
    if (debtIn.lt(dueMin.debt)) {
      app.ports.receiveBorrowAnswer.send({
        ...query,
        result: 1,
      });

      return;
    }

    // Debt too high check
    if (debtIn.gt(dueMax.debt)) {
      app.ports.receiveBorrowAnswer.send({
        ...query,
        result: 2,
      });

      return;
    }

    const {
      dueOut,
      assetOut: assetOutReturn,
      xDecrease,
      yIncrease,
      zIncrease,
    } = pool.borrowGivenDebt(state, assetOut, debtIn, currentTime);

    const collateralIn = dueOut.collateral.toString();
    const txnFee = xDecrease.sub(assetOutReturn).toString();
    const percent = calculatePercent(yMin, yMax, yIncrease);
    const timeSlippageBefore = currentTime.sub(60);

    const { dueOut: dueSlippageBefore } = pool.borrowGivenDebt(
      state,
      assetOut,
      debtIn,
      timeSlippageBefore
    );

    const maxCollateral = calculateMaxValue(
      dueSlippageBefore.collateral,
      query.slippage
    ).toString();

    const apr = calculateApr(
      debtIn,
      xDecrease.toString(),
      maturity,
      currentTime
    );
    const cdp = calculateCdp(
      query.assetOut,
      query.pool.asset.decimals,
      query.poolInfo.assetSpot,
      collateralIn,
      query.pool.collateral.decimals,
      query.poolInfo.collateralSpot
    );

    const futureApr = calculateFuturisticApr(state, xDecrease, yIncrease);
    const futureCdp = calculateFuturisticCdp(
      state,
      query.pool.asset.decimals,
      query.pool.collateral.decimals,
      xDecrease,
      zIncrease,
      query.poolInfo.assetSpot,
      query.poolInfo.collateralSpot
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
        futureApr,
        futureCdp,
        txnFee,
      },
    });
  } catch (err) {
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
  return await pool.upgrade(gp.walletSigner!).borrowGivenDebt({
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
