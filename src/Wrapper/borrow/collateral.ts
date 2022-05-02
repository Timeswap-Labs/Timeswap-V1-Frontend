import { Pool, Uint112, Uint256 } from "@timeswap-labs/timeswap-v1-sdk-core";
import { Pool as SDKPool } from "@timeswap-labs/timeswap-v1-sdk";

import { GlobalParams } from "../global";
import { calculateMaxValue, getCurrentTime } from "../helper";
import {
  calculateApr,
  calculateCdp,
  calculateFuturisticApr,
  calculateFuturisticCdp,
  calculateHelper,
  calculatePercent,
  percentMinMaxValues,
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

    const { yMin, yMax, dueMin, dueMax } = percentMinMaxValues(
      pool,
      state,
      assetOut,
      currentTime
    );

    // Collateral too high check
    if (collateralIn.gt(dueMin.collateral)) {
      app.ports.receiveBorrowAnswer.send({
        ...query,
        result: 4,
      });

      return;
    }

    // Collateral too low check
    if (collateralIn.lt(dueMax.collateral)) {
      app.ports.receiveBorrowAnswer.send({
        ...query,
        result: 3,
      });

      return;
    }

    const {
      dueOut,
      assetOut: assetOutReturn,
      xDecrease,
      yIncrease,
      zIncrease,
    } = pool.borrowGivenCollateral(state, assetOut, collateralIn, currentTime);
    const debtIn = dueOut.debt.toString();
    const txnFee = xDecrease.sub(assetOutReturn).toString();
    const percent = calculatePercent(yMin, yMax, yIncrease);

    const timeSlippageBefore = currentTime.sub(60);
    const timeSlippageAfter =
      currentTime.add(3 * 60).toBigInt() >= maturity.toBigInt()
        ? maturity.sub(1)
        : currentTime.add(3 * 60);

    const { yIncrease: yIncreaseBefore } = pool.borrowGivenCollateral(
      state,
      assetOut,
      collateralIn,
      timeSlippageBefore
    );
    const { xDecrease: xDecreaseAfter } = pool.borrowGivenCollateral(
      state,
      assetOut,
      collateralIn,
      timeSlippageAfter
    );

    const interestBefore = calculateHelper(
      maturity,
      timeSlippageBefore,
      yIncreaseBefore
    );

    const principalAfter = xDecreaseAfter;

    const maxDebt = calculateMaxValue(interestBefore, query.slippage)
      .add(principalAfter)
      .toString();

    const apr = calculateApr(
      dueOut.debt,
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
        debtIn,
        maxDebt,
        apr,
        cdp,
        futureApr,
        futureCdp,
        txnFee,
      },
    });
  } catch (err) {
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
  return await pool.upgrade(gp.walletSigner!).borrowGivenCollateral({
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
