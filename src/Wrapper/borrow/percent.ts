import {
  Pool,
  Uint112,
  Uint256,
  Uint40,
} from "@timeswap-labs/timeswap-v1-sdk-core";
import { Pool as SDKPool } from "@timeswap-labs/timeswap-v1-sdk";
import { GlobalParams } from "../global";
import { calculateMaxValue, getCurrentTime } from "../helper";
import {
  calculateApr,
  calculateCdp,
  calculateFuturisticApr,
  calculateFuturisticCdp,
  calculateHelper,
} from "./common";

export function percentCalculate(
  app: ElmApp<Ports>,
  pool: Pool,
  query: BorrowQuery
) {
  try {
    const maturity = new Uint256(query.pool.maturity);
    const currentTime = getCurrentTime();
    const state = {
      x: new Uint112(query.poolInfo.x),
      y: new Uint112(query.poolInfo.y),
      z: new Uint112(query.poolInfo.z),
    };

    const {
      dueOut,
      assetOut: assetOutReturn,
      xDecrease,
      yIncrease,
      zIncrease,
    } = pool.borrowGivenPercent(
      state,
      new Uint112(query.assetOut),
      new Uint40(query.percent!),
      currentTime
    );
    const debtIn = dueOut.debt.toString();
    const collateralIn = dueOut.collateral.toString();
    const txnFee = xDecrease.sub(assetOutReturn).toString();

    const timeSlippageBefore = currentTime.sub(60);
    // const timeSlippageAfter =
    //   currentTime.add(3 * 60).toBigInt() >= maturity.toBigInt()
    //     ? maturity.sub(1)
    //     : currentTime.add(3 * 60);

    const {
      dueOut: dueSlippageBefore,
      yIncrease: yIncreaseBefore,
      xDecrease: xDecreaseBefore
    } =
      pool.borrowGivenPercent(
        state,
        new Uint112(query.assetOut),
        new Uint40(query.percent!),
        timeSlippageBefore
      );
    // const { xDecrease: xDecreaseAfter } = pool.borrowGivenPercent(
    //   state,
    //   new Uint112(query.assetOut),
    //   new Uint40(query.percent!),
    //   timeSlippageAfter
    // );

    const interestBefore = calculateHelper(
      maturity,
      timeSlippageBefore,
      yIncreaseBefore
    );

    // const principalAfter = xDecreaseAfter;

    const maxDebt = calculateMaxValue(interestBefore, query.slippage)
      .add(xDecreaseBefore)
      .toString();

    const maxCollateral = calculateMaxValue(
      dueSlippageBefore.collateral,
      query.slippage
    ).toString();

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
      dueOut.collateral,
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
        debtIn,
        collateralIn,
        maxDebt,
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

export async function percentTransaction(
  pool: SDKPool,
  gp: GlobalParams,
  borrow: Borrow
) {
  return await pool.upgrade(gp.walletSigner!).borrowGivenPercent({
    assetTo: borrow.assetTo,
    dueTo: borrow.dueTo,
    assetOut: new Uint112(borrow.assetOut),
    percent: new Uint40(borrow.percent),
    maxDebt: new Uint112(borrow.maxDebt),
    maxCollateral: new Uint112(borrow.maxCollateral),
    deadline: new Uint256(borrow.deadline),
  });
}

interface Borrow {
  assetTo: string;
  dueTo: string;
  assetOut: string;
  percent: number;
  maxDebt: string;
  maxCollateral: string;
  deadline: number;
}
