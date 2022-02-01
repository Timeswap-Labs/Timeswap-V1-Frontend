// import { Pool } from "@timeswap-labs/timeswap-v1-sdk";
import { Pool, Uint112, Uint256, Uint40 } from "@timeswap-labs/timeswap-v1-sdk-core";
import { Pool as SDKPool } from "@timeswap-labs/timeswap-v1-sdk";
import { GlobalParams } from "../global";
import { getCurrentTime } from "../helper";
import { calculateApr, calculateCdp, calculateMaxValue } from "./common";

export async function percentCalculate(
  app: ElmApp<Ports>,
  pool: Pool,
  query: BorrowQuery
) {
  try {
    const maturity = new Uint256(query.pool.maturity);
    const currentTime = getCurrentTime();
    const state = { x: new Uint112(query.poolInfo.x), y: new Uint112(query.poolInfo.y), z: new Uint112(query.poolInfo.z) };

    const { due } = pool.borrowGivenPercent(
      state,
      new Uint112(query.assetOut),
      new Uint40(query.percent!),
      currentTime
    );
    const debtIn = due.debt.toString();
    const collateralIn = due.collateral.toString();

    const maxDebt = calculateMaxValue(
      due.debt.sub(query.assetOut),
      query.slippage
    )
      .add(query.assetOut)
      .toString();
    const maxCollateral = calculateMaxValue(
      due.collateral,
      query.slippage
    ).toString();

    const apr = calculateApr(due.debt, query.assetOut, maturity, currentTime);
    const cdp = calculateCdp(
      query.assetOut,
      query.pool.asset.decimals,
      query.poolInfo.assetSpot,
      due.collateral,
      query.pool.collateral.decimals,
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
  const txn = await pool.upgrade(gp.metamaskSigner!).borrowGivenPercent({
    assetTo: borrow.assetTo,
    dueTo: borrow.dueTo,
    assetOut: new Uint112(borrow.assetOut),
    percent: new Uint40(borrow.percent),
    maxDebt: new Uint112(borrow.maxDebt),
    maxCollateral: new Uint112(borrow.maxCollateral),
    deadline: new Uint256(borrow.deadline),
  });

  await txn.wait();
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
