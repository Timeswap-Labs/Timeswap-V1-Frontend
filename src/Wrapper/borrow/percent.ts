import { Pool } from "@timeswap-labs/timeswap-v1-sdk";
import { Uint112, Uint256, Uint40 } from "@timeswap-labs/timeswap-v1-sdk-core";
import { GlobalParams } from "../global";
import { getCurrentTime } from "../helper";
import { WhiteList } from "../whitelist";
import { calculateApr, calculateCf, calculateMaxValue } from "./common";

export async function percentCalculate(
  app: ElmApp<Ports>,
  whitelist: WhiteList,
  pool: Pool,
  query: Query
) {
  try {
    const maturity = new Uint256(query.maturity);
    const currentTime = getCurrentTime();

    const { due } = await pool.calculateBorrowGivenPercent(
      new Uint112(query.assetOut),
      new Uint40(query.percent),
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
    const cf = calculateCf(
      query.assetOut,
      whitelist,
      query.asset,
      due.collateral
    );

    app.ports.sdkBorrowMsg.send({
      asset: query.asset,
      collateral: query.collateral,
      maturity: query.maturity,
      assetOut: query.assetOut,
      percent: query.percent,
      result: {
        debtIn,
        collateralIn,
        maxDebt,
        maxCollateral,
        apr,
        cf,
      },
    });
  } catch {
    app.ports.sdkBorrowMsg.send({
      asset: query.asset,
      collateral: query.collateral,
      maturity: query.maturity,
      assetOut: query.assetOut,
      percent: query.percent,
      result: 0,
    });
  }
}

export async function percentTransaction(
  pool: Pool,
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

interface Query {
  asset: string;
  collateral: string;
  maturity: number;
  assetOut: string;
  percent: number;
  slippage: number;
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
