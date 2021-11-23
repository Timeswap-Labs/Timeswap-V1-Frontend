import { Pool } from "@timeswap-labs/timeswap-v1-sdk";
import { Uint112, Uint256 } from "@timeswap-labs/timeswap-v1-sdk-core";
import { GlobalParams } from "../global";
import { getCurrentTime } from "../helper";
import { WhiteList } from "../whitelist";
import {
  calculateApr,
  calculateCf,
  calculateMaxValue,
  calculatePercent,
} from "./common";

export async function collateralCalculate(
  app: ElmApp<Ports>,
  whitelist: WhiteList,
  pool: Pool,
  query: Query
) {
  try {
    const maturity = new Uint256(query.maturity);
    const currentTime = getCurrentTime();
    const assetOut = new Uint112(query.assetOut);

    const { due, yIncrease } = await pool.calculateBorrowGivenCollateral(
      assetOut,
      new Uint112(query.collateralIn),
      currentTime
    );
    const debtIn = due.debt.toString();

    const percent = await calculatePercent(
      pool,
      assetOut,
      yIncrease,
      currentTime
    );

    const maxDebt = calculateMaxValue(
      due.debt.sub(query.assetOut),
      query.slippage
    )
      .add(query.assetOut)
      .toString();

    const apr = calculateApr(due.debt, query.assetOut, maturity, currentTime);
    const cf = calculateCf(
      query.assetOut,
      whitelist,
      query.collateral,
      query.collateralIn
    );

    app.ports.sdkBorrowMsg.send({
      asset: query.asset,
      collateral: query.collateral,
      maturity: query.maturity,
      assetOut: query.assetOut,
      collateralIn: query.collateralIn,
      result: {
        percent: Number(percent.toBigInt()),
        debtIn,
        maxDebt,
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
      collateralIn: query.collateralIn,
      result: 0,
    });
  }
}

export async function collateralTransaction(
  pool: Pool,
  gp: GlobalParams,
  borrow: Borrow
) {
  const txn = await pool.upgrade(gp.metamaskSigner!).borrowGivenCollateral({
    assetTo: borrow.assetTo,
    dueTo: borrow.dueTo,
    assetOut: new Uint112(borrow.assetOut),
    collateralIn: new Uint112(borrow.collateralIn),
    maxDebt: new Uint112(borrow.maxDebt),
    deadline: new Uint256(borrow.deadline),
  });

  await txn.wait();
}

interface Query {
  asset: string;
  collateral: string;
  maturity: number;
  assetOut: string;
  collateralIn: string;
  slippage: number;
}

interface Borrow {
  assetTo: string;
  dueTo: string;
  assetOut: string;
  collateralIn: string;
  maxDebt: string;
  deadline: number;
}
