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

export async function debtCalculate(
  app: ElmApp<Ports>,
  whitelist: WhiteList,
  pool: Pool,
  query: Query
) {
  try {
    const maturity = new Uint256(query.maturity);
    const currentTime = getCurrentTime();
    const assetOut = new Uint112(query.assetOut);
    const debtIn = new Uint112(query.debtIn);

    const { due, yIncrease } = await pool.calculateBorrowGivenDebt(
      assetOut,
      debtIn,
      currentTime
    );
    const collateralIn = due.collateral.toString();

    const percent = await calculatePercent(
      pool,
      assetOut,
      yIncrease,
      currentTime
    );

    const maxCollateral = calculateMaxValue(
      due.collateral,
      query.slippage
    ).toString();

    const apr = calculateApr(debtIn, query.assetOut, maturity, currentTime);
    const cf = calculateCf(
      query.assetOut,
      whitelist,
      query.collateral,
      collateralIn
    );

    app.ports.sdkBorrowMsg.send({
      asset: query.asset,
      collateral: query.collateral,
      maturity: query.maturity,
      assetOut: query.assetOut,
      debtIn: query.debtIn,
      result: {
        percent: Number(percent.toBigInt()),
        collateralIn,
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
      debtIn: query.debtIn,
      result: 0,
    });
  }
}

export async function debtTransaction(
  pool: Pool,
  gp: GlobalParams,
  borrow: Borrow
) {
  const txn = await pool.upgrade(gp.metamaskSigner!).borrowGivenDebt({
    assetTo: borrow.assetTo,
    dueTo: borrow.dueTo,
    assetOut: new Uint112(borrow.assetOut),
    debtIn: new Uint112(borrow.debtIn),
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
  debtIn: string;
  slippage: number;
}

interface Borrow {
  assetTo: string;
  dueTo: string;
  assetOut: string;
  debtIn: string;
  maxCollateral: string;
  deadline: number;
}
