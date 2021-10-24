import { Pool } from "@timeswap-labs/timeswap-v1-sdk";
import { Uint112, Uint256, Uint40 } from "@timeswap-labs/timeswap-v1-sdk-core";
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
      query.collateral,
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
        apr: Number(apr.toBigInt()) / 10000,
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

interface Query {
  asset: string;
  collateral: string;
  maturity: number;
  assetOut: string;
  percent: number;
  slippage: number;
}
