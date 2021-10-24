import { Pool } from "@timeswap-labs/timeswap-v1-sdk";
import { Uint112, Uint128, Uint256 } from "@timeswap-labs/timeswap-v1-sdk-core";
import { GlobalParams } from "../global";
import { getCurrentTime } from "../helper";
import { WhiteList } from "../whitelist";
import {
  calculateApr,
  calculateCf,
  calculateMinValue,
  calculatePercent,
} from "./common";

export async function bondCalculate(
  app: ElmApp<Ports>,
  whitelist: WhiteList,
  pool: Pool,
  query: Query
) {
  try {
    const maturity = new Uint256(query.maturity);
    const currentTime = getCurrentTime();
    const assetIn = new Uint112(query.assetIn);
    const bondOut = new Uint128(query.bondOut);

    const { claims, yDecrease } = await pool.calculateLendGivenBond(
      assetIn,
      bondOut,
      currentTime
    );
    const insuranceOut = claims.insurance.toString();

    const percent = await calculatePercent(
      pool,
      assetIn,
      yDecrease,
      currentTime
    );

    const timeSlippage =
      currentTime.add(3 * 60).toBigInt() >= maturity.toBigInt()
        ? maturity.sub(1)
        : currentTime.add(3 * 60);
    const { claims: claimsSlippage } = await pool.calculateLendGivenBond(
      assetIn,
      bondOut,
      timeSlippage
    );

    const minInsurance = calculateMinValue(
      claimsSlippage.insurance,
      query.slippage
    ).toString();

    const apr = calculateApr(bondOut, query.assetIn, maturity, currentTime);
    const cf = calculateCf(
      query.assetIn,
      whitelist,
      query.collateral,
      claims.insurance
    );

    app.ports.sdkLendMsg.send({
      ...query,
      result: {
        percent: Number(percent.toBigInt()),
        insuranceOut,
        minInsurance,
        apr,
        cf,
      },
    });
  } catch {
    app.ports.sdkLendMsg.send({
      ...query,
      result: 0,
    });
  }
}

export async function bondTransaction(
  pool: Pool,
  gp: GlobalParams,
  lend: Lend
) {
  const txn = await pool.upgrade(gp.metamaskSigner!).lendGivenBond({
    bondTo: lend.bondTo,
    insuranceTo: lend.insuranceTo,
    assetIn: new Uint112(lend.assetIn),
    bondOut: new Uint128(lend.bondOut),
    minInsurance: new Uint128(lend.minInsurance),
    deadline: new Uint256(lend.deadline),
  });

  await txn.wait();
}

interface Query {
  asset: string;
  collateral: string;
  maturity: number;
  assetIn: string;
  bondOut: string;
  slippage: number;
}

interface Lend {
  bondTo: string;
  insuranceTo: string;
  assetIn: string;
  bondOut: string;
  minInsurance: string;
  deadline: number;
}
