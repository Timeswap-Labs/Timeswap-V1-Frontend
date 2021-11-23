import { Pool } from "@timeswap-labs/timeswap-v1-sdk";
import {
  Uint112,
  Uint128,
  Uint256,
  Uint40,
} from "@timeswap-labs/timeswap-v1-sdk-core";
import { GlobalParams } from "../global";
import { getCurrentTime } from "../helper";
import { WhiteList } from "../whitelist";
import { calculateApr, calculateCf, calculateMinValue } from "./common";

export async function percentCalculate(
  app: ElmApp<Ports>,
  whitelist: WhiteList,
  pool: Pool,
  query: Query
) {
  try {
    const maturity = new Uint256(query.maturity);
    const currentTime = getCurrentTime();

    const { claims } = await pool.calculateLendGivenPercent(
      new Uint112(query.assetIn),
      new Uint40(query.percent),
      currentTime
    );
    const bondOut = claims.bond.toString();
    const insuranceOut = claims.insurance.toString();

    const timeSlippage =
      currentTime.add(3 * 60).toBigInt() >= maturity.toBigInt()
        ? maturity.sub(1)
        : currentTime.add(3 * 60);
    const { claims: claimsSlippage } = await pool.calculateLendGivenPercent(
      new Uint112(query.assetIn),
      new Uint40(query.percent),
      timeSlippage
    );

    const minBond = calculateMinValue(
      claimsSlippage.bond.sub(query.assetIn),
      query.slippage
    )
      .add(query.assetIn)
      .toString();
    const minInsurance = calculateMinValue(
      claimsSlippage.insurance,
      query.slippage
    ).toString();

    const apr = calculateApr(claims.bond, query.assetIn, maturity, currentTime);
    const cf = calculateCf(
      query.assetIn,
      whitelist,
      query.collateral,
      claims.insurance
    );

    app.ports.sdkLendMsg.send({
      ...query,
      result: {
        bondOut,
        insuranceOut,
        minBond,
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

export async function percentTransaction(
  pool: Pool,
  gp: GlobalParams,
  lend: Lend
) {
  const txn = await pool.upgrade(gp.metamaskSigner!).lendGivenPercent({
    bondTo: lend.bondTo,
    insuranceTo: lend.insuranceTo,
    assetIn: new Uint112(lend.assetIn),
    percent: new Uint40(lend.percent),
    minBond: new Uint128(lend.minBond),
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
  percent: number;
  slippage: number;
}

interface Lend {
  bondTo: string;
  insuranceTo: string;
  assetIn: string;
  percent: number;
  minBond: string;
  minInsurance: string;
  deadline: number;
}
