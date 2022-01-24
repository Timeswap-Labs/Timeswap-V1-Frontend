import { Pool as SDKPool } from "@timeswap-labs/timeswap-v1-sdk";
import { Pool } from "@timeswap-labs/timeswap-v1-sdk-core";
import { Uint112, Uint128, Uint256 } from "@timeswap-labs/timeswap-v1-sdk-core";
import { GlobalParams } from "../global";
import { getCurrentTime } from "../helper";
import {
  calculateApr,
  calculateCdp,
  calculateMinValue,
  calculatePercent,
} from "./common";

export function insuranceCalculate(
  app: ElmApp<Ports>,
  pool: Pool,
  query: LendQuery
) {
  try {
    const maturity = new Uint256(query.pool.maturity);
    const currentTime = getCurrentTime();
    const assetIn = new Uint112(query.assetIn);
    const insuranceOut = new Uint128(query.insuranceOut!);
    const state = { x: new Uint112(query.poolInfo.x), y: new Uint112(query.poolInfo.y), z: new Uint112(query.poolInfo.z) };

    const { claims, yDecrease } = pool.lendGivenInsurance(
      state,
      assetIn,
      insuranceOut,
      currentTime
    );
    const bondOut = claims.bond.toString();

    const percent = calculatePercent(
      pool,
      state,
      assetIn,
      yDecrease,
      currentTime
    );

    const timeSlippage =
      currentTime.add(3 * 60).toBigInt() >= maturity.toBigInt()
        ? maturity.sub(1)
        : currentTime.add(3 * 60);
    const { claims: claimsSlippage } = pool.lendGivenInsurance(
      state,
      assetIn,
      insuranceOut,
      timeSlippage
    );

    const minBond = calculateMinValue(
      claimsSlippage.bond.sub(query.assetIn),
      query.slippage
    )
      .add(query.assetIn)
      .toString();

    const apr = calculateApr(claims.bond, query.assetIn, maturity, currentTime);
    const cdp = calculateCdp(
      query.assetIn,
      query.pool.collateral.decimals,
      query.poolInfo.assetSpot,
      query.insuranceOut!,
      query.pool.collateral.decimals,
      query.poolInfo.collateralSpot

    );

    query.pool.maturity = query.pool.maturity.toString();

    app.ports.receiveLendAnswer.send({
      ...query,
      result: {
        percent: Number(percent.toBigInt()),
        bondOut,
        minBond,
        apr,
        cdp,
      },
    });
  } catch {
    app.ports.receiveLendAnswer.send({
      ...query,
      result: 0,
    });
  }
}

export async function insuranceTransaction(
  pool: SDKPool,
  gp: GlobalParams,
  lend: Lend
) {
  const txn = await pool.upgrade(gp.metamaskSigner!).lendGivenInsurance({
    bondTo: lend.bondTo,
    insuranceTo: lend.insuranceTo,
    assetIn: new Uint112(lend.assetIn),
    insuranceOut: new Uint128(lend.insuranceOut),
    minBond: new Uint128(lend.minBond),
    deadline: new Uint256(lend.deadline),
  });

  await txn.wait();
}

interface Query {
  asset: string;
  collateral: string;
  maturity: number;
  assetIn: string;
  insuranceOut: string;
  slippage: number;
}

interface Lend {
  bondTo: string;
  insuranceTo: string;
  assetIn: string;
  insuranceOut: string;
  minBond: string;
  deadline: number;
}
