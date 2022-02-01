import { Pool as SDKPool } from "@timeswap-labs/timeswap-v1-sdk";
import {
  Uint112,
  Uint128,
  Uint256,
  Uint40,
  Pool
} from "@timeswap-labs/timeswap-v1-sdk-core";
import { GlobalParams } from "../global";
import { getCurrentTime } from "../helper";
import { calculateApr, calculateCdp, calculateMinValue } from "./common";

export function percentCalculate(
  app: ElmApp<Ports>,
  pool: Pool,
  query: LendQuery
) {
  try {
    const maturity = new Uint256(query.pool.maturity);
    const currentTime = getCurrentTime();

    const state = { x: new Uint112(query.poolInfo.x), y: new Uint112(query.poolInfo.y), z: new Uint112(query.poolInfo.z) };

    const { claims } = pool.lendGivenPercent(
      state,
      new Uint112(query.assetIn),
      new Uint40(query.percent!),
      currentTime
    );

    const bondOut = new Uint128(claims.bondInterest).add(new Uint128(claims.bondPrincipal));
    const insuranceOut = new Uint128(claims.insuranceInterest).add(new Uint128(claims.insurancePrincipal));

    const timeSlippage =
      currentTime.add(3 * 60).toBigInt() >= maturity.toBigInt()
        ? maturity.sub(1)
        : currentTime.add(3 * 60);
    const { claims: claimsSlippage } = pool.lendGivenPercent(
      state,
      new Uint112(query.assetIn),
      new Uint40(query.percent!),
      timeSlippage
    );

    const bondSlippage = new Uint128(claimsSlippage.bondInterest).add(new Uint128(claimsSlippage.bondPrincipal));
    const insuranceSlippage = new Uint128(claimsSlippage.insuranceInterest).add(new Uint128(claimsSlippage.insurancePrincipal));

    const minBond = calculateMinValue(
      bondSlippage.sub(query.assetIn),
      query.slippage
    )
      .add(query.assetIn)
      .toString();

    const minInsurance = calculateMinValue(
      insuranceSlippage,
      query.slippage
    ).toString();

    const apr = calculateApr(bondOut, query.assetIn, maturity, currentTime);
    const cdp = calculateCdp(
      query.assetIn,
      query.pool.asset.decimals,
      query.poolInfo.assetSpot,
      insuranceOut,
      query.pool.collateral.decimals,
      query.poolInfo.collateralSpot
      );

    query.pool.maturity = query.pool.maturity.toString();

    app.ports.receiveLendAnswer.send({
      ...query,
      result: {
        bondOut: bondOut.toString(),
        insuranceOut: insuranceOut.toString(),
        minBond,
        minInsurance,
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

export async function percentTransaction(
  pool: SDKPool,
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

interface Lend {
  bondTo: string;
  insuranceTo: string;
  assetIn: string;
  percent: number;
  minBond: string;
  minInsurance: string;
  deadline: number;
}
