import { Pool as SDKPool } from "@timeswap-labs/timeswap-v1-sdk";
import { Pool } from "@timeswap-labs/timeswap-v1-sdk-core";
import { Uint112, Uint128, Uint256 } from "@timeswap-labs/timeswap-v1-sdk-core";
import { GlobalParams } from "../global";
import { getCurrentTime } from "../helper";
import {
  calculateApr,
  calculateCdp,
  calculateFuturisticApr,
  calculateFuturisticCdp,
  calculateMinValue,
  calculatePercent,
  percentMinMaxValues,
} from "./common";

export async function insuranceCalculate(
  app: ElmApp<Ports>,
  pool: Pool,
  query: LendQuery
) {
  try {
    const maturity = new Uint256(query.pool.maturity);
    const currentTime = getCurrentTime();
    const assetIn = new Uint112(query.assetIn);
    const insuranceOut = new Uint128(query.insuranceOut!);
    const state = {
      x: new Uint112(query.poolInfo.x),
      y: new Uint112(query.poolInfo.y),
      z: new Uint112(query.poolInfo.z),
    };

    const { yMin, yMax, claimsMin, claimsMax } = percentMinMaxValues(
      pool,
      state,
      assetIn,
      currentTime
    );

    // Insurance too high check
    if (
      insuranceOut.gt(
        claimsMin.insuranceInterest.add(claimsMin.insurancePrincipal)
      )
    ) {
      app.ports.receiveLendAnswer.send({
        ...query,
        result: 4,
      });

      return;
    }

    // Insurance too low check
    if (
      insuranceOut.lt(
        claimsMax.insuranceInterest.add(claimsMax.insurancePrincipal)
      )
    ) {
      app.ports.receiveLendAnswer.send({
        ...query,
        result: 3,
      });

      return;
    }

    const {
      claimsOut,
      assetIn: assetInReturn,
      xIncrease,
      yDecrease,
      zDecrease,
    } = pool.lendGivenInsurance(state, assetIn, insuranceOut, currentTime);
    const bondOut = new Uint128(claimsOut.bondInterest).add(
      new Uint128(claimsOut.bondPrincipal)
    );

    const percent = calculatePercent(yMin, yMax, yDecrease);
    const txnFee = assetInReturn.sub(xIncrease).toString();

    const timeSlippageBefore = currentTime.sub(60);
    const timeSlippageAfter =
      currentTime.add(3 * 60).toBigInt() >= maturity.toBigInt()
        ? maturity.sub(1)
        : currentTime.add(3 * 60);

    const { claimsOut: claimsSlippageBefore } = pool.lendGivenInsurance(
      state,
      assetIn,
      insuranceOut,
      timeSlippageBefore
    );
    const { claimsOut: claimsSlippageAfter } = pool.lendGivenInsurance(
      state,
      assetIn,
      insuranceOut,
      timeSlippageAfter
    );

    const minBond = calculateMinValue(
      new Uint128(claimsSlippageAfter.bondInterest),
      query.slippage
    )
      .add(new Uint128(claimsSlippageBefore.bondPrincipal))
      .toString();

    const apr = calculateApr(
      bondOut,
      xIncrease.toString(),
      maturity,
      currentTime
    );
    const cdp = calculateCdp(
      query.assetIn,
      query.pool.asset.decimals,
      query.poolInfo.assetSpot,
      query.insuranceOut!,
      query.pool.collateral.decimals,
      query.poolInfo.collateralSpot
    );

    const futureApr = calculateFuturisticApr(state, xIncrease, yDecrease);
    const futureCdp = calculateFuturisticCdp(
      state,
      query.pool.asset.decimals,
      query.pool.collateral.decimals,
      xIncrease,
      zDecrease,
      query.poolInfo.assetSpot,
      query.poolInfo.collateralSpot
    );

    query.pool.maturity = query.pool.maturity.toString();

    app.ports.receiveLendAnswer.send({
      ...query,
      result: {
        percent: Number(percent.toBigInt()),
        bondOut: bondOut.toString(),
        minBond,
        apr,
        cdp,
        futureApr,
        futureCdp,
        txnFee,
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
  return await pool.upgrade(gp.walletSigner!).lendGivenInsurance({
    bondTo: lend.bondTo,
    insuranceTo: lend.insuranceTo,
    assetIn: new Uint112(lend.assetIn),
    insuranceOut: new Uint128(lend.insuranceOut),
    minBond: new Uint128(lend.minBond),
    deadline: new Uint256(lend.deadline),
  });
}

interface Lend {
  bondTo: string;
  insuranceTo: string;
  assetIn: string;
  insuranceOut: string;
  minBond: string;
  deadline: number;
}
