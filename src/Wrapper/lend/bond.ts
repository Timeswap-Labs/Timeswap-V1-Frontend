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

export function bondCalculate(
  app: ElmApp<Ports>,
  pool: Pool,
  query: LendQuery
) {
  try {
    const maturity = new Uint256(query.pool.maturity);
    const currentTime = getCurrentTime();
    const assetIn = new Uint112(query.assetIn);
    const bondOut = new Uint128(query.bondOut!);
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

    // Bond too low check
    if (bondOut.lt(claimsMin.bondInterest.add(claimsMin.bondPrincipal))) {
      app.ports.receiveLendAnswer.send({
        ...query,
        result: 1,
      });

      return;
    }

    // Bond too high check
    if (bondOut.gt(claimsMax.bondInterest.add(claimsMax.bondPrincipal))) {
      app.ports.receiveLendAnswer.send({
        ...query,
        result: 2,
      });

      return;
    }

    const {
      claimsOut,
      assetIn: assetInReturn,
      xIncrease,
      yDecrease,
      zDecrease,
    } = pool.lendGivenBond(state, assetIn, bondOut, currentTime);

    const insuranceOut = new Uint128(claimsOut.insuranceInterest).add(
      new Uint128(claimsOut.insurancePrincipal)
    );

    const percent = calculatePercent(yMin, yMax, yDecrease);
    const txnFee = assetInReturn.sub(xIncrease).toString();

    const timeSlippageAfter =
      currentTime.add(3 * 60).toBigInt() >= maturity.toBigInt()
        ? maturity.sub(1)
        : currentTime.add(3 * 60);
    const { claimsOut: claimsSlippageAfter } = pool.lendGivenBond(
      state,
      assetIn,
      bondOut,
      timeSlippageAfter
    );

    const insuranceSlippage = new Uint128(
      claimsSlippageAfter.insuranceInterest
    ).add(new Uint128(claimsSlippageAfter.insurancePrincipal));
    const minInsurance = calculateMinValue(
      insuranceSlippage,
      query.slippage
    ).toString();

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
      insuranceOut,
      query.pool.collateral.decimals,
      query.poolInfo.collateralSpot
    );

    const futuristicApr = calculateFuturisticApr(state, xIncrease, yDecrease);
    const futuristicCdp = calculateFuturisticCdp(
      state,
      query.pool.asset.decimals,
      xIncrease,
      zDecrease
    );

    query.pool.maturity = query.pool.maturity.toString();

    app.ports.receiveLendAnswer.send({
      ...query,
      result: {
        percent: Number(percent.toBigInt()),
        insuranceOut: insuranceOut.toString(),
        minInsurance,
        apr,
        cdp,
        txnFee,
      },
    });
  } catch (err) {
    app.ports.receiveLendAnswer.send({
      ...query,
      result: 0,
    });
  }
}

export async function bondTransaction(
  pool: SDKPool,
  gp: GlobalParams,
  lend: Lend
) {
  return await pool.upgrade(gp.walletSigner!).lendGivenBond({
    bondTo: lend.bondTo,
    insuranceTo: lend.insuranceTo,
    assetIn: new Uint112(lend.assetIn),
    bondOut: new Uint128(lend.bondOut),
    minInsurance: new Uint128(lend.minInsurance),
    deadline: new Uint256(lend.deadline),
  });
}

interface Lend {
  bondTo: string;
  insuranceTo: string;
  assetIn: string;
  bondOut: string;
  minInsurance: string;
  deadline: number;
}
