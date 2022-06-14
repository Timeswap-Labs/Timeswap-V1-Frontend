import { Pool as SDKPool } from "@timeswap-labs/timeswap-v1-biconomy-sdk";
import {
  Uint112,
  Uint128,
  Uint256,
  Uint40,
  PoolCore,
} from "@timeswap-labs/timeswap-v1-biconomy-sdk";
import { GlobalParams } from "../global";
import { calculateMinValue, getCurrentTime } from "../helper";
import {
  calculateApr,
  calculateCdp,
  calculateFuturisticApr,
  calculateFuturisticCdp,
} from "./common";

export async function percentCalculate(
  app: ElmApp<Ports>,
  pool: PoolCore,
  query: LendQuery
) {
  try {
    const maturity = new Uint256(query.pool.maturity);
    const currentTime = getCurrentTime();

    const state = {
      x: new Uint112(query.poolInfo.x),
      y: new Uint112(query.poolInfo.y),
      z: new Uint112(query.poolInfo.z),
    };
    const {
      claimsOut,
      assetIn: assetInReturn,
      xIncrease,
      yDecrease,
      zDecrease,
    } = pool.lendGivenPercent(
      state,
      new Uint112(query.assetIn),
      new Uint40(query.percent!),
      currentTime
    );

    const bondOut = new Uint128(claimsOut.bondInterest).add(
      new Uint128(claimsOut.bondPrincipal)
    );
    const insuranceOut = new Uint128(claimsOut.insuranceInterest).add(
      new Uint128(claimsOut.insurancePrincipal)
    );
    const txnFee = assetInReturn.sub(xIncrease).toString();

    const timeSlippageBefore = currentTime.sub(60);
    const timeSlippageAfter =
      currentTime.add(3 * 60).toBigInt() >= maturity.toBigInt()
        ? maturity.sub(1)
        : currentTime.add(3 * 60);

    const { claimsOut: claimsSlippageBefore } = pool.lendGivenPercent(
      state,
      new Uint112(query.assetIn),
      new Uint40(query.percent!),
      new Uint256(timeSlippageBefore)
    );
    const { claimsOut: claimsSlippageAfter } = pool.lendGivenPercent(
      state,
      new Uint112(query.assetIn),
      new Uint40(query.percent!),
      new Uint256(timeSlippageAfter)
    );

    const insuranceSlippage = new Uint128(
      claimsSlippageAfter.insuranceInterest
    ).add(new Uint128(claimsSlippageAfter.insurancePrincipal));

    const minBond = calculateMinValue(
      new Uint128(claimsSlippageAfter.bondInterest),
      query.slippage
    )
      .add(new Uint128(claimsSlippageBefore.bondPrincipal))
      .toString();

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
        bondOut: bondOut.toString(),
        insuranceOut: insuranceOut.toString(),
        minBond,
        minInsurance,
        apr,
        cdp,
        futureApr,
        futureCdp,
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

export async function percentTransaction(
  pool: SDKPool,
  gp: GlobalParams,
  lend: Lend
) {
  return await pool
    .upgrade(gp.biconomy.getSignerByAddress(await gp.walletSigner.getAddress()))
    .lendGivenPercent({
      bondTo: lend.bondTo,
      insuranceTo: lend.insuranceTo,
      assetIn: new Uint112(lend.assetIn),
      percent: new Uint40(lend.percent),
      minBond: new Uint128(lend.minBond),
      minInsurance: new Uint128(lend.minInsurance),
      deadline: new Uint256(lend.deadline),
    });
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
