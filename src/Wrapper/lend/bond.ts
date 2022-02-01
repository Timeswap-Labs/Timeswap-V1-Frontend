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
    const state = { x: new Uint112(query.poolInfo.x), y: new Uint112(query.poolInfo.y), z: new Uint112(query.poolInfo.z) };

    const { claims, yDecrease } = pool.lendGivenBond(
      state,
      assetIn,
      bondOut,
      currentTime
    );
    const insuranceOut = new Uint128(claims.insuranceInterest).add(new Uint128(claims.insurancePrincipal));

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
    const { claims: claimsSlippage } = pool.lendGivenBond(
      state,
      assetIn,
      bondOut,
      timeSlippage
    );

    const insuranceSlippage = new Uint128(claimsSlippage.insuranceInterest).add(new Uint128(claimsSlippage.insurancePrincipal));
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
        percent: Number(percent.toBigInt()),
        insuranceOut: insuranceOut.toString(),
        minInsurance,
        apr,
        cdp,
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
