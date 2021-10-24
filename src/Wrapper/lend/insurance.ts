import { Pool } from "@timeswap-labs/timeswap-v1-sdk";
import { Uint112, Uint128, Uint256 } from "@timeswap-labs/timeswap-v1-sdk-core";
import { getCurrentTime } from "../helper";
import { WhiteList } from "../whitelist";
import {
  calculateApr,
  calculateCf,
  calculateMinValue,
  calculatePercent,
} from "./common";

export async function insuranceCalculate(
  app: ElmApp<Ports>,
  whitelist: WhiteList,
  pool: Pool,
  query: Query
) {
  try {
    const maturity = new Uint256(query.maturity);
    const currentTime = getCurrentTime();
    const assetIn = new Uint112(query.assetIn);

    const { claims, yDecrease } = await pool.calculateLendGivenInsurance(
      assetIn,
      new Uint128(query.insuranceOut),
      currentTime
    );
    const bondOut = claims.bond.toString();

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
    const { claims: claimsSlippage } = await pool.calculateLendGivenInsurance(
      assetIn,
      new Uint128(query.insuranceOut),
      timeSlippage
    );

    const minBond = calculateMinValue(
      claimsSlippage.bond.sub(query.assetIn),
      query.slippage
    )
      .add(query.assetIn)
      .toString();

    const apr = calculateApr(claims.bond, query.assetIn, maturity, currentTime);
    const cf = calculateCf(
      query.assetIn,
      whitelist,
      query.collateral,
      query.insuranceOut
    );

    app.ports.sdkLendMsg.send({
      ...query,
      result: {
        percent: Number(percent.toBigInt()),
        bondOut,
        minBond,
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

interface Query {
  asset: string;
  collateral: string;
  maturity: number;
  assetIn: string;
  insuranceOut: string;
  slippage: number;
}
