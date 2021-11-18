import { Pool } from "@timeswap-labs/timeswap-v1-sdk";
import {
  Uint112,
  Uint128,
  Uint256,
  Uint40,
} from "@timeswap-labs/timeswap-v1-sdk-core";
import { WhiteList } from "../whitelist";

export function calculateApr(
  bond: Uint128,
  assetIn: string,
  maturity: Uint256,
  currentTime: Uint256
): number {
  const SECONDS = 31556926;
  const apr = bond
    .sub(assetIn)
    .mul(SECONDS)
    .mul(10000)
    .div(assetIn)
    .div(maturity.sub(currentTime));

  return Number(apr.toBigInt()) / 10_000;
}

export function calculateCf(
  assetIn: string,
  whitelist: WhiteList,
  asset: string,
  insuranceOut: Uint128 | string
): string {
  return new Uint256(insuranceOut)
    .mul(pow(10n, BigInt(whitelist.getToken(asset).decimals)))
    .div(assetIn)
    .toString();
}

export function calculateMinValue(value: Uint128, slippage: number): Uint256 {
  return new Uint256(value).mul(10000 * (1 - slippage)).div(10000);
}

export async function calculatePercent(
  pool: Pool,
  assetIn: Uint112,
  yDecrease: Uint112,
  currentTime: Uint256
): Promise<Uint256> {
  const { yDecrease: yMin } = await pool.calculateLendGivenPercent(
    assetIn,
    new Uint40(0),
    currentTime
  );
  const { yDecrease: yMax } = await pool.calculateLendGivenPercent(
    assetIn,
    new Uint40(1n << 32n),
    currentTime
  );

  return new Uint256(yDecrease)
    .sub(yMin)
    .mul(1n << 32n)
    .div(yMax.sub(yMin));
}

export function pow(a: bigint, b: bigint) {
  let result = 1n;
  for (let i = 0n; i < b; i++) result *= a;
  return result;
}
