import { Pool } from "@timeswap-labs/timeswap-v1-sdk";
import { Uint112, Uint256, Uint40 } from "@timeswap-labs/timeswap-v1-sdk-core";
import { WhiteList } from "../whitelist";

export function calculateApr(
  debt: Uint112,
  assetOut: string,
  maturity: Uint256,
  currentTime: Uint256
): number {
  const SECONDS = 31556926;
  const apr = debt
    .sub(assetOut)
    .mul(SECONDS)
    .mul(10000)
    .div(assetOut)
    .div(maturity.sub(currentTime));

  return Number(apr.toBigInt()) / 10_000;
}

export function calculateCf(
  assetOut: string,
  whitelist: WhiteList,
  asset: string,
  collateralIn: Uint112 | string
): string {
  return new Uint256(collateralIn)
    .mul(pow(10n, BigInt(whitelist.getToken(asset).decimals)))
    .div(assetOut)
    .toString();
}

export function calculateMaxValue(value: Uint112, slippage: number): Uint256 {
  return new Uint256(value).mul(10000 * (1 + slippage)).div(10000);
}

export async function calculatePercent(
  pool: Pool,
  assetOut: Uint112,
  yIncrease: Uint112,
  currentTime: Uint256
): Promise<Uint256> {
  const { yIncrease: yMin } = await pool.calculateBorrowGivenPercent(
    assetOut,
    new Uint40(0),
    currentTime
  );
  const { yIncrease: yMax } = await pool.calculateBorrowGivenPercent(
    assetOut,
    new Uint40(1n << 32n),
    currentTime
  );

  return new Uint256(yIncrease)
    .sub(yMin)
    .mul(1n << 32n)
    .div(yMax.sub(yMin));
}

export function pow(a: bigint, b: bigint) {
  let result = 1n;
  for (let i = 0n; i < b; i++) result *= a;
  return result;
}
