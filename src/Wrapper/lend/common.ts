import { CP, Pool } from "@timeswap-labs/timeswap-v1-sdk-core";
import {
  Uint112,
  Uint128,
  Uint256,
  Uint40,
} from "@timeswap-labs/timeswap-v1-sdk-core";

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

export function calculateCdp(
  assetIn: string,
  assetDecimals: number,
  assetSpot: number | null,
  insuranceOut: Uint128 | string,
  collateralDecimals: number,
  collateralSpot: number | null
): CDP {
  const ratio = new Uint256(assetIn)
    .mul(pow(10n, BigInt(collateralDecimals)))
    .div(insuranceOut)
    .toString();

  // const percent = (assetSpot && collateralSpot) ? new Uint256(assetIn).div(new Uint256(insuranceOut)) : null;
  const percent = null;

  return { ratio, percent };
}

export function calculateMinValue(value: Uint128, slippage: number): Uint256 {
  return new Uint256(value).mul(100000 * (1 - slippage)).div(100000);
}

export function calculatePercent(
  pool: Pool,
  state : CP,
  assetIn: Uint112,
  yDecrease: Uint112,
  currentTime: Uint256
): Uint256 {
  const { yDecrease: yMin } = pool.lendGivenPercent(
    state,
    assetIn,
    new Uint40(0),
    currentTime
  );
  const { yDecrease: yMax } = pool.lendGivenPercent(
    state,
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
