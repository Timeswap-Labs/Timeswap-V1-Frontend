import { CP, PoolCore } from "@timeswap-labs/timeswap-v1-biconomy-sdk";

import {
  Uint112,
  Uint128,
  Uint256,
  Uint40,
} from "@timeswap-labs/timeswap-v1-biconomy-sdk";

export function calculateApr(
  bond: Uint128,
  assetIn: string,
  maturity: Uint256,
  currentTime: Uint256
): number {
  const SECONDS = 31556926n;
  const apr =
    ((bond.toBigInt() - BigInt(assetIn)) * SECONDS * 10_000n) /
    (BigInt(assetIn) * (maturity.toBigInt() - currentTime.toBigInt()));

  return Number(apr) / 10_000;
}

export function calculateCdp(
  assetIn: string,
  assetDecimals: number,
  assetSpot: number | null,
  insuranceOut: Uint128 | string,
  collateralDecimals: number,
  collateralSpot: number | null
): CDP {
  let percent = null;
  const ratio = new Uint256(insuranceOut)
    .mul(pow(10n, BigInt(assetDecimals)))
    .div(assetIn)
    .toString();

  if (assetSpot && collateralSpot) {
    percent =
      Number(
        (new Uint256(insuranceOut).toBigInt() *
          BigInt(Math.floor(collateralSpot * 10000000000)) *
          pow(10n, BigInt(assetDecimals))) /
          BigInt(assetIn) /
          BigInt(Math.floor(assetSpot * 1000000)) /
          pow(10n, BigInt(collateralDecimals))
      ) / 10000;
  }

  return { ratio, percent };
}

export function calculateFuturisticApr(
  state: CP,
  xIncrease: Uint112,
  yDecrease: Uint112
): number {
  const SECONDS = 31556926n;
  const temp =
    (state.y.sub(yDecrease).toBigInt() * SECONDS * 10000n) /
    (state.x.add(xIncrease).toBigInt() << 32n);
  const apr = Number(temp) / 10000;
  return apr;
}

export function calculateFuturisticCdp(
  state: CP,
  assetDecimals: number,
  collateralDecimals: number,
  xIncrease: Uint112,
  zDecrease: Uint112,
  assetSpot: number | null,
  collateralSpot: number | null
): CDP {
  let percent = null;
  const ratio = (
    (state.z.sub(zDecrease).toBigInt() * (10n ** BigInt(assetDecimals))) /
    state.x.add(xIncrease).toBigInt()
  ).toString();

  if (assetSpot && collateralSpot) {
    percent = Number(
          (BigInt(ratio) * BigInt(Math.floor(collateralSpot * 10000000000))) /
          BigInt(Math.floor(assetSpot * 1000000)) /
          10n ** BigInt(collateralDecimals)
    ) / 10000
  }

  return { ratio, percent };
}

export function percentMinMaxValues(
  pool: PoolCore,
  state: CP,
  assetIn: Uint112,
  currentTime: Uint256
) {
  const { yDecrease: yMin, claimsOut: claimsMin } = pool.lendGivenPercent(
    state,
    assetIn,
    new Uint40(0),
    currentTime
  );

  const { yDecrease: yMax, claimsOut: claimsMax } = pool.lendGivenPercent(
    state,
    assetIn,
    new Uint40(1n << 32n),
    currentTime
  );

  return { yMin, yMax, claimsMin, claimsMax };
}

export function calculatePercent(
  yMin: Uint112,
  yMax: Uint112,
  yDecrease: Uint112
): Uint256 {
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
