import {
  CP,
  Uint112,
  Uint256,
} from "@timeswap-labs/timeswap-v1-sdk-core";

export function calculateApr(
  debt: Uint112,
  assetIn: Uint112,
  maturity: Uint256,
  currentTime: Uint256
): number {
  const SECONDS = 31556926;
  const apr = debt
    .sub(assetIn)
    .mul(SECONDS)
    .mul(10000)
    .div(assetIn)
    .div(maturity.sub(currentTime));

  return Number(apr.toBigInt()) / 10_000;
}

export function calculateCdp(
  assetIn: Uint112 | string,
  assetDecimals: number,
  assetSpot: number | null,
  collateralIn: Uint112 | string,
  collateralDecimals: number,
  collateralSpot: number | null
): CDP {
  let percent = null;
  const ratio = new Uint256(collateralIn)
    .mul(pow(10n, BigInt(assetDecimals)))
    .div(assetIn)
    .toString();

  if (assetSpot && collateralSpot) {
    percent =
      Number(
        (new Uint256(collateralIn).toBigInt() *
          BigInt(Math.floor(collateralSpot * 10000000000)) *
          pow(10n, BigInt(assetDecimals))) /
          BigInt(assetIn.toString()) /
          BigInt(Math.floor(assetSpot * 1000000)) /
          pow(10n, BigInt(collateralDecimals))
      ) / 10000;
  }

  return { ratio, percent };
}

export function calculateFuturisticApr(
  state: CP,
  xIncrease: Uint112,
  yIncrease: Uint112
): number {
  const SECONDS = 31556926n;
  const temp =
    (state.y.add(yIncrease).toBigInt() * SECONDS * 10000n) /
    (state.x.add(xIncrease).toBigInt() << 32n);
  const apr = Number(temp) / 10000;
  return apr;
}

export function calculateFuturisticCdp(
  state: CP,
  assetDecimals: number,
  collateralDecimals: number,
  xIncrease: Uint112,
  zIncrease: Uint112,
  assetSpot: number | null,
  collateralSpot: number | null
): CDP {

  let percent = null;
  const ratio = (
    (state.z.add(zIncrease).toBigInt() * (10n ** BigInt(assetDecimals))) /
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
