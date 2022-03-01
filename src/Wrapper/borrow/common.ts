import { CP, Pool, Uint112, Uint256, Uint40 } from "@timeswap-labs/timeswap-v1-sdk-core";

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

export function calculateCdp(
  assetOut: string,
  assetDecimals: number,
  assetSpot: number | null,
  collateralIn: Uint112 | string,
  collateralDecimals: number,
  collateralSpot: number | null
): CDP {
  let percent = null;
  // const ratio = new Uint256(assetOut)
  //   .mul(pow(10n, BigInt(collateralDecimals)))
  //   .div(collateralIn)
  //   .toString();

  const ratio = new Uint256(collateralIn)
    .mul(pow(10n, BigInt(assetDecimals)))
    .div(assetOut)
    .toString();

  if (assetSpot && collateralSpot) {
    // const newRatio = new Uint256(assetOut)
    //   .mul(pow(10n, BigInt(collateralDecimals)))
    //   .div(collateralIn)
    //   .div(pow(10n, BigInt(assetDecimals)))
    //   .toString();

    // percent = Number(newRatio) ? Number(newRatio) * assetSpot / collateralSpot : null;

    percent =
      Number(
        new Uint256(collateralIn).toBigInt()
        * BigInt(Math.floor(collateralSpot * 10000000000))
        * (pow(10n, BigInt(assetDecimals)))
        / BigInt(assetOut)
        / BigInt(Math.floor(assetSpot * 1000000))
        / (pow(10n, BigInt(collateralDecimals)))
      ) / 10000
  }

  return { ratio, percent };
}

export function calculateMaxValue(value: Uint112, slippage: number): Uint256 {
  return new Uint256(value).mul(Math.round(100000 * (1 + slippage))).div(100000);
}

export function percentMinMaxValues(
  pool: Pool,
  state : CP,
  assetOut: Uint112,
  currentTime: Uint256
) {
  const { yIncrease: yMin, dueOut: dueMin } = pool.borrowGivenPercent(
    state,
    assetOut,
    new Uint40(0),
    currentTime
  );
  const { yIncrease: yMax, dueOut: dueMax } = pool.borrowGivenPercent(
    state,
    assetOut,
    new Uint40(1n << 32n),
    currentTime
  );

  return { yMin, yMax, dueMin, dueMax };
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
