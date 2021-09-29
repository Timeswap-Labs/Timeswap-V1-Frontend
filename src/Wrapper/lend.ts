import { ERC20Token, Pool } from "@timeswap-labs/timeswap-v1-sdk";
import {
  Uint112,
  Uint128,
  Uint256,
  Uint40,
} from "@timeswap-labs/timeswap-v1-sdk-core";
import { GlobalParams } from "./global";
import { getCurrentTime } from "./helper";
import { WhiteList } from "./whitelist";

export function lend(app: ElmApp<Ports>, whitelist: WhiteList) {
  app.ports.queryLend.subscribe((query) =>
    lendQueryCalculation(app, whitelist, query)
  );

  app.ports.queryLendPerSecond.subscribe((query) =>
    lendQueryCalculation(app, whitelist, query)
  );
}

export function lendSigner(
  app: ElmApp<Ports>,
  whitelist: WhiteList,
  gp: GlobalParams
) {
  app.ports.approveLend.subscribe(async ({ erc20 }) => {
    (whitelist.getToken(erc20) as ERC20Token)
      .upgrade(gp.metamaskSigner!)
      .approve(whitelist.convenience, new Uint256((1n << 256n) - 1n));
  });

  app.ports.lend.subscribe(async (params) => {
    const pool = whitelist.getPool(
      params.asset,
      params.collateral,
      params.maturity
    );

    const txn = await pool.upgrade(gp.metamaskSigner!).lendGivenPercent({
      bondTo: params.bondTo,
      insuranceTo: params.insuranceTo,
      assetIn: new Uint112(params.assetIn),
      percent: new Uint40(params.percent),
      minBond: new Uint128(params.minBond),
      minInsurance: new Uint128(params.minInsurance),
      deadline: new Uint256(params.deadline),
    });

    await txn.wait();
  });
}

async function lendQueryCalculation(
  app: ElmApp<Ports>,
  whitelist: WhiteList,
  query: LendQuery
) {
  const pool = whitelist.getPool(query.asset, query.collateral, query.maturity);

  if (query.percent != undefined) {
    try {
      const maturity = new Uint256(query.maturity);
      const currentTime = getCurrentTime();

      const { claims } = await pool.calculateLendGivenPercent(
        new Uint112(query.assetIn),
        new Uint40(query.percent),
        currentTime
      );
      const bondOut = claims.bond.toString();
      const insuranceOut = claims.insurance.toString();

      const timeSlippage =
        currentTime.add(3 * 60).toBigInt() >= maturity.toBigInt()
          ? maturity.sub(1)
          : currentTime.add(3 * 60);
      const { claims: claimsSlippage } = await pool.calculateLendGivenPercent(
        new Uint112(query.assetIn),
        new Uint40(query.percent),
        timeSlippage
      );

      const minBond = calculateMinValue(
        claimsSlippage.bond.sub(query.assetIn),
        query.slippage
      )
        .add(query.assetIn)
        .toString();
      const minInsurance = calculateMinValue(
        claimsSlippage.insurance,
        query.slippage
      ).toString();

      const apr = calculateApr(
        claims.bond,
        query.assetIn,
        maturity,
        currentTime
      );
      const cf = calculateCf(
        query.assetIn,
        whitelist,
        query.collateral,
        claims.insurance
      );

      app.ports.sdkLendMsg.send({
        asset: query.asset,
        collateral: query.collateral,
        maturity: query.maturity,
        assetIn: query.assetIn,
        percent: query.percent,
        result: {
          bondOut,
          insuranceOut,
          minBond,
          minInsurance,
          apr: Number(apr.toBigInt()) / 10000,
          cf,
        },
      });
    } catch {
      app.ports.sdkLendMsg.send({
        asset: query.asset,
        collateral: query.collateral,
        maturity: query.maturity,
        assetIn: query.assetIn,
        percent: query.percent,
      });
    }
  } else if (query.bondOut != undefined) {
    try {
      const maturity = new Uint256(query.maturity);
      const currentTime = getCurrentTime();
      const assetIn = new Uint112(query.assetIn);
      const bondOut = new Uint128(query.bondOut);

      const { claims, yDecrease } = await pool.calculateLendGivenBond(
        assetIn,
        bondOut,
        currentTime
      );
      const insuranceOut = claims.insurance.toString();

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
      const { claims: claimsSlippage } = await pool.calculateLendGivenBond(
        assetIn,
        bondOut,
        timeSlippage
      );

      const minInsurance = calculateMinValue(
        claimsSlippage.insurance,
        query.slippage
      ).toString();

      const apr = calculateApr(bondOut, query.assetIn, maturity, currentTime);
      const cf = calculateCf(
        query.assetIn,
        whitelist,
        query.collateral,
        claims.insurance
      );

      app.ports.sdkLendMsg.send({
        asset: query.asset,
        collateral: query.collateral,
        maturity: query.maturity,
        assetIn: query.assetIn,
        bondOut: query.bondOut,
        result: {
          percent: Number(percent.toBigInt()),
          insuranceOut,
          minInsurance,
          apr: Number(apr.toBigInt()) / 10000,
          cf,
        },
      });
    } catch {
      app.ports.sdkLendMsg.send({
        asset: query.asset,
        collateral: query.collateral,
        maturity: query.maturity,
        assetIn: query.assetIn,
        bondOut: query.bondOut,
      });
    }
  } else if (query.insuranceOut != undefined) {
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

      const apr = calculateApr(
        claims.bond,
        query.assetIn,
        maturity,
        currentTime
      );
      const cf = calculateCf(
        query.assetIn,
        whitelist,
        query.collateral,
        query.insuranceOut
      );

      app.ports.sdkLendMsg.send({
        asset: query.asset,
        collateral: query.collateral,
        maturity: query.maturity,
        assetIn: query.assetIn,
        insuranceOut: query.insuranceOut,
        result: {
          percent: Number(percent.toBigInt()),
          bondOut,
          minBond,
          apr: Number(apr.toBigInt()) / 10000,
          cf,
        },
      });
    } catch {
      app.ports.sdkLendMsg.send({
        asset: query.asset,
        collateral: query.collateral,
        maturity: query.maturity,
        assetIn: query.assetIn,
        insuranceOut: query.insuranceOut,
      });
    }
  }
}

function calculateApr(
  bond: Uint128,
  assetIn: string,
  maturity: Uint256,
  currentTime: Uint256
): Uint128 {
  const SECONDS = 31556926;
  return bond
    .sub(assetIn)
    .mul(SECONDS)
    .mul(10000)
    .div(assetIn)
    .div(maturity.sub(currentTime));
}

function calculateCf(
  assetIn: string,
  whitelist: WhiteList,
  collateral: string,
  insuranceOut: Uint128 | string
): string {
  return new Uint256(assetIn)
    .mul(pow(10n, BigInt(whitelist.getToken(collateral).decimals)))
    .div(insuranceOut)
    .toString();
}

function calculateMinValue(value: Uint128, slippage: number): Uint256 {
  return new Uint256(value).mul(10000 * (1 - slippage)).div(10000);
}

async function calculatePercent(
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

function pow(a: bigint, b: bigint) {
  let result = 1n;
  for (let i = 0n; i < b; i++) result *= a;
  return result;
}
