import { ERC20Token } from "@timeswap-labs/timeswap-v1-sdk";
import {
  Uint112,
  Uint128,
  Uint256,
  Uint40,
} from "@timeswap-labs/timeswap-v1-sdk-core";
import { GlobalParams } from "./global";
import { WhiteList } from "./whitelist";

export function lend(app: ElmApp<Ports>, whitelist: WhiteList) {
  app.ports.queryLend.subscribe((query) =>
    lendQueryCalculation(app, whitelist, query)
  );

  app.ports.queryLendPerSecond.subscribe((query) =>
    lendQueryCalculation(app, whitelist, query)
  );
}

export async function lendSigner(
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

    pool.upgrade(gp.metamaskSigner!).lendGivenPercent({
      bondTo: params.bondTo,
      insuranceTo: params.insuranceTo,
      assetIn: new Uint112(params.assetIn),
      percent: new Uint40(params.percent),
      minBond: new Uint128(params.minBond),
      minInsurance: new Uint128(params.minInsurance),
      deadline: new Uint256(params.deadline),
    });
  });
}

async function lendQueryCalculation(
  app: ElmApp<Ports>,
  whitelist: WhiteList,
  query: LendQuery
) {
  const pool = whitelist.getPool(query.asset, query.collateral, query.maturity);

  if (query.percent != undefined) {
    const maturity = new Uint256(query.maturity);
    const currentTime = new Uint256(Date.now()).div(1000);

    const claims = await pool.calculateLendGivenPercent(
      new Uint112(query.assetIn),
      new Uint40(query.percent),
      currentTime
    );
    const bondOut = claims.bond.value.toString();
    const insuranceOut = claims.insurance.value.toString();

    const minBond = new Uint256(claims.bond)
      .mul(10000 * (1 - query.slippage))
      .div(10000)
      .value.toString();
    const minInsurance = new Uint256(claims.insurance)
      .mul(10000 * (1 - query.slippage))
      .div(10000)
      .value.toString();

    const SECONDS = 31556926;
    const apr = claims.bond
      .sub(query.assetIn)
      .mul(SECONDS)
      .mul(10000)
      .div(query.assetIn)
      .div(maturity.sub(currentTime));
    const cf = new Uint256(query.assetIn)
      .mul(pow(10n, BigInt(whitelist.getToken(query.collateral).decimals)))
      .div(claims.insurance)
      .value.toString();

    app.ports.sdkLendMsg.send({
      asset: query.asset,
      collateral: query.collateral,
      maturity: query.maturity,
      assetIn: query.assetIn,
      percent: query.percent,
      bondOut,
      insuranceOut,
      minBond,
      minInsurance,
      apr: Number(apr.value) / 10000,
      cf,
    });
  }
  // else if (query.bondOut) {
  //   const maturity = new Uint256(query.maturity);
  //   const currentTime = new Uint256(Date.now()).div(1000);

  //   const claims = await pool.calculateLendGivenBond(
  //     new Uint112(query.assetIn),
  //     new Uint128(query.bondOut),
  //     new Uint256(Date.now()).div(1000)
  //   );
  //   const insuranceOut = claims.insurance.value.toString();

  //   const minBond = new Uint256(claims.bond)
  //     .mul(100000 * query.slippage)
  //     .div(100000)
  //     .value.toString();
  //   const minInsurance = new Uint256(claims.insurance)
  //     .mul(100000 * query.slippage)
  //     .div(100000)
  //     .value.toString();

  //   const SECONDS = 31556926;
  //   const apr = claims.bond
  //     .sub(query.assetIn)
  //     .mul(SECONDS)
  //     .mul(10000)
  //     .div(query.assetIn)
  //     .div(maturity.sub(currentTime));
  //   const cf = new Uint256(query.assetIn)
  //     .mul(pow(10n, BigInt(whitelist.getToken(query.collateral).decimals)))
  //     .div(claims.insurance)
  //     .value.toString();

  //   app.ports.sdkLendMsg.send({
  //     asset: query.asset,
  //     collateral: query.collateral,
  //     maturity: query.maturity,
  //     assetIn: query.assetIn,
  //     percent: 1452345,
  //     bondOut: query.bondOut,
  //     insuranceOut,
  //     minBond,
  //     minInsurance,
  //     apr: Number(apr.value) / 10000,
  //     cf,
  //   });
  // }
  // else if (query.insuranceOut) {
  //     const claims = await pool.calculateLendGivenInsurance(
  //       new Uint112(query.assetIn),
  //       new Uint128(query.insuranceOut),
  //       new Uint256(Date.now()).div(1000)
  //     );
  //     const bondOut = claims.bond.value.toString();
  //     const insuranceOut = claims.insurance.value.toString();

  //     const minBond = new Uint256(claims.bond)
  //       .mul(100000 * query.slippage)
  //       .div(100000)
  //       .value.toString();
  //     const minInsurance = new Uint256(claims.insurance)
  //       .mul(100000 * query.slippage)
  //       .div(100000)
  //       .value.toString();

  //     const SECONDS = 31556926;
  //     // const apr = claims.bond.sub(query.assetIn).mul(SECONDS).div(query.assetIn * (query.maturity - now));
  //     // const cf = assetIn * (10 ** collateral.decimals)/claims.insurance;
  //     // const cf = assetIn * (10 ** collateral.decimals)/claims.insurance;

  //     app.ports.sdkLendMsg.send({
  //       asset: query.asset,
  //       collateral: query.collateral,
  //       maturity: query.maturity,
  //       assetIn: query.assetIn,
  //       percent: query.percent,
  //       bondOut,
  //       insuranceOut,
  //       minBond,
  //       minInsurance,
  //       apr: 0.35,
  //       cf: "1234523423",
  //     });
  //   }
}

function pow(a: bigint, b: bigint) {
  let result = 1n;
  for (let i = 0n; i < b; i++) result *= a;
  return result;
}
