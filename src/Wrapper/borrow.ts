import { ERC20Token } from "@timeswap-labs/timeswap-v1-sdk";
import { Uint112, Uint256, Uint40 } from "@timeswap-labs/timeswap-v1-sdk-core";
import { GlobalParams } from "./global";
import { WhiteList } from "./whitelist";

export function borrow(app: ElmApp<Ports>, whitelist: WhiteList) {
  app.ports.queryBorrow.subscribe((query) =>
    borrowQueryCalculation(app, whitelist, query)
  );

  app.ports.queryBorrowPerSecond.subscribe((query) =>
    borrowQueryCalculation(app, whitelist, query)
  );
}

export async function borrowSigner(
  app: ElmApp<Ports>,
  whitelist: WhiteList,
  gp: GlobalParams
) {
  app.ports.approveBorrow.subscribe(async ({ erc20 }) => {
    (whitelist.getToken(erc20) as ERC20Token)
      .upgrade(gp.metamaskSigner!)
      .approve(whitelist.convenience, new Uint256((1n << 256n) - 1n));
  });

  //   app.ports.borrow.subscribe(async (params) => {
  //     const pool = whitelist.getPool(
  //       params.asset,
  //       params.collateral,
  //       params.maturity
  //     );

  //     pool.upgrade(gp.metamaskSigner!).borrowGivenPercent({
  //       bondTo: params.bondTo,
  //       insuranceTo: params.insuranceTo,
  //       assetIn: new Uint112(params.assetIn),
  //       percent: new Uint40(params.percent),
  //       minBond: new Uint128(params.minBond),
  //       minInsurance: new Uint128(params.minInsurance),
  //       deadline: new Uint256(params.deadline),
  //     });
  //   });
}

async function borrowQueryCalculation(
  app: ElmApp<Ports>,
  whitelist: WhiteList,
  query: BorrowQuery
) {
  const pool = whitelist.getPool(query.asset, query.collateral, query.maturity);

  if (query.percent != undefined) {
    const maturity = new Uint256(query.maturity);
    const currentTime = new Uint256(Date.now()).div(1000);

    const due = await pool.calculateBorrowGivenPercent(
      new Uint112(query.assetOut),
      new Uint40(query.percent),
      currentTime
    );
    const debtIn = due.debt.value.toString();
    const collateralIn = due.collateral.value.toString();

    const maxDebt = new Uint256(due.debt)
      .mul(10000 * (1 + query.slippage))
      .div(10000)
      .value.toString();
    const maxCollateral = new Uint256(due.collateral)
      .mul(10000 * (1 + query.slippage))
      .div(10000)
      .value.toString();

    const SECONDS = 31556926;
    const apr = due.debt
      .sub(query.assetOut)
      .mul(SECONDS)
      .mul(10000)
      .div(query.assetOut)
      .div(maturity.sub(currentTime));
    const cf = new Uint256(query.assetOut)
      .mul(pow(10n, BigInt(whitelist.getToken(query.collateral).decimals)))
      .div(due.collateral)
      .value.toString();

    app.ports.sdkBorrowMsg.send({
      asset: query.asset,
      collateral: query.collateral,
      maturity: query.maturity,
      assetOut: query.assetOut,
      percent: query.percent,
      debtIn,
      collateralIn,
      maxDebt,
      maxCollateral,
      apr: Number(apr.value) / 10000,
      cf,
    });
  }
  //   else if (query.bondOut) {
  //     const claims = await pool.calculateLendGivenBond(
  //       new Uint112(query.assetIn),
  //       new Uint128(query.bondOut),
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
  //   } else if (query.insuranceOut) {
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
