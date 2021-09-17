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

  app.ports.borrow.subscribe(async (params) => {
    const pool = whitelist.getPool(
      params.asset,
      params.collateral,
      params.maturity
    );

    const txn = await pool.upgrade(gp.metamaskSigner!).borrowGivenPercent({
      assetTo: params.assetTo,
      dueTo: params.dueTo,
      assetOut: new Uint112(params.assetOut),
      percent: new Uint40(params.percent),
      maxDebt: new Uint112(params.maxDebt),
      maxCollateral: new Uint112(params.maxCollateral),
      deadline: new Uint256(params.deadline),
    });

    await txn.wait();
  });
}

async function borrowQueryCalculation(
  app: ElmApp<Ports>,
  whitelist: WhiteList,
  query: BorrowQuery
) {
  const pool = whitelist.getPool(query.asset, query.collateral, query.maturity);

  if (query.percent != undefined) {
    try {
      const maturity = new Uint256(query.maturity);
      const currentTime = new Uint256(Date.now()).div(1000);

      const { due } = await pool.calculateBorrowGivenPercent(
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
        result: {
          debtIn,
          collateralIn,
          maxDebt,
          maxCollateral,
          apr: Number(apr.value) / 10000,
          cf,
        },
      });
    } catch {
      app.ports.sdkBorrowMsg.send({
        asset: query.asset,
        collateral: query.collateral,
        maturity: query.maturity,
        assetOut: query.assetOut,
        percent: query.percent,
      });
    }
  } else if (query.debtIn != undefined) {
    try {
      const maturity = new Uint256(query.maturity);
      const currentTime = new Uint256(Date.now()).div(1000);
      const assetOut = new Uint112(query.assetOut);
      const debtIn = new Uint112(query.debtIn);

      const { due, yIncrease } = await pool.calculateBorrowGivenDebt(
        assetOut,
        debtIn,
        currentTime
      );
      const collateralIn = due.collateral.value.toString();

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
      const percent = new Uint256(yIncrease)
        .sub(yMin)
        .mul(1n << 32n)
        .div(yMax.sub(yMin));

      const maxCollateral = new Uint256(due.collateral)
        .mul(10000 * (1 + query.slippage))
        .div(10000)
        .value.toString();

      const SECONDS = 31556926;
      const apr = debtIn
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
        debtIn: query.debtIn,
        result: {
          percent: Number(percent.value),
          collateralIn,
          maxCollateral,
          apr: Number(apr.value) / 10000,
          cf,
        },
      });
    } catch {
      app.ports.sdkBorrowMsg.send({
        asset: query.asset,
        collateral: query.collateral,
        maturity: query.maturity,
        assetOut: query.assetOut,
        debtIn: query.debtIn,
      });
    }
  } else if (query.collateralIn != undefined) {
    try {
      const maturity = new Uint256(query.maturity);
      const currentTime = new Uint256(Date.now()).div(1000);
      const assetOut = new Uint112(query.assetOut);

      const { due, yIncrease } = await pool.calculateBorrowGivenCollateral(
        assetOut,
        new Uint112(query.collateralIn),
        currentTime
      );
      const debtIn = due.debt.value.toString();

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
      const percent = new Uint256(yIncrease)
        .sub(yMin)
        .mul(1n << 32n)
        .div(yMax.sub(yMin));

      const maxDebt = new Uint256(due.debt)
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
        .div(query.collateralIn)
        .value.toString();

      app.ports.sdkBorrowMsg.send({
        asset: query.asset,
        collateral: query.collateral,
        maturity: query.maturity,
        assetOut: query.assetOut,
        collateralIn: query.collateralIn,
        result: {
          percent: Number(percent.value),
          debtIn,
          maxDebt,
          apr: Number(apr.value) / 10000,
          cf,
        },
      });
    } catch {
      app.ports.sdkBorrowMsg.send({
        asset: query.asset,
        collateral: query.collateral,
        maturity: query.maturity,
        assetOut: query.assetOut,
        collateralIn: query.collateralIn,
      });
    }
  }
}

function pow(a: bigint, b: bigint) {
  let result = 1n;
  for (let i = 0n; i < b; i++) result *= a;
  return result;
}
