import { ERC20Token, Pool } from "@timeswap-labs/timeswap-v1-sdk";
import { Uint112, Uint256, Uint40 } from "@timeswap-labs/timeswap-v1-sdk-core";
import { GlobalParams } from "./global";
import { getCurrentTime } from "./helper";
import { WhiteList } from "./whitelist";

export function borrow(app: ElmApp<Ports>, whitelist: WhiteList) {
  app.ports.queryBorrow.subscribe((query) =>
    borrowQueryCalculation(app, whitelist, query)
  );

  app.ports.queryBorrowPerSecond.subscribe((query) =>
    borrowQueryCalculation(app, whitelist, query)
  );
}

export function borrowSigner(
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
      const currentTime = getCurrentTime();

      const { due } = await pool.calculateBorrowGivenPercent(
        new Uint112(query.assetOut),
        new Uint40(query.percent),
        currentTime
      );
      const debtIn = due.debt.value.toString();
      const collateralIn = due.collateral.value.toString();

      const maxDebt = calculateMaxValue(
        due.debt.sub(query.assetOut),
        query.slippage
      )
        .add(query.assetOut)
        .value.toString();
      const maxCollateral = calculateMaxValue(
        due.collateral,
        query.slippage
      ).value.toString();

      const apr = calculateApr(due.debt, query.assetOut, maturity, currentTime);
      const cf = calculateCf(
        query.assetOut,
        whitelist,
        query.collateral,
        due.collateral
      );

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
      const currentTime = getCurrentTime();
      const assetOut = new Uint112(query.assetOut);
      const debtIn = new Uint112(query.debtIn);

      const { due, yIncrease } = await pool.calculateBorrowGivenDebt(
        assetOut,
        debtIn,
        currentTime
      );
      const collateralIn = due.collateral.value.toString();

      const percent = await calculatePercent(
        pool,
        assetOut,
        yIncrease,
        currentTime
      );

      const maxCollateral = calculateMaxValue(
        due.collateral,
        query.slippage
      ).value.toString();

      const apr = calculateApr(debtIn, query.assetOut, maturity, currentTime);
      const cf = calculateCf(
        query.assetOut,
        whitelist,
        query.collateral,
        collateralIn
      );

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
      const currentTime = getCurrentTime();
      const assetOut = new Uint112(query.assetOut);

      const { due, yIncrease } = await pool.calculateBorrowGivenCollateral(
        assetOut,
        new Uint112(query.collateralIn),
        currentTime
      );
      const debtIn = due.debt.value.toString();

      const percent = await calculatePercent(
        pool,
        assetOut,
        yIncrease,
        currentTime
      );

      const maxDebt = calculateMaxValue(
        due.debt.sub(query.assetOut),
        query.slippage
      )
        .add(query.assetOut)
        .value.toString();

      const apr = calculateApr(due.debt, query.assetOut, maturity, currentTime);
      const cf = calculateCf(
        query.assetOut,
        whitelist,
        query.collateral,
        query.collateralIn
      );

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

function calculateApr(
  debt: Uint112,
  assetOut: string,
  maturity: Uint256,
  currentTime: Uint256
): Uint112 {
  const SECONDS = 31556926;
  return debt
    .sub(assetOut)
    .mul(SECONDS)
    .mul(10000)
    .div(assetOut)
    .div(maturity.sub(currentTime));
}

function calculateCf(
  assetOut: string,
  whitelist: WhiteList,
  collateral: string,
  collateralIn: Uint112 | string
): string {
  return new Uint256(assetOut)
    .mul(pow(10n, BigInt(whitelist.getToken(collateral).decimals)))
    .div(collateralIn)
    .value.toString();
}

function calculateMaxValue(value: Uint112, slippage: number): Uint256 {
  return new Uint256(value).mul(10000 * (1 + slippage)).div(10000);
}

async function calculatePercent(
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
function pow(a: bigint, b: bigint) {
  let result = 1n;
  for (let i = 0n; i < b; i++) result *= a;
  return result;
}
