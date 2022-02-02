import { GlobalParams } from "../global";
import { getPool, getPoolSDK } from "../helper";
import { collateralCalculate, collateralTransaction } from "./collateral";
import { debtCalculate, debtTransaction } from "./debt";
import { percentCalculate, percentTransaction } from "./percent";

export function borrow(app: ElmApp<Ports>) {
  app.ports.queryBorrow.subscribe((query) =>
    borrowQueryCalculation(app, query)
  );

  app.ports.queryBorrowPerSecond.subscribe((query) =>
    borrowQueryCalculation(app, query)
  );
}

export function borrowSigner(
  app: ElmApp<Ports>,
  gp: GlobalParams
) {
  app.ports.borrow.subscribe(async (params) => {
    const pool = getPoolSDK(gp, params.send.asset, params.send.collateral, params.send.maturity, params.chain);
    const { percent, debtIn, collateralIn, maxDebt, maxCollateral } = params.send;

    if (
      percent !== undefined &&
      maxDebt !== undefined &&
      maxCollateral !== undefined
    ) {
      await percentTransaction(pool, gp, {
        ...params.send,
        percent,
        maxDebt,
        maxCollateral,
      });
    } else if (debtIn !== undefined && maxCollateral !== undefined) {
      await debtTransaction(pool, gp, {
        ...params.send,
        debtIn,
        maxCollateral,
      });
    } else if (collateralIn !== undefined && maxDebt !== undefined) {
      await collateralTransaction(pool, gp, {
        ...params.send,
        collateralIn,
        maxDebt,
      });
    }
  });
}

async function borrowQueryCalculation(
  app: ElmApp<Ports>,
  query: BorrowQuery
) {
  const pool = getPool(query);
  const { percent, debtIn, collateralIn } = query;

  if (percent !== undefined) {
    await percentCalculate(app, pool, { ...query, percent });
  } else if (debtIn !== undefined) {
    await debtCalculate(app, pool, { ...query, debtIn });
  } else if (collateralIn !== undefined) {
    await collateralCalculate(app, pool, { ...query, collateralIn });
  }
}
