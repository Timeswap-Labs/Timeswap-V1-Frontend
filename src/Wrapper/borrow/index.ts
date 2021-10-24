import { ERC20Token } from "@timeswap-labs/timeswap-v1-sdk";
import { Uint256 } from "@timeswap-labs/timeswap-v1-sdk-core";
import { GlobalParams } from "../global";
import { WhiteList } from "../whitelist";
import { collateralCalculate, collateralTransaction } from "./collateral";
import { debtCalculate, debtTransaction } from "./debt";
import { percentCalculate, percentTransaction } from "./percent";

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
    const { percent, debtIn, collateralIn, maxDebt, maxCollateral } = params;

    if (
      percent !== undefined &&
      maxDebt !== undefined &&
      maxCollateral !== undefined
    ) {
      await percentTransaction(pool, gp, {
        ...params,
        percent,
        maxDebt,
        maxCollateral,
      });
    } else if (debtIn !== undefined && maxCollateral !== undefined) {
      await debtTransaction(pool, gp, {
        ...params,
        debtIn,
        maxCollateral,
      });
    } else if (collateralIn !== undefined && maxDebt !== undefined) {
      await collateralTransaction(pool, gp, {
        ...params,
        collateralIn,
        maxDebt,
      });
    }
  });
}

async function borrowQueryCalculation(
  app: ElmApp<Ports>,
  whitelist: WhiteList,
  query: BorrowQuery
) {
  const pool = whitelist.getPool(query.asset, query.collateral, query.maturity);
  const { percent, debtIn, collateralIn } = query;

  if (percent !== undefined) {
    await percentCalculate(app, whitelist, pool, { ...query, percent });
  } else if (debtIn !== undefined) {
    await debtCalculate(app, whitelist, pool, { ...query, debtIn });
  } else if (collateralIn !== undefined) {
    await collateralCalculate(app, whitelist, pool, { ...query, collateralIn });
  }
}
