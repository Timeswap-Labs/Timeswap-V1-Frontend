import { ERC20Token } from "@timeswap-labs/timeswap-v1-sdk";
import { Uint112, Uint256, Uint40 } from "@timeswap-labs/timeswap-v1-sdk-core";
import { GlobalParams } from "../global";
import { WhiteList } from "../whitelist";
import { collateralCalculate } from "./collateral";
import { debtCalculate } from "./debt";
import { percentCalculate } from "./percent";

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
  const { percent, debtIn, collateralIn } = query;

  if (percent !== undefined) {
    await percentCalculate(app, whitelist, pool, { ...query, percent });
  } else if (debtIn !== undefined) {
    await debtCalculate(app, whitelist, pool, { ...query, debtIn });
  } else if (collateralIn !== undefined) {
    await collateralCalculate(app, whitelist, pool, { ...query, collateralIn });
  }
}
