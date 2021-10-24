import { ERC20Token } from "@timeswap-labs/timeswap-v1-sdk";
import {
  Uint112,
  Uint128,
  Uint256,
  Uint40,
} from "@timeswap-labs/timeswap-v1-sdk-core";
import { GlobalParams } from "../global";
import { WhiteList } from "../whitelist";
import { bondCalculate } from "./bond";
import { insuranceCalculate } from "./insurance";
import { percentCalculate } from "./percent";

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
  const { percent, bondOut, insuranceOut } = query;

  if (percent !== undefined) {
    await percentCalculate(app, whitelist, pool, { ...query, percent });
  } else if (bondOut !== undefined) {
    await bondCalculate(app, whitelist, pool, { ...query, bondOut });
  } else if (insuranceOut !== undefined) {
    await insuranceCalculate(app, whitelist, pool, { ...query, insuranceOut });
  }
}
