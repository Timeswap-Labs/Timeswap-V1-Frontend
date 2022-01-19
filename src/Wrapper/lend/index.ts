import { Uint256, ERC20Token } from "@timeswap-labs/timeswap-v1-sdk-core";
import { GlobalParams } from "../global";
import { getPool } from "../helper";
import { WhiteList } from "../whitelist";
import { bondCalculate, bondTransaction } from "./bond";
import { insuranceCalculate, insuranceTransaction } from "./insurance";
import { percentCalculate, percentTransaction } from "./percent";

export function lend(app: ElmApp<Ports>) {
  app.ports.queryLend.subscribe((query) => {
    lendQueryCalculation(app, query)
  });

  app.ports.queryLendPerSecond.subscribe((query) =>
    lendQueryCalculation(app, query)
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
    const { percent, bondOut, insuranceOut, minBond, minInsurance } = params;

    if (
      percent !== undefined &&
      minBond !== undefined &&
      minInsurance !== undefined
    ) {
      await percentTransaction(pool, gp, {
        ...params,
        percent,
        minBond,
        minInsurance,
      });
    } else if (bondOut !== undefined && minInsurance !== undefined) {
      await bondTransaction(pool, gp, {
        ...params,
        bondOut,
        minInsurance,
      });
    } else if (insuranceOut !== undefined && minBond !== undefined) {
      await insuranceTransaction(pool, gp, {
        ...params,
        insuranceOut,
        minBond,
      });
    }
  });
}

async function lendQueryCalculation(
  app: ElmApp<Ports>,
  query: LendQuery,
) {
  const pool = getPool(query);
  const { percent, bondOut, insuranceOut } = query;

  if (percent !== undefined) {
    percentCalculate(app, pool, { ...query, percent });
  } else if (bondOut !== undefined) {
    await bondCalculate(app, pool, { ...query, bondOut });
  } else if (insuranceOut !== undefined) {
    await insuranceCalculate(app, pool, { ...query, insuranceOut });
  }
}
