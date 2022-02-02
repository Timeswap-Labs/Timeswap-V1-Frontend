import { Uint128, Pair, Uint112 } from "@timeswap-labs/timeswap-v1-sdk-core";
import { GlobalParams } from "../global";
import { getPool, getPoolSDK } from "../helper";
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

  app.ports.querySum.subscribe((claimsData) => {
    const bond = new Uint128(claimsData.claim.bondInterest).add(new Uint128(claimsData.claim.bondPrincipal));
    const insurance = new Uint128(claimsData.claim.insuranceInterest).add(new Uint128(claimsData.claim.insurancePrincipal));

    app.ports.receiveSum.send({
      ...claimsData,
      result: {
        asset: bond.toString(),
        collateral: insurance.toString()
      }
    });
  });

  app.ports.queryClaim.subscribe((claimsData) => {
    const reserves = {
      asset: new Uint128(claimsData.poolInfo.assetReserve),
      collateral: new Uint128(claimsData.poolInfo.collateralReserve)
    }

    const totalClaims = {
      bondInterest: new Uint112(claimsData.poolInfo.totalBondInterest),
      bondPrincipal: new Uint112(claimsData.poolInfo.totalBondPrincipal),
      insuranceInterest: new Uint112(claimsData.poolInfo.totalInsuranceInterest),
      insurancePrincipal: new Uint112(claimsData.poolInfo.totalInsurancePrincipal)
    }

    const claimsIn = {
      bondInterest: new Uint112(claimsData.claimsIn.bondInterest),
      bondPrincipal: new Uint112(claimsData.claimsIn.bondPrincipal),
      insuranceInterest: new Uint112(claimsData.claimsIn.insuranceInterest),
      insurancePrincipal: new Uint112(claimsData.claimsIn.insurancePrincipal)
    }

    const { asset, collateral } = Pair.calculateWithdraw(reserves, totalClaims, claimsIn)

    app.ports.receiveReturn.send({
      ...claimsData,
      result: {
        asset: asset.toString(),
        collateral: collateral.toString()
      }
    });
  });
}

export function lendSigner(
  app: ElmApp<Ports>,
  gp: GlobalParams
) {
  app.ports.lend.subscribe(async (params) => {
    const pool = getPoolSDK(gp, params.send.asset, params.send.collateral, params.send.maturity, params.chain);
    const { percent, bondOut, insuranceOut, minBond, minInsurance } = params.send;
    let txnConfirmation;

    try {
      if (
        percent !== undefined &&
        minBond !== undefined &&
        minInsurance !== undefined
      ) {
        txnConfirmation = await percentTransaction(pool, gp, {
          ...params.send,
          percent,
          minBond,
          minInsurance,
        });
      } else if (bondOut !== undefined && minInsurance !== undefined) {
        txnConfirmation = await bondTransaction(pool, gp, {
          ...params.send,
          bondOut,
          minInsurance,
        });
      } else if (insuranceOut !== undefined && minBond !== undefined) {
        txnConfirmation = await insuranceTransaction(pool, gp, {
          ...params.send,
          insuranceOut,
          minBond,
        });
      }

      if (txnConfirmation) {
        app.ports.receiveConfirm.send({
          id: params.id,
          chain: params.chain,
          address: params.address,
          hash: txnConfirmation.hash || null
        });

        await txnConfirmation.wait();
      }
    } catch (error) {
      app.ports.receiveConfirm.send({
        id: params.id,
        chain: params.chain,
        address: params.address,
        hash: null
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
