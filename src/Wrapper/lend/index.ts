import { Uint112 } from '@timeswap-labs/timeswap-v1-sdk-core';
import { GlobalParams } from './../global';
import { getPool, getPoolSDK, handleTxnErrors, updateCachedTxns } from "../helper";
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
          hash: txnConfirmation.hash
        });

        const txnReceipt = await txnConfirmation.wait();
        const receiveReceipt = {
          chain: params.chain,
          address: params.address,
          hash: txnConfirmation.hash,
          state: txnReceipt.status ? "success" : "failed"
        }
        app.ports.receiveReceipt.send(receiveReceipt);
        updateCachedTxns(receiveReceipt);
      }
    } catch (error: any) {
      handleTxnErrors(error, app, gp, params);
    }
  });
}

async function lendQueryCalculation(
  app: ElmApp<Ports>,
  query: LendQuery,
) {
  // Asset Overflow check
  try {
    const assetIn = new Uint112(query.assetIn);
    const stateX = new Uint112(query.poolInfo.x);
    stateX.add(assetIn);
  } catch (error) {
    app.ports.receiveLendAnswer.send({
      ...query,
      result: 5,
    });
  }

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
