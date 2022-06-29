import { Uint112 } from '@timeswap-labs/timeswap-v1-sdk-core';
import { GlobalParams } from "../global";
import { compareConvAddress, getPool, getPoolSDK, handleTxnErrors, updateCachedTxns } from "../helper";
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
    borrowHandler(app, gp, params)
  });
}

export async function borrowHandler(
  app: ElmApp<Ports>,
  gp: GlobalParams,
  params: Borrow
) {
  const pool = getPoolSDK(gp, params.send.asset, params.send.collateral, params.send.maturity, params.chain);
  const { percent, debtIn, collateralIn, maxDebt, maxCollateral } = params.send;
  let txnConfirmation;

  try {
    if (
      percent !== undefined &&
      maxDebt !== undefined &&
      maxCollateral !== undefined
    ) {
      txnConfirmation = await percentTransaction(pool, gp, {
        ...params.send,
        percent,
        maxDebt,
        maxCollateral,
      });
    } else if (debtIn !== undefined && maxCollateral !== undefined) {
      txnConfirmation = await debtTransaction(pool, gp, {
        ...params.send,
        debtIn,
        maxCollateral,
      });
    } else if (collateralIn !== undefined && maxDebt !== undefined) {
      txnConfirmation = await collateralTransaction(pool, gp, {
        ...params.send,
        collateralIn,
        maxDebt,
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
        id: params.id,
        chain: params.chain,
        address: params.address,
        hash: txnConfirmation.hash,
        state: txnReceipt.status ? "success" : "failed"
      }
      app.ports.receiveReceipt.send(receiveReceipt);
      updateCachedTxns(receiveReceipt);
    }
  } catch (error) {
    handleTxnErrors(error, app, gp, params);
  }
}

function borrowQueryCalculation(
  app: ElmApp<Ports>,
  query: BorrowQuery
) {
  // Insufficient Asset in pool check
  const assetOut = new Uint112(query.assetOut);
  const stateX = new Uint112(query.poolInfo.x);

  if (assetOut.gt(stateX)) {
    app.ports.receiveBorrowAnswer.send({
      ...query,
      result: 5,
    });

    return;
  }

  compareConvAddress(query.poolInfo.convAddress, query.chain.chainId);

  const pool = getPool(query, query.poolInfo.fee, query.poolInfo.protocolFee);
  const { percent, debtIn, collateralIn } = query;

  if (percent !== undefined) {
    percentCalculate(app, pool, { ...query, percent });
  } else if (debtIn !== undefined) {
    debtCalculate(app, pool, { ...query, debtIn });
  } else if (collateralIn !== undefined) {
    collateralCalculate(app, pool, { ...query, collateralIn });
  }
}
