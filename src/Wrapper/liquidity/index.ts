import { GlobalParams } from './../global';
import { compareConvAddress, getPool, getPoolSDK, handleTxnErrors, updateCachedTxns } from '../helper';
import { assetCalculate, assetTransaction } from './asset';
import { collateralCalculate, collateralTransaction } from './collateral';
import { debtCalculate, debtTransaction } from './debt';
import { newLiquidityCalculate } from './new';
import { Uint112, Uint256 } from '@timeswap-labs/timeswap-v1-biconomy-sdk';

export function liquidity(app: ElmApp<Ports>) {
  app.ports.queryLiquidity.subscribe((query) => {
    liquidityQueryCalculation(app, query)
  });

  app.ports.queryLiquidityPerSecond.subscribe((query) =>
    liquidityQueryCalculation(app, query)
  );

  app.ports.queryCreate.subscribe((query) => {
    newLiquidityQueryCalculation(app, query)
  });
}

export function liquiditySigner(
  app: ElmApp<Ports>,
  gp: GlobalParams
) {
  app.ports.liquidity.subscribe(async (params) => {
    const pool = getPoolSDK(gp, params.send.asset, params.send.collateral, params.send.maturity, params.chain);
    const { assetIn, debtIn, collateralIn, maxAsset, maxDebt, maxCollateral } = params.send;
    let txnConfirmation;

    try {
      if (
        assetIn !== undefined &&
        maxDebt !== undefined &&
        maxCollateral !== undefined
      ) {
        txnConfirmation = await assetTransaction(pool, gp, {
          ...params.send,
          assetIn,
          maxDebt,
          maxCollateral,
        });
      } else if (debtIn !== undefined && maxAsset !== undefined && maxCollateral !== undefined) {
        txnConfirmation = await debtTransaction(pool, gp, {
          ...params.send,
          debtIn,
          maxAsset,
          maxCollateral,
        });
      } else if (collateralIn !== undefined && maxAsset !== undefined && maxDebt !== undefined) {
        txnConfirmation = await collateralTransaction(pool, gp, {
          ...params.send,
          collateralIn,
          maxAsset,
          maxDebt
        });
      }

      if (txnConfirmation) {
        app.ports.receiveConfirm.send({
          id: params.id,
          chain: params.chain,
          address: params.address,
          hash: txnConfirmation.hash || null
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
    } catch (error) {
      handleTxnErrors(error, app, gp, params);
    }
  });

  // New Liquidity txn
  app.ports.create.subscribe(async (params) => {
    const pool = getPoolSDK(gp, params.send.asset, params.send.collateral, params.send.maturity, params.chain);
    const { assetIn, debtIn, collateralIn } = params.send;
    let txnConfirmation;

    try {
      if (
        assetIn !== undefined &&
        debtIn !== undefined &&
        collateralIn !== undefined
      ) {
        txnConfirmation = await pool.upgrade(await gp.getSigner()).newLiquidity({
          liquidityTo: params.send.liquidityTo,
          dueTo: params.send.dueTo,
          assetIn: new Uint112(params.send.assetIn!),
          debtIn: new Uint112(params.send.debtIn!),
          collateralIn: new Uint112(params.send.collateralIn!),
          deadline: new Uint256(params.send.deadline),
        });
      }

      if (txnConfirmation) {
        app.ports.receiveConfirm.send({
          id: params.id,
          chain: params.chain,
          address: params.address,
          hash: txnConfirmation.hash || null
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
    } catch (error) {
      handleTxnErrors(error, app, gp, params);
    }
  });
}

async function liquidityQueryCalculation(
  app: ElmApp<Ports>,
  query: LiquidityQuery,
) {
  // Asset Overflow check
  // try {
  //   const assetIn = new Uint112(query.assetIn);
  //   const stateX = new Uint112(query.poolInfo.x);
  //   stateX.add(assetIn);
  // } catch (error) {
  //   app.ports.receiveLendAnswer.send({
  //     ...query,
  //     result: 5,
  //   });
  // }

  compareConvAddress(query.poolInfo.convAddress, query.chain.chainId);

  const pool = getPool(query, query.poolInfo.fee, query.poolInfo.protocolFee);
  if (query.assetIn !== undefined) {
    await assetCalculate(app, pool, query);
  } else if (query.debtIn !== undefined) {
    await debtCalculate(app, pool, query);
  } else if (query.collateralIn !== undefined) {
    await collateralCalculate(app, pool, query);
  }
}

async function newLiquidityQueryCalculation(
  app: ElmApp<Ports>,
  query: NewLiquidityQuery,
) {
  const pool = getPool(query, 0, 0);
  await newLiquidityCalculate(app, pool, query)
}

