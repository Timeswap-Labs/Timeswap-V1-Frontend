import { GlobalParams } from './../global';
import { Pool as SDKPool } from "@timeswap-labs/timeswap-v1-sdk";
import { Pool, Uint112, Uint256 } from '@timeswap-labs/timeswap-v1-sdk-core';
import { calculateMaxValue, calculateMinValue, getCurrentTime } from "../helper";
import { calculateApr, calculateCdp, calculateFuturisticApr, calculateFuturisticCdp } from './common';

export async function assetCalculate(
  app: ElmApp<Ports>,
  pool: Pool,
  query: LiquidityQuery
) {
  try {
    const maturity = new Uint256(query.pool.maturity);
    const currentTime = getCurrentTime();

    const state = {
      x: new Uint112(query.poolInfo.x),
      y: new Uint112(query.poolInfo.y),
      z: new Uint112(query.poolInfo.z),
    };

    const {
      assetIn: assetInReturn,
      liquidityOut,
      dueOut,
      xIncrease,
      yIncrease,
      zIncrease,
    } = pool.liquidityGivenAsset(
      state,
      new Uint256(query.poolInfo.totalLiquidity),
      new Uint112(query.assetIn!),
      new Uint256(currentTime),
      new Uint256(query.poolInfo.feeStored),
    );

    const debtIn = dueOut.debt.toString();
    const collateralIn = dueOut.collateral.toString();
    const minLiquidity = calculateMinValue(liquidityOut, query.slippage);
    const maxDebt = calculateMaxValue(dueOut.debt, query.slippage);
    const maxCollateral = calculateMaxValue(dueOut.collateral, query.slippage);

    const futureApr = calculateFuturisticApr(state, xIncrease, yIncrease);
    const futureCdp = calculateFuturisticCdp(
      state,
      query.pool.asset.decimals,
      query.pool.collateral.decimals,
      xIncrease,
      zIncrease,
      query.poolInfo.assetSpot,
      query.poolInfo.collateralSpot
    );

    query.pool.maturity = query.pool.maturity.toString();

    app.ports.receiveAddLiqAnswer.send({
      ...query,
      result: {
        debtIn,
        collateralIn,
        liquidityOut: liquidityOut.toString(),
        minLiquidity: minLiquidity.toString(),
        maxDebt: maxDebt.toString(),
        maxCollateral: maxCollateral.toString(),
        apr: futureApr,
        cdp: futureCdp,
      }
    });
  } catch (err) {
    console.log("err", err);

    app.ports.receiveAddLiqAnswer.send({
      ...query,
      result: 0,
    });
  }
}

export async function assetTransaction(
  pool: SDKPool,
  gp: GlobalParams,
  liquidity: Liquidity
) {
  return await pool.upgrade(gp.walletSigner!).liquidityGivenAsset({
    liquidityTo: liquidity.liquidityTo,
    dueTo: liquidity.dueTo,
    assetIn: new Uint112(liquidity.assetIn),
    minLiquidity: new Uint256(liquidity.minLiquidity),
    maxDebt: new Uint112(liquidity.maxDebt),
    maxCollateral: new Uint112(liquidity.maxCollateral),
    deadline: new Uint256(liquidity.deadline),
  });
}

interface Liquidity {
  liquidityTo: string;
  dueTo: string;
  assetIn: string;
  minLiquidity: string;
  maxDebt: string;
  maxCollateral: string;
  deadline: number;
}
