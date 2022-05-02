import { GlobalParams } from '../global';
import { Pool as SDKPool } from "@timeswap-labs/timeswap-v1-sdk";
import { Pool, Uint112, Uint256 } from '@timeswap-labs/timeswap-v1-sdk-core';
import { calculateMaxValue, calculateMinValue, getCurrentTime } from "../helper";
import { calculateFuturisticApr, calculateFuturisticCdp } from './common';


export async function collateralCalculate(
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
    } = pool.liquidityGivenCollateral(
      state,
      new Uint256(query.poolInfo.totalLiquidity),
      new Uint112(query.collateralIn!),
      new Uint256(currentTime),
      new Uint256(query.poolInfo.feeStored),
    );

    const debtIn = dueOut.debt.toString();
    const collateralIn = dueOut.collateral.toString();
    const minLiquidity = calculateMinValue(liquidityOut, query.slippage);
    const maxDebt = calculateMaxValue(dueOut.debt, query.slippage);
    const maxAsset = calculateMaxValue(assetInReturn, query.slippage);

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

    app.ports.receiveAddLiqAnswer.send({
      ...query,
      result: {
        assetIn: assetInReturn.toString(),
        debtIn,
        liquidityOut: liquidityOut.toString(),
        minLiquidity: minLiquidity.toString(),
        maxAsset: maxAsset.toString(),
        maxDebt: maxDebt.toString(),
        apr: futureApr,
        cdp: futureCdp,
      },
    });
  } catch(err) {
    console.log(err);

    app.ports.receiveAddLiqAnswer.send({
      ...query,
      result: 0,
    });
  }
}

export async function collateralTransaction(
  pool: SDKPool,
  gp: GlobalParams,
  liquidity: Liquidity
) {
  return await pool.upgrade(gp.walletSigner!).liquidityGivenCollateral({
    liquidityTo: liquidity.liquidityTo,
    dueTo: liquidity.dueTo,
    collateralIn: new Uint112(liquidity.collateralIn),
    minLiquidity: new Uint256(liquidity.minLiquidity),
    maxAsset: new Uint112(liquidity.maxAsset),
    maxDebt: new Uint112(liquidity.maxDebt),
    deadline: new Uint256(liquidity.deadline),
  });
}

interface Liquidity {
  liquidityTo: string;
  dueTo: string;
  collateralIn: string;
  minLiquidity: string;
  maxAsset: string;
  maxDebt: string;
  deadline: number;
}
