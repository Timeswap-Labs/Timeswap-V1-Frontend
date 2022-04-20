import { GlobalParams } from './../global';
import { Pool as SDKPool } from "@timeswap-labs/timeswap-v1-sdk";
import { Pool, Uint112, Uint256 } from '@timeswap-labs/timeswap-v1-sdk-core';
import { calculateMaxValue, calculateMinValue, getCurrentTime } from "../helper";
import { calculateFuturisticApr, calculateFuturisticCdp } from './common';


export async function debtCalculate(
  app: ElmApp<Ports>,
  pool: Pool,
  query: LiquidityQuery
) {
  try {
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
    } = pool.liquidityGivenDebt(
      state,
      new Uint256(query.poolInfo.totalLiquidity),
      new Uint112(query.debtIn!),
      new Uint256(currentTime),
      new Uint256(query.poolInfo.feeStored),
    );

    const collateralIn = dueOut.collateral.toString();
    const minLiquidity = calculateMinValue(liquidityOut, query.slippage);
    const maxAsset = calculateMaxValue(assetInReturn, query.slippage);
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

    app.ports.receiveAddLiqAnswer.send({
      ...query,
      result: {
        assetIn: assetInReturn.toString(),
        collateralIn,
        liquidityOut: liquidityOut.toString(),
        minLiquidity: minLiquidity.toString(),
        maxAsset: maxAsset.toString(),
        maxCollateral: maxCollateral.toString(),
        apr: futureApr,
        cdp: futureCdp,
      },
    });
  } catch(err) {
    console.log(err);

    app.ports.receiveLendAnswer.send({
      ...query,
      result: 0,
    });
  }
}

export async function debtTransaction(
  pool: SDKPool,
  gp: GlobalParams,
  liquidity: Liquidity
) {
  return await pool.upgrade(gp.walletSigner!).liquidityGivenDebt({
    liquidityTo: liquidity.liquidityTo,
    dueTo: liquidity.dueTo,
    debtIn: new Uint112(liquidity.debtIn),
    minLiquidity: new Uint256(liquidity.minLiquidity),
    maxAsset: new Uint112(liquidity.maxAsset),
    maxCollateral: new Uint112(liquidity.maxCollateral),
    deadline: new Uint256(liquidity.deadline),
  });
}

interface Liquidity {
  liquidityTo: string;
  dueTo: string;
  debtIn: string;
  minLiquidity: string;
  maxAsset: string;
  maxCollateral: string;
  deadline: number;
}
