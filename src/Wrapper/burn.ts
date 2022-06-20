import { GlobalParams } from './global';
import { Uint112, Uint128, Uint256 } from "@timeswap-labs/timeswap-v1-biconomy-sdk";
import { getPool, getPoolSDK, handleTxnErrors, updateCachedTxns } from "./helper";

export function burn(app: ElmApp<Ports>) {
  app.ports.queryLiq.subscribe(async (liqData) => {
    const now = Date.now();

    // Active pool
    if (now < Number(liqData.pool.maturity) * 1000) {
      let liqPercent = (BigInt(liqData.liquidityIn) * 10000n) / BigInt(liqData.poolInfo.totalLiquidity)

      app.ports.receiveLiqReturn.send({
        ...liqData,
        result: Number(liqPercent) / 100
      });
    } else {
      if (liqData.liquidityIn === "0") {
        app.ports.receiveLiqReturn.send({
          ...liqData,
          result: {
            asset: "0",
            collateral: "0"
          }
        });
      } else {
        const pool = getPool(liqData, liqData.poolInfo.fee, liqData.poolInfo.protocolFee);
        const reserves = {
          asset : new Uint128(liqData.poolInfo.assetReserve),
          collateral: new Uint128(liqData.poolInfo.collateralReserve)
        };
        const totalClaims = {
          bondPrincipal: new Uint112(liqData.poolInfo.totalBondPrincipal),
          bondInterest: new Uint112(liqData.poolInfo.totalBondInterest),
          insurancePrincipal: new Uint112(liqData.poolInfo.totalInsurancePrincipal),
          insuranceInterest: new Uint112(liqData.poolInfo.totalInsuranceInterest)
        }
        const { assetOut, collateralOut } = await pool.burn(
          reserves,
          totalClaims,
          new Uint256(liqData.poolInfo.totalLiquidity),
          new Uint256(liqData.liquidityIn),
          new Uint256(liqData.poolInfo.feeStored),
        );

        app.ports.receiveLiqReturn.send({
          ...liqData,
          result: {
            asset: assetOut.toString(),
            collateral: collateralOut.toString()
          }
        });
      }
    }
  });
}

export function burnSigner(
  app: ElmApp<Ports>,
  gp: GlobalParams
) {
  app.ports.burn.subscribe(async (params) => {
    const pool = getPoolSDK(gp, params.send.asset, params.send.collateral, params.send.maturity, params.chain);

    try {
      const txnConfirmation = await pool.upgrade(await gp.getSigner()).removeLiquidity({
        assetTo: params.send.assetTo,
        collateralTo: params.send.collateralTo,
        liquidityIn: new Uint256(params.send.liquidityIn),
      });

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
    } catch (error) {
      handleTxnErrors(error, app, gp, params);
    }
  });
}
