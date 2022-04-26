import { Pair, Uint112, Uint128, Uint256 } from "@timeswap-labs/timeswap-v1-sdk-core";
import { getPool, getPoolSDK, updateCachedTxns } from "./helper";

export function burn(app: ElmApp<Ports>) {
  app.ports.queryLiq.subscribe(async (liqData) => {
    const now = Date.now();

    if (now > Number(liqData.pool.maturity) * 1000) {
      let liqPercent = (BigInt(liqData.liquidityIn) * 10000n) / BigInt(liqData.poolInfo.totalLiquidity)

      app.ports.receiveLiqReturn.send({
        ...liqData,
        result: Number(liqPercent) / 100
      });
    } else {
      const pool = getPool(liqData);
      const reserves = { asset : new Uint128(liqData.poolInfo.assetReserve), collateral: new Uint128(liqData.poolInfo.assetReserve) };
      const totalClaims = {
        bondPrincipal: new Uint112(liqData.poolInfo.totalBondPrincipal),
        bondInterest: new Uint112(liqData.poolInfo.totalBondInterest),
        insurancePrincipal: new Uint112(liqData.poolInfo.totalInsurancePrincipal),
        insuranceInterest: new Uint112(liqData.poolInfo.totalBondInterest)
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
  });
}

// export function burnSigner(
//   app: ElmApp<Ports>,
//   gp: GlobalParams
// ) {
//   app.ports.withdraw.subscribe(async (params) => {
//     const pool = getPoolSDK(gp, params.send.asset, params.send.collateral, params.send.maturity, params.chain);

//     try {
//       const txnConfirmation = await pool.upgrade(gp.walletSigner!).collect({
//         assetTo: params.send.assetTo,
//         collateralTo: params.send.collateralTo,
//         claimsIn: {
//           bondInterest: new Uint112(params.send.claimsIn.bondInterest),
//           bondPrincipal: new Uint112(params.send.claimsIn.bondPrincipal),
//           insuranceInterest: new Uint112(params.send.claimsIn.insuranceInterest),
//           insurancePrincipal: new Uint112(params.send.claimsIn.insurancePrincipal)
//         },
//       });

//       if (txnConfirmation) {
//         app.ports.receiveConfirm.send({
//           id: params.id,
//           chain: params.chain,
//           address: params.address,
//           hash: txnConfirmation.hash
//         });

//         const txnReceipt = await txnConfirmation.wait();
//         const receiveReceipt = {
//           chain: params.chain,
//           address: params.address,
//           hash: txnConfirmation.hash,
//           state: txnReceipt.status ? "success" : "failed"
//         }
//         app.ports.receiveReceipt.send(receiveReceipt);
//         updateCachedTxns(receiveReceipt);
//       }
//     } catch (error) {
//       app.ports.receiveConfirm.send({
//         id: params.id,
//         chain: params.chain,
//         address: params.address,
//         hash: null
//       });
//     }
//   });
// }
