import { PairCore, Uint112, Uint128 } from "@timeswap-labs/timeswap-v1-biconomy-sdk";
import { GlobalParams } from "./global";
import { getPoolSDK, handleTxnErrors, updateCachedTxns } from "./helper";

export function withdraw(app: ElmApp<Ports>) {
  app.ports.querySum.subscribe((claimsData) => {
    const bond = new Uint128(claimsData.claims.bondInterest).add(new Uint128(claimsData.claims.bondPrincipal));
    const insurance = new Uint128(claimsData.claims.insuranceInterest).add(new Uint128(claimsData.claims.insurancePrincipal));

    app.ports.receiveSum.send({
      ...claimsData,
      result: {
        asset: bond.toString(),
        collateral: insurance.toString()
      }
    });
  });

  app.ports.queryClaim.subscribe((claimsData) => {
    if (claimsData.claimsIn.bondPrincipal === "0" && claimsData.claimsIn.insurancePrincipal === "0") {
      app.ports.receiveReturn.send({
        ...claimsData,
        result: {
          asset: "0",
          collateral: "0"
        }
      });
    } else {
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

      const { asset, collateral } = PairCore.calculateWithdraw(reserves, totalClaims, claimsIn)

      app.ports.receiveReturn.send({
        ...claimsData,
        result: {
          asset: asset.toString(),
          collateral: collateral.toString()
        }
      });
    }
  });
}

export function withdrawSigner(
  app: ElmApp<Ports>,
  gp: GlobalParams
) {
  app.ports.withdraw.subscribe(async (params) => {
    const pool = getPoolSDK(
      gp, params.send.asset,
      params.send.collateral,
      params.send.maturity,
      params.chain,
      params.send.convAddress
    );

    try {
      const txnConfirmation = await pool.upgrade(await gp.getSigner()).collect({
        assetTo: params.send.assetTo,
        collateralTo: params.send.collateralTo,
        claimsIn: {
          bondInterest: new Uint112(params.send.claimsIn.bondInterest),
          bondPrincipal: new Uint112(params.send.claimsIn.bondPrincipal),
          insuranceInterest: new Uint112(params.send.claimsIn.insuranceInterest),
          insurancePrincipal: new Uint112(params.send.claimsIn.insurancePrincipal)
        },
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
  });
}
