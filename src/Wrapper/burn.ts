import { Contract } from "@ethersproject/contracts";
import cdTokenAbi from "./abi/cdToken";

import { GlobalParams } from './global';
import { Uint112, Uint128, Uint256 } from "@timeswap-labs/timeswap-v1-sdk-core";
import { getPool, getPoolSDK, handleTxnErrors, updateCachedTxns } from "./helper";
import { FlashTSRepay } from "@timeswap-labs/flash-repay-sdk";
import { CONVENIENCE, WNATIVE_ADDRESS } from '@timeswap-labs/timeswap-v1-sdk';

const flashTSRepayAddress = "0xd88afb2186d1b6974de255fd18a04552d4f87b50";

export function burn(app: ElmApp<Ports>, gp: GlobalParams) {
  app.ports.queryLiq.subscribe(async (liqData) => {
    const now = Date.now();

    // Active pool
    if (now < Number(liqData.pool.maturity) * 1000) {
      let liqPercent = (BigInt(liqData.liquidityIn) * 10000n) / BigInt(liqData.poolInfo.totalLiquidity)
      const assetAddress = liqData.pool.asset.address || WNATIVE_ADDRESS[liqData.chain.chainId];
      const collateralAddress = liqData.pool.collateral.address || WNATIVE_ADDRESS[liqData.chain.chainId];
      let isFlashRepayAllowed = false;
      let isCDTApproved = false;

      if (liqData.cdtAddress) {
        const cdtContract = new Contract(liqData.cdtAddress, cdTokenAbi, gp.walletProviderMulti)
        isCDTApproved = await cdtContract.isApprovedForAll(await gp.walletSigner.getAddress(), flashTSRepayAddress);
      }

      if (isCDTApproved) {
        const flashTSRepay = new FlashTSRepay(flashTSRepayAddress, gp.walletSigner);

        try {
          await flashTSRepay.try(
            CONVENIENCE[liqData.chain.chainId],
            assetAddress,
            collateralAddress,
            liqData.pool.maturity,
            liqData.tokenIds
          );

          isFlashRepayAllowed = true;
        } catch (error) {
          isFlashRepayAllowed = false;
        }
      }

      app.ports.receiveLiqReturn.send({
        ...liqData,
        result: {
          liqPercent: Number(liqPercent) / 100,
          isFlashRepayAllowed,
          isCDTApproved
        }
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

  app.ports.flashRepay.subscribe(async (params) => {
    const assetAddress = params.send.asset.address || WNATIVE_ADDRESS[params.chain.chainId];
    const collateralAddress = params.send.collateral.address || WNATIVE_ADDRESS[params.chain.chainId];
    const flashTSRepay = new FlashTSRepay(flashTSRepayAddress, gp.walletSigner);

    try {
      const txnConfirmation = await flashTSRepay.execute(
        CONVENIENCE[params.chain.chainId],
        assetAddress,
        collateralAddress,
        params.send.maturity,
        params.send.ids
      );

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

export function burnSigner(
  app: ElmApp<Ports>,
  gp: GlobalParams
) {
  app.ports.burn.subscribe(async (params) => {
    const pool = getPoolSDK(gp, params.send.asset, params.send.collateral, params.send.maturity, params.chain);

    try {
      const txnConfirmation = await pool.upgrade(gp.walletSigner!).removeLiquidity({
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
