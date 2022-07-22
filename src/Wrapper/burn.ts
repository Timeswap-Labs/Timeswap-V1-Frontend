import { Contract } from "@ethersproject/contracts";
import axios, { AxiosResponse } from "axios";

import { CONVENIENCE, WNATIVE_ADDRESS, Uint112, Uint128, Uint256 } from "@timeswap-labs/timeswap-v1-biconomy-sdk";
import { FlashTSRepay } from "@timeswap-labs/flash-repay-sdk";
import cdTokenAbi from "./abi/cdToken";
import { GlobalParams } from './global';
import { getPool, getPoolSDK, handleTxnErrors, updateCachedTxns } from "./helper";
import { API_ENDPOINT_PROD, API_ENDPOINT_TEST, FLASH_REPAY_CONTRACT } from "./constants";

const flashTSRepayAddress = FLASH_REPAY_CONTRACT;

export function burn(app: ElmApp<Ports>, gp: GlobalParams) {
  app.ports.queryLiq.subscribe(async (liqData) => {
    const now = Date.now();

    // Active pool
    if (now < Number(liqData.pool.maturity) * 1000) {
      let liqPercent = (BigInt(liqData.liquidityIn) * 10000n) / BigInt(liqData.poolInfo.totalLiquidity)

      app.ports.receiveLiqReturn.send({
        ...liqData,
        result: {
          liqPercent: Number(liqPercent) / 100,
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

  app.ports.queryFlashRepay.subscribe(async (flashRepayData) => {
    const now = Date.now();
    let apiEndpoint;

    if (process.env.PARCEL_PUBLIC_ENVIRONMENT === "production")
      apiEndpoint = API_ENDPOINT_PROD;
    else
      apiEndpoint = API_ENDPOINT_TEST;

    // Active pool
    if (now < Number(flashRepayData.pool.maturity) * 1000) {
      const assetAddress = flashRepayData.pool.asset.address || WNATIVE_ADDRESS[flashRepayData.chain.chainId];
      const collateralAddress = flashRepayData.pool.collateral.address || WNATIVE_ADDRESS[flashRepayData.chain.chainId];
      let isCDTApproved = false;
      let liqDueTokenIds: string[] = [];

      const cdtokenids: Promise<AxiosResponse<any, any>> = axios.get(
        `${apiEndpoint}/cdtokenids?chainId=${flashRepayData.chain.chainId}&asset=${assetAddress}&collateral=${collateralAddress}&maturity=${flashRepayData.pool.maturity}`,
      );

      const cdtContract = new Contract(flashRepayData.cdtAddress, cdTokenAbi, gp.walletProviderMulti)
      const cdtApprovalCheck = cdtContract.isApprovedForAll(await gp.walletSigner.getAddress(), flashTSRepayAddress);

      const [cdTokenIdsResp, cdtApproval] = await Promise.all([cdtokenids, cdtApprovalCheck]);
      isCDTApproved = cdtApproval;

      if (cdTokenIdsResp.data && cdTokenIdsResp.data.length) {
        flashRepayData.tokenIds.forEach(tokenId => {
          try {
            if (cdTokenIdsResp.data.includes(parseInt(tokenId))) {
              liqDueTokenIds.push(tokenId);
            }
          } catch (error) {
            // if parseInt fails, do nothing
          }
        })
      }

      app.ports.receiveFlashRepay.send({
        ...flashRepayData,
        result: {
          isCDTApproved,
          liqDueTokenIds
        }
      });
    }
  });

  app.ports.flashRepayTry.subscribe(async (flashRepayData) => {
    let isFlashRepayAllowed = false;
    const assetAddress = flashRepayData.pool.asset.address || WNATIVE_ADDRESS[flashRepayData.chain.chainId];
    const collateralAddress = flashRepayData.pool.collateral.address || WNATIVE_ADDRESS[flashRepayData.chain.chainId];

    const flashTSRepay = new FlashTSRepay(flashTSRepayAddress, gp.walletSigner);

    try {
      await flashTSRepay.try(
        CONVENIENCE[flashRepayData.chain.chainId],
        assetAddress,
        collateralAddress,
        flashRepayData.pool.maturity,
        flashRepayData.tokenIds
      );

      isFlashRepayAllowed = true;
    } catch (error) {
      isFlashRepayAllowed = false;
    }

    app.ports.receiveFlashRepayTry.send({
      ...flashRepayData,
      result: isFlashRepayAllowed
    });
  });

  app.ports.flashRepay.subscribe(async (params) => {
    flashRepayHandler(app, gp, params)
  });
}

export function burnSigner(
  app: ElmApp<Ports>,
  gp: GlobalParams
) {
  app.ports.burn.subscribe(async (params) => {
    const pool = getPoolSDK(
      gp, params.send.asset,
      params.send.collateral,
      params.send.maturity,
      params.chain,
      params.send.convAddress
    );

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

export async function flashRepayHandler(app: ElmApp<Ports>, gp: GlobalParams, params: FlashRepay) {
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
}
