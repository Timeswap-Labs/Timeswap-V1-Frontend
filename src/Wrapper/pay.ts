import { Uint112, Uint256 } from "@timeswap-labs/timeswap-v1-sdk-core";
import { GlobalParams } from "./global";
import { getPoolSDK, updateCachedTxns } from "./helper";

export function pay(app: ElmApp<Ports>) {
  app.ports.queryFull.subscribe((duesData) => {
    try {
      let totalDebt = new Uint256(0);
      let totalCollateral = new Uint256(0);

      duesData.dues.forEach((dueData) => {
        totalDebt = totalDebt.add(new Uint256(dueData.due.debt));
        totalCollateral = totalCollateral.add(
          new Uint256(dueData.due.collateral)
        );
      });

      app.ports.receiveFull.send({
        ...duesData,
        result: {
          assetIn: totalDebt.toString(),
          collateralOut: totalCollateral.toString(),
        },
      });
    } catch {
      app.ports.receiveFull.send({
        ...duesData,
        result: "invalid",
      });
    }
  });

  app.ports.queryCustom.subscribe((query) => {
    let result: {
      collateralsOut: { tokenId: string; collateralOut: string }[];
      total?: { assetIn: string; collateralOut: string } | string;
    } = { collateralsOut: [] };

    let totalCollateralOut = new Uint256(0);
    let totalAssetIn = new Uint256(0);

    try {
      query.assetsIn.forEach((dueInput, index) => {
        const tokenDebt = new Uint256(query.dues[index].due.debt);
        const tokenCollateral = new Uint256(query.dues[index].due.collateral);
        const customDebt = new Uint256(dueInput.assetIn);

        if (customDebt.gt(tokenDebt)) {
          result.total = "invalid";
          result.collateralsOut[index] = {
            tokenId: dueInput.tokenId,
            collateralOut: "invalid",
          };
        } else {
          const proportionalCollateral = customDebt
            .mul(tokenCollateral)
            .div(tokenDebt);

          totalCollateralOut.add(proportionalCollateral);
          totalAssetIn.add(customDebt);

          result.collateralsOut[index] = {
            tokenId: dueInput.tokenId,
            collateralOut: proportionalCollateral.toString(),
          };
        }
      });

      if (result.total !== "invalid") {
        result.total = {
          assetIn: totalAssetIn.toString(),
          collateralOut: totalCollateralOut.toString(),
        };
      }

      app.ports.receiveCustom.send({
        ...query,
        result,
      });
    } catch (error) {
      app.ports.receiveCustom.send({
        ...query,
        result: {
          collateralsOut: [],
          total: "invalid",
        },
      });
    }
  });
}

export function paySigner(app: ElmApp<Ports>, gp: GlobalParams) {
  app.ports.pay.subscribe(async (params) => {
    const pool = getPoolSDK(
      gp,
      params.send.asset,
      params.send.collateral,
      params.send.maturity,
      params.chain
    );

    try {
      const txnConfirmation = await pool.upgrade(gp.metamaskSigner!).repay({
        collateralTo: params.send.collateralTo,
        ids: params.send.ids.map((id) => new Uint256(id)),
        maxAssetsIn: params.send.maxAssetsIn.map(
          (maxAssetIn) => new Uint112(maxAssetIn)
        ),
        deadline: new Uint256(params.send.deadline),
      });

      if (txnConfirmation) {
        app.ports.receiveConfirm.send({
          id: params.id,
          chain: params.chain,
          address: params.address,
          hash: txnConfirmation.hash,
        });

        const txnReceipt = await txnConfirmation.wait();
        const receiveReceipt = {
          chain: params.chain,
          address: params.address,
          hash: txnConfirmation.hash,
          state: txnReceipt.status ? "success" : "failed",
        };
        app.ports.receiveReceipt.send(receiveReceipt);
        updateCachedTxns(receiveReceipt);
      }
    } catch (error) {
      app.ports.receiveConfirm.send({
        id: params.id,
        chain: params.chain,
        address: params.address,
        hash: null,
      });
    }
  });
}
