import { Uint256 } from "@timeswap-labs/timeswap-v1-sdk-core";
import { GlobalParams } from "../global";
import { getPool, getPoolSDK } from "../helper";
import { collateralCalculate, collateralTransaction } from "./collateral";
import { debtCalculate, debtTransaction } from "./debt";
import { percentCalculate, percentTransaction } from "./percent";

export function borrow(app: ElmApp<Ports>) {
  app.ports.queryBorrow.subscribe((query) =>
    borrowQueryCalculation(app, query)
  );

  app.ports.queryBorrowPerSecond.subscribe((query) =>
    borrowQueryCalculation(app, query)
  );

  app.ports.queryFull.subscribe((duesData) => {
    try {
      let totalDebt = new Uint256(0);
      let totalCollateral = new Uint256(0);

      duesData.dues.forEach(dueData => {
        totalDebt = totalDebt.add(new Uint256(dueData.due.debt));
        totalCollateral = totalCollateral.add(new Uint256(dueData.due.collateral));
      });

      app.ports.receiveFull.send({
        ...duesData,
        result: {
          assetIn: totalDebt.toString(),
          collateralOut: totalCollateral.toString()
        }
      });
    } catch {
      app.ports.receiveFull.send({
        ...duesData,
        result: "invalid"
      });
    }
  });

  app.ports.queryCustom.subscribe((query) => {
    let result: {
      collateralsOut: { tokenId: string, collateralOut: string }[];
      total?: { assetIn: string, collateralOut: string } | string
    } = { collateralsOut : []};

    let totalCollateralOut = new Uint256(0);
    let totalAssetIn = new Uint256(0);

    try {
      query.assetsIn.forEach((dueInput, index) => {
        const tokenDebt = new Uint256(query.dues[index].due.debt);
        const tokenCollateral = new Uint256(query.dues[index].due.collateral);
        const customDebt = new Uint256(dueInput.assetIn);

        if (customDebt > tokenDebt) {
          result.total = "invalid";
          result.collateralsOut[index] = {
            tokenId: dueInput.tokenId,
            collateralOut: "repay amount is greater than debt"
          }
        } else {
          const proportionalCollateral = customDebt.div(tokenDebt).mul(tokenCollateral);

          totalCollateralOut.add(proportionalCollateral);
          totalAssetIn.add(dueInput.assetIn);

          result.collateralsOut[index] = {
            tokenId: dueInput.tokenId,
            collateralOut: proportionalCollateral.toString()
          }
        }
      });

      if (result.total !== "invalid") {
        result.total = {
          assetIn: totalAssetIn.toString(),
          collateralOut: totalCollateralOut.toString()
        }
      }

      app.ports.receiveCustom.send({
        ...query,
        result
      });
    } catch (error) {
      app.ports.receiveCustom.send({
        ...query,
        result: {
          collateralsOut: [],
          total: "invalid"
        }
      });
    }
  });
}

export function borrowSigner(
  app: ElmApp<Ports>,
  gp: GlobalParams
) {
  app.ports.borrow.subscribe(async (params) => {
    const pool = getPoolSDK(gp, params.send.asset, params.send.collateral, params.send.maturity, params.chain);
    const { percent, debtIn, collateralIn, maxDebt, maxCollateral } = params.send;

    if (
      percent !== undefined &&
      maxDebt !== undefined &&
      maxCollateral !== undefined
    ) {
      await percentTransaction(pool, gp, {
        ...params.send,
        percent,
        maxDebt,
        maxCollateral,
      });
    } else if (debtIn !== undefined && maxCollateral !== undefined) {
      await debtTransaction(pool, gp, {
        ...params.send,
        debtIn,
        maxCollateral,
      });
    } else if (collateralIn !== undefined && maxDebt !== undefined) {
      await collateralTransaction(pool, gp, {
        ...params.send,
        collateralIn,
        maxDebt,
      });
    }
  });
}

async function borrowQueryCalculation(
  app: ElmApp<Ports>,
  query: BorrowQuery
) {
  const pool = getPool(query);
  const { percent, debtIn, collateralIn } = query;

  if (percent !== undefined) {
    await percentCalculate(app, pool, { ...query, percent });
  } else if (debtIn !== undefined) {
    await debtCalculate(app, pool, { ...query, debtIn });
  } else if (collateralIn !== undefined) {
    await collateralCalculate(app, pool, { ...query, collateralIn });
  }
}
