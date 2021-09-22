import { ERC20Token } from "@timeswap-labs/timeswap-v1-sdk";
import { Uint112, Uint256 } from "@timeswap-labs/timeswap-v1-sdk-core";
import { GlobalParams } from "./global";
import { WhiteList } from "./whitelist";

export function pay(app: ElmApp<Ports>) {
  app.ports.queryPay.subscribe((query) => {
    payQueryCalculate(app, query);
  });
}

export function paySigner(
  app: ElmApp<Ports>,
  whitelist: WhiteList,
  gp: GlobalParams
) {
  app.ports.approvePay.subscribe(async ({ erc20 }) => {
    (whitelist.getToken(erc20) as ERC20Token)
      .upgrade(gp.metamaskSigner!)
      .approve(whitelist.convenience, new Uint256((1n << 256n) - 1n));
  });

  app.ports.pay.subscribe(async (params) => {
    const pool = whitelist.getPool(
      params.asset,
      params.collateral,
      params.maturity
    );

    const txn = await pool.upgrade(gp.metamaskSigner!).repay({
      collateralTo: params.collateralTo,
      ids: params.ids.map((id) => new Uint256(id)),
      maxAssetsIn: params.maxAssetsIn.map(
        (maxAssetIn) => new Uint112(maxAssetIn)
      ),
      deadline: new Uint256(params.deadline),
    });

    await txn.wait();
  });
}

export function payQueryCalculate(app: ElmApp<Ports>, query: PayQuery) {
  const assetIn = query.dues
    .reduce((sum, due) => sum + BigInt(due.debt), 0n)
    .toString();
  const collateralOut = query.dues
    .reduce((sum, due) => sum + BigInt(due.collateral), 0n)
    .toString();

  app.ports.sdkPayMsg.send({
    ...query,
    assetIn,
    collateralOut,
  });
}
