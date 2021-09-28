import { Pool } from "@timeswap-labs/timeswap-v1-sdk";
import { WhiteList } from "./whitelist";
import timeswapPair from "./abi/pair";
import {
  Uint112,
  Uint128,
  Uint16,
  Uint256,
} from "@timeswap-labs/timeswap-v1-sdk-core";
import { Contract } from "@ethersproject/contracts";
import { GlobalParams } from "./global";

export async function pool(
  app: ElmApp<Ports>,
  whitelist: WhiteList,
  gp: GlobalParams
) {
  const params: {
    asset: string;
    collateral: string;
    maturity: number;
    pool: Pool;
  }[] = [];
  const calls: any[] = [];

  const now = Math.floor(Date.now() / 1000);

  for (const { asset, collateral, maturity, pool } of whitelist.poolEntries()) {
    if (now < maturity) {
      params.push({ asset, collateral, maturity, pool });
      const pairContract = pool.pairContract()!.connect(gp.provider);

      calls.push(pairContract.constantProduct(maturity));
      calls.push(pairContract.totalReserves(maturity));
      calls.push(pairContract.totalClaims(maturity));
      calls.push(pairContract.totalLiquidity(maturity));
      calls.push(pairContract.fee());
      calls.push(pairContract.protocolFee());

      const pair = new Contract(
        pairContract.address,
        timeswapPair,
        gp.provider
      );
      const filter = pair.filters.Sync(maturity);
      pair.removeAllListeners(filter);
      pair.on(filter, async (_maturity, state) => {
        const cache = getModifiedCache([
          state.slice(4),
          state[0],
          state[2],
          state[1],
        ]);
        pool.updateCache(cache);

        const apr = await pool.calculateApr();
        const cf = (await pool.calculateCf()).value.toString();

        app.ports.sdkPoolsMsg.send([
          {
            asset,
            collateral,
            maturity,
            assetLiquidity: cache.reserves.asset.value.toString(),
            collateralLiquidity: cache.reserves.collateral.value.toString(),
            apr,
            cf,
          },
        ]);
      });
    }
  }

  const results = await Promise.all(calls);

  for (const [
    index,
    { asset, collateral, maturity, pool },
  ] of params.entries()) {
    const base = index * 6;
    const cache = getCache(results, base);
    pool.setCache(cache);

    const apr = await pool.calculateApr();
    const cf = (await pool.calculateCf()).value.toString();

    app.ports.sdkPoolsMsg.send([
      {
        asset,
        collateral,
        maturity,
        assetLiquidity: cache.reserves.asset.value.toString(),
        collateralLiquidity: cache.reserves.collateral.value.toString(),
        apr,
        cf,
      },
    ]);
  }
}

function getCache(results: any[], base?: number) {
  const fee = new Uint16(results[(base ?? 0) + 4].toString());
  const protocolFee = new Uint16(results[(base ?? 0) + 5].toString());

  return {
    ...getModifiedCache(results, base),
    fee,
    protocolFee,
  };
}

function getModifiedCache(results: any[], base?: number) {
  const x = new Uint112(results[base ?? 0][0].toString());
  const y = new Uint112(results[base ?? 0][1].toString());
  const z = new Uint112(results[base ?? 0][2].toString());

  const asset = new Uint128(results[(base ?? 0) + 1][0].toString());
  const collateral = new Uint128(results[(base ?? 0) + 1][1].toString());

  const bond = new Uint128(results[(base ?? 0) + 2][0].toString());
  const insurance = new Uint128(results[(base ?? 0) + 2][1].toString());

  const totalLiquidity = new Uint256(results[(base ?? 0) + 3].toString());

  return {
    state: { x, y, z },
    reserves: { asset, collateral },
    totalClaims: { bond, insurance },
    totalLiquidity,
  };
}
