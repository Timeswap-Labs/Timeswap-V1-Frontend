import { Pool } from "@timeswap-labs/timeswap-v1-sdk";
import { Contract, Provider } from "ethcall";
import { WhiteList } from "./whitelist";
import { abi as TimeswapPairAbi } from "./abi/IPair.json";
import type { IPair } from "./typechain";
import {
  Uint112,
  Uint128,
  Uint16,
  Uint256,
} from "@timeswap-labs/timeswap-v1-sdk-core";

export async function pool(app: ElmApp<Ports>, whitelist: WhiteList) {
  const params: {
    asset: string;
    collateral: string;
    maturity: number;
    pool: Pool;
  }[] = [];
  const calls: any[] = [];

  const now = Math.floor(Date.now() / 1000);

  const multiCallProvider = new Provider();
  await multiCallProvider.init(whitelist.provider);

  for (const { asset, collateral, maturity, pool } of whitelist.poolEntries()) {
    if (now < maturity) {
      params.push({ asset, collateral, maturity, pool });
      const pairContract = new Contract(pool.pairAddress()!, TimeswapPairAbi);

      calls.push(pairContract.constantProduct(maturity));
      calls.push(pairContract.totalReserves(maturity));
      calls.push(pairContract.totalClaims(maturity));
      calls.push(pairContract.totalLiquidity(maturity));
      calls.push(pairContract.fee());
      calls.push(pairContract.protocolFee());

      const pair = pool.pairContract() as IPair;
      const filter = pair.filters.Sync(maturity);
      pair.removeAllListeners(filter);
      pair.on(filter, async (_maturity, state) => {
        const cache = getModifiedCache([
          state,
          state.reserves,
          state.totalClaims,
          state.totalLiquidity,
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

  const results = await multiCallProvider.tryAll(calls);

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
  const x = new Uint112(results[base ?? 0].x.toString());
  const y = new Uint112(results[base ?? 0].y.toString());
  const z = new Uint112(results[base ?? 0].z.toString());

  const asset = new Uint128(results[(base ?? 0) + 1].asset.toString());
  const collateral = new Uint128(
    results[(base ?? 0) + 1].collateral.toString()
  );

  const bond = new Uint128(results[(base ?? 0) + 2].bond.toString());
  const insurance = new Uint128(results[(base ?? 0) + 2].insurance.toString());

  const totalLiquidity = new Uint256(results[(base ?? 0) + 3].toString());

  return {
    state: { x, y, z },
    reserves: { asset, collateral },
    totalClaims: { bond, insurance },
    totalLiquidity,
  };
}
