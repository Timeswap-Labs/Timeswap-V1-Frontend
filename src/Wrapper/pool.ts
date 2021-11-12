import { Pool } from "@timeswap-labs/timeswap-v1-sdk";
import { WhiteList } from "./whitelist";
import timeswapPair from "./abi/pair";
import {
  Claims,
  CP,
  Tokens,
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
      const pairContract = pool.pairContract()!.connect(gp.providerMulti);

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

      filters(app, pair, pairContract, asset, collateral, maturity, pool);
    }
  }

  const results = await Promise.all(calls);

  for (const [
    index,
    { asset, collateral, maturity, pool },
  ] of params.entries()) {
    const base = index * 6;
    const cache = getCache(results, base);
    updateCache(app, asset, collateral, maturity, cache, pool);
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

function filters(
  app: ElmApp<Ports>,
  pair: Contract,
  pairMulti: Contract,
  asset: string,
  collateral: string,
  maturity: number,
  pool: Pool
) {
  const mintFilter = pair.filters.Mint();
  const burnFilter = pair.filters.Burn();
  const lendFilter = pair.filters.Lend();
  const withdrawFilter = pair.filters.Withdraw();
  const borrowFilter = pair.filters.Borrow();
  const payFilter = pair.filters.Pay();

  pair.on(mintFilter, async (eventMaturity) => {
    if (eventMaturity.toString() == maturity) {
      const calls = [];

      calls.push(pairMulti.constantProduct(maturity));
      calls.push(pairMulti.totalReserves(maturity));
      calls.push(pairMulti.totalLiquidity(maturity));

      const result = await Promise.all(calls);

      const cache = {
        state: {
          x: new Uint112(result[0][0].toString()),
          y: new Uint112(result[0][1].toString()),
          z: new Uint112(result[0][2].toString()),
        },
        reserves: {
          asset: new Uint128(result[1][0].toString()),
          collateral: new Uint128(result[1][1].toString()),
        },
        totalLiquidity: new Uint256(result[2].toString()),
      };

      updateCache(app, asset, collateral, maturity, cache, pool);
    }
  });

  pair.on(burnFilter, async (eventMaturity) => {
    if (eventMaturity.toString() == maturity) {
      const calls = [];

      calls.push(pairMulti.totalReserves(maturity));
      calls.push(pairMulti.totalLiquidity(maturity));

      const result = await Promise.all(calls);

      const cache = {
        reserves: {
          asset: new Uint128(result[0][0].toString()),
          collateral: new Uint128(result[0][1].toString()),
        },
        totalLiquidity: new Uint256(result[1].toString()),
      };

      updateCache(app, asset, collateral, maturity, cache, pool);
    }
  });

  pair.on(lendFilter, async (eventMaturity) => {
    if (eventMaturity.toString() == maturity) {
      const calls = [];

      calls.push(pairMulti.constantProduct(maturity));
      calls.push(pairMulti.totalReserves(maturity));
      calls.push(pairMulti.totalClaims(maturity));

      const result = await Promise.all(calls);

      const cache = {
        state: {
          x: new Uint112(result[0][0].toString()),
          y: new Uint112(result[0][1].toString()),
          z: new Uint112(result[0][2].toString()),
        },
        reserves: {
          asset: new Uint128(result[1][0].toString()),
          collateral: new Uint128(result[1][1].toString()),
        },
        totalClaims: {
          bond: new Uint128(result[2][0].toString()),
          insurance: new Uint128(result[2][1].toString()),
        },
      };

      updateCache(app, asset, collateral, maturity, cache, pool);
    }
  });

  pair.on(withdrawFilter, async (eventMaturity) => {
    if (eventMaturity.toString() == maturity) {
      const calls = [];

      calls.push(pairMulti.totalReserves(maturity));
      calls.push(pairMulti.totalClaims(maturity));

      const result = await Promise.all(calls);

      const cache = {
        reserves: {
          asset: new Uint128(result[0][0].toString()),
          collateral: new Uint128(result[0][1].toString()),
        },
        totalClaims: {
          bond: new Uint128(result[1][0].toString()),
          insurance: new Uint128(result[1][1].toString()),
        },
      };

      updateCache(app, asset, collateral, maturity, cache, pool);
    }
  });

  pair.on(borrowFilter, async (eventMaturity) => {
    if (eventMaturity.toString() == maturity) {
      const calls = [];

      calls.push(pairMulti.constantProduct(maturity));
      calls.push(pairMulti.totalReserves(maturity));

      const result = await Promise.all(calls);

      const cache = {
        state: {
          x: new Uint112(result[0][0].toString()),
          y: new Uint112(result[0][1].toString()),
          z: new Uint112(result[0][2].toString()),
        },
        reserves: {
          asset: new Uint128(result[1][0].toString()),
          collateral: new Uint128(result[1][1].toString()),
        },
      };

      updateCache(app, asset, collateral, maturity, cache, pool);
    }
  });

  pair.on(payFilter, async (eventMaturity) => {
    if (eventMaturity.toString() == maturity) {
      const calls = [];

      calls.push(pairMulti.totalReserves(maturity));

      const result = await Promise.all(calls);

      const cache = {
        reserves: {
          asset: new Uint128(result[0][0].toString()),
          collateral: new Uint128(result[0][1].toString()),
        },
      };

      updateCache(app, asset, collateral, maturity, cache, pool);
    }
  });
}

async function updateCache(
  app: ElmApp<Ports>,
  asset: string,
  collateral: string,
  maturity: number,
  cache: {
    state?: CP;
    reserves: Tokens;
    totalClaims?: Claims;
    totalLiquidity?: Uint256;
    fee?: Uint16;
    protocolFee?: Uint16;
  },
  pool: Pool
) {
  await pool.updateCache(cache);

  const apr = await pool.calculateApr();
  const cf = (await pool.calculateCdp()).toString();

  app.ports.sdkPoolsMsg.send([
    {
      asset,
      collateral,
      maturity,
      assetLiquidity: cache.reserves.asset.toString(),
      collateralLiquidity: cache.reserves.collateral.toString(),
      apr,
      cf,
    },
  ]);
}
