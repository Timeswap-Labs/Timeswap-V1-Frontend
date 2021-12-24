import { BaseProvider } from "@ethersproject/providers";
import { WhiteList } from "./whitelist";
import { updateErc20Balance } from "./helper";
import { Uint128, Uint256 } from "@timeswap-labs/timeswap-v1-sdk-core";
import { ERC20Token, Pool } from "@timeswap-labs/timeswap-v1-sdk";
import erc20Abi from "./abi/erc20";
import pairAbi from "./abi/pair";
import { Contract, Event } from "@ethersproject/contracts";
import { GlobalParams } from "./global";

export async function lendPositionsInit(
  app: ElmApp<Ports>,
  whitelist: WhiteList,
  gp: GlobalParams,
  address: string
) {
  const lendPositionsToken: {
    asset: string;
    collateral: string;
    maturity: number;
    pool: Pool;
  }[] = [];
  const lendPositions: Promise<Uint256>[] = [];

  for (const {
    asset,
    collateral,
    maturity,
    pool,
    bond,
    insurance,
  } of whitelist.poolEntries()) {
    const bondToken = bond.connect(gp.metamaskProviderMulti);
    const insuranceToken = insurance.connect(gp.metamaskProviderMulti);

    lendPositionsToken.push({
      asset,
      collateral,
      maturity,
      pool,
    });
    lendPositions.push(bondToken.balanceOf(address));
    lendPositions.push(insuranceToken.balanceOf(address));

    updateBalances(
      app,
      asset,
      collateral,
      maturity,
      pool,
      bondToken,
      insuranceToken,
      gp.metamaskProvider,
      address
    );
  }

  const result = await Promise.all(lendPositions);

  for (const [
    index,
    { asset, collateral, maturity, pool },
  ] of lendPositionsToken.entries()) {
    const base = index * 2;
    const now = Math.floor(Date.now() / 1000);

    const bond = result[base];
    const insurance = result[base + 1];

    if (!(bond.toBigInt() === 0n && insurance.toBigInt() === 0n)) {
      if (maturity <= now) {
        try {
          const { asset: assetOut, collateral: collateralOut } =
          await pool.calculateWithdraw({
            bond: new Uint128(bond),
            insurance: new Uint128(insurance),
          });

          app.ports.sdkPositionsMsg.send([
            {
              asset,
              collateral,
              maturity,
              bond: bond.toString(),
              insurance: insurance.toString(),
              assetOut: assetOut.toString(),
              collateralOut: collateralOut.toString(),
            },
          ]);
        }
        catch {
          app.ports.sdkPositionsMsg.send([
            {
              asset,
              collateral,
              maturity,
              bond: bond.toString(),
              insurance: insurance.toString(),
              assetOut: "0",
              collateralOut: "0",
            },
          ]);
        }
      } else {
        app.ports.sdkPositionsMsg.send([
          {
            asset,
            collateral,
            maturity,
            bond: bond.toString(),
            insurance: insurance.toString(),
          },
        ]);
      }
    }
  }
}

export async function borrowPositionsInit(
  app: ElmApp<Ports>,
  whitelist: WhiteList,
  gp: GlobalParams,
  address: string
) {
  app.ports.sdkPositionsMsg.send(
    await Promise.all(
      whitelist
        .poolEntries()
        .map(async ({ asset, collateral, maturity, collateralizedDebt }) => {
          const cdToken = collateralizedDebt.connect(gp.metamaskProviderMulti);
          const startBlock = (await cdToken.dueOf(0))[2] - 1;

          const tokenBalanceIn = cdToken.filters.Transfer(null, address);
          const tokenBalanceOut = cdToken.filters.Transfer(address);

          collateralizedDebt.on(tokenBalanceIn, async (_from, _to, id) => {
            const [debtValue, collateralValue] = await cdToken.dueOf(id);
            whitelist.pushTokenId(
              cdToken.address,
              id.toString(),
              debtValue.toString(),
              collateralValue.toString()
            );

            const dues: {
              id: string;
              debt: string;
              collateral: string;
            }[] = [];
            for (const [id, { debt, collateral }] of whitelist.getTokenIds(
              cdToken.address
            )) {
              dues.push({ id, debt, collateral });
            }

            app.ports.sdkPositionsMsg.send([
              {
                asset,
                collateral,
                maturity,
                dues,
              },
            ]);
          });

          collateralizedDebt.on(tokenBalanceOut, async (_from, _to, id) => {
            whitelist.popTokenId(cdToken.address, id.toString());

            const dues: {
              id: string;
              debt: string;
              collateral: string;
            }[] = [];
            for (const [id, { debt, collateral }] of whitelist.getTokenIds(
              cdToken.address
            )) {
              dues.push({ id, debt, collateral });
            }

            app.ports.sdkPositionsMsg.send([
              {
                asset,
                collateral,
                maturity,
                dues,
              },
            ]);
          });

          const pair = whitelist.getPairAddress(asset, collateral);
          const pairContract = new Contract(pair, pairAbi, gp.metamaskProvider);
          const payFilter = pairContract.filters.Pay(); // TODO: Fix this

          pairContract.on(payFilter, async (pairMaturity) => {
            if (pairMaturity == maturity) {
              const ids = Array.from(
                whitelist.getTokenIds(cdToken.address)
              ).map(([id]) => id);
              const dues = await getDues(cdToken, ids, whitelist);

              app.ports.sdkPositionsMsg.send([
                { asset, collateral, maturity, dues },
              ]);
            }
          });

          const tokenIdsIn = await cdToken.queryFilter(
            tokenBalanceIn,
            startBlock
          );
          const tokenIdsOut = await cdToken.queryFilter(
            tokenBalanceOut,
            startBlock
          );

          const tokenIds = tokenFilter(tokenIdsIn, tokenIdsOut);
          const dues = await getDues(cdToken, tokenIds, whitelist);

          return {
            asset,
            collateral,
            maturity,
            dues,
          };
        })
    )
  );
}

function tokenFilter(tokenIdsIn: Event[], tokenIdsOut: Event[]) {
  const tokenIdsMap = new Map<string, number>();

  tokenIdsIn
    .map((event) => {
      return {
        blockNumber: event.blockNumber,
        id: event.args![2].toString(),
      };
    })
    .forEach(({ blockNumber, id }) => {
      tokenIdsMap.set(id, blockNumber);
    });
  tokenIdsOut
    .map((event) => {
      return {
        blockNumber: event.blockNumber,
        id: event.args![2].toString(),
      };
    })
    .forEach(({ blockNumber, id }) => {
      if (tokenIdsMap.get(id)! < blockNumber) {
        tokenIdsMap.delete(id);
      }
    });

  return Array.from(tokenIdsMap.keys());
}

async function getDues(
  cdToken: Contract,
  tokenIds: string[],
  whitelist: WhiteList
) {
  return await Promise.all(
    tokenIds.map(async (id) => {
      const [debt, collateral] = await cdToken.dueOf(id);

      whitelist.pushTokenId(
        cdToken.address,
        id,
        debt.toString(),
        collateral.toString()
      );

      return {
        id,
        debt: debt.toString(),
        collateral: collateral.toString(),
      };
    })
  );
}

function updateBalances(
  app: ElmApp<Ports>,
  asset: string,
  collateral: string,
  maturity: number,
  pool: Pool,
  bondToken: ERC20Token,
  insuranceToken: ERC20Token,
  provider: BaseProvider,
  address: string
) {
  const now = Math.floor(Date.now() / 1000);

  const updateErc20BM = async () => {
    const bondBalance = bondToken.balanceOf(address);
    const insuranceBalance = insuranceToken.balanceOf(address);
    const result = await Promise.all([bondBalance, insuranceBalance]);

    app.ports.sdkPositionsMsg.send([
      {
        asset,
        collateral,
        maturity,
        bond: result[0].toString(),
        insurance: result[1].toString(),
      },
    ]);
  };

  const updateErc20AM = async () => {
    const bondBalance = bondToken.balanceOf(address);
    const insuranceBalance = insuranceToken.balanceOf(address);
    const result = await Promise.all([bondBalance, insuranceBalance]);

    const { asset: assetOut, collateral: collateralOut } =
      await pool.calculateWithdraw({
        bond: new Uint128(result[0]),
        insurance: new Uint128(result[1]),
      });

    app.ports.sdkPositionsMsg.send([
      {
        asset,
        collateral,
        maturity,
        bond: result[0].toString(),
        insurance: result[1].toString(),
        assetOut: assetOut.toString(),
        collateralOut: collateralOut.toString(),
      },
    ]);
  };

  const bondTokenContract = new Contract(bondToken.address, erc20Abi, provider);
  const insuranceTokenContract = new Contract(
    insuranceToken.address,
    erc20Abi,
    provider
  );

  const updateBalancesBM = () => {
    updateErc20Balance(bondTokenContract, address, updateErc20BM);
    updateErc20Balance(insuranceTokenContract, address, updateErc20BM);
  };

  const updateBalancesAM = () => {
    updateErc20Balance(bondTokenContract, address, updateErc20AM);
    updateErc20Balance(insuranceTokenContract, address, updateErc20AM);
  };

  if (now < maturity) {
    updateBalancesBM();
    setTimeout(() => {
      updateErc20AM();
      updateBalancesAM();
    }, (maturity - now) * 1000);
  } else {
    updateBalancesAM();
  }
}
