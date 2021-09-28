import { BaseProvider } from "@ethersproject/providers";
import { WhiteList } from "./whitelist";
import { updateErc20Balance } from "./helper";
import { Uint128 } from "@timeswap-labs/timeswap-v1-sdk-core";
import { ERC20Token, Pool } from "@timeswap-labs/timeswap-v1-sdk";
import erc20 from "./abi/erc20";
import { Contract } from "@ethersproject/contracts";

export async function lendPositionsInit(
  app: ElmApp<Ports>,
  whitelist: WhiteList,
  provider: BaseProvider,
  address: string
) {
  const lendPositionsToken: {
    asset: string;
    collateral: string;
    maturity: number;
    pool: Pool;
  }[] = [];
  const lendPositions: any[] = [];

  for (const {
    asset,
    collateral,
    maturity,
    pool,
    bond,
    insurance,
  } of whitelist.poolEntries()) {
    const bondToken = bond.connect(provider);
    const insuranceToken = insurance.connect(provider);

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

    if (!(bond === "0" && insurance === "0")) {
      if (maturity <= now) {
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
            bond,
            insurance,
            assetOut: assetOut.value.toString(),
            collateralOut: collateralOut.value.toString(),
          },
        ]);
      } else {
        app.ports.sdkPositionsMsg.send([
          {
            asset,
            collateral,
            maturity,
            bond,
            insurance,
          },
        ]);
      }
    }
  }
}

export async function borrowPositionsInit(
  app: ElmApp<Ports>,
  whitelist: WhiteList,
  provider: BaseProvider,
  address: string
) {
  app.ports.sdkPositionsMsg.send(
    await Promise.all(
      whitelist
        .poolEntries()
        .map(async ({ asset, collateral, maturity, collateralizedDebt }) => {
          const cdToken = collateralizedDebt.connect(provider);
          const startBlock = (await cdToken.dueOf(0))[2] - 1;

          return { asset, collateral, maturity, cdToken, startBlock };
        })
        .map(async (params) => {
          const { asset, collateral, maturity, cdToken, startBlock } =
            await params;

          const tokenBalanceIn = cdToken.filters.Transfer(null, address);
          const tokenBalanceOut = cdToken.filters.Transfer(address);

          const tokenIdsIn = cdToken.queryFilter(tokenBalanceIn, startBlock);
          const tokenIdsOut = cdToken.queryFilter(tokenBalanceOut, startBlock);

          return {
            asset,
            collateral,
            maturity,
            cdToken,
            tokenIdsIn,
            tokenIdsOut,
          };
        })
        .map(async (params) => {
          const {
            asset,
            collateral,
            maturity,
            cdToken,
            tokenIdsIn,
            tokenIdsOut,
          } = await params;

          const tokenIds = new Map<string, number>();

          (await tokenIdsIn)
            .map((event) => {
              return {
                blockNumber: event.blockNumber,
                id: event.args![2].toString(),
              };
            })
            .forEach(({ blockNumber, id }) => {
              tokenIds.set(id, blockNumber);
            });
          (await tokenIdsOut)
            .map((event) => {
              return {
                blockNumber: event.blockNumber,
                id: event.args![2].toString(),
              };
            })
            .forEach(({ blockNumber, id }) => {
              if (tokenIds.get(id)! < blockNumber) {
                tokenIds.delete(id);
              }
            });

          return {
            asset,
            collateral,
            maturity,
            cdToken,
            tokenIds: Array.from(tokenIds.keys()),
          };
        })
        .map(async (params) => {
          const { asset, collateral, maturity, cdToken, tokenIds } =
            await params;

          const dues = tokenIds.map((id) => cdToken.dueOf(id));

          return { asset, collateral, maturity, dues, tokenIds };
        })
        .map(async (params) => {
          const { asset, collateral, maturity, dues, tokenIds } = await params;

          return {
            asset,
            collateral,
            maturity,
            dues: (await Promise.all(dues))
              .map(([debt, collateral], index) => {
                return {
                  id: tokenIds[index],
                  debt: debt.toString(),
                  collateral: collateral.toString(),
                };
              })
              .filter(
                ({ debt, collateral }) => !(debt === "0" && collateral === "0")
              ),
          };
        })
    )
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
        bond: result[0].value.toString(),
        insurance: result[1].value.toString(),
      },
    ]);
  };

  const updateErc20AM = async () => {
    const bondBalance = bondToken.balanceOf(address);
    const insuranceBalance = insuranceToken.balanceOf(address);
    const result = await Promise.all([bondBalance, insuranceBalance]);

    const { asset: assetOut, collateral: collateralOut } =
      await pool.calculateWithdraw({
        bond: new Uint128(result[0].value),
        insurance: new Uint128(result[1].value),
      });

    app.ports.sdkPositionsMsg.send([
      {
        asset,
        collateral,
        maturity,
        bond: result[0].toString(),
        insurance: result[1].toString(),
        assetOut: assetOut.value.toString(),
        collateralOut: collateralOut.value.toString(),
      },
    ]);
  };

  const bondTokenContract = new Contract(
    bondToken.address,
    erc20,
    bondToken.provider()
  );
  const insuranceTokenContract = new Contract(
    insuranceToken.address,
    erc20,
    insuranceToken.provider()
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
