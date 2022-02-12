import { Contract } from "@ethersproject/contracts";
import { GlobalParams } from "../global";
import cdTokenAbi from "../abi/cdToken";
import pairAbi from "../abi/pair";
import { updateTransferEventBalance } from "../helper";
import { CONVENIENCE } from "@timeswap-labs/timeswap-v1-sdk";

export async function borrowPositionsInit(
  gp: GlobalParams,
  positionsOf: PositionsOf
): Promise<Dues> {
  const cdTokens = positionsOf.natives.map(
    ({ natives: { collateralizedDebt } }) =>
      new Contract(collateralizedDebt, cdTokenAbi, gp.metamaskProviderMulti)
  );

  const promiseCDTokenBalances = [];
  for (const cdToken of cdTokens) {
    promiseCDTokenBalances.push(cdToken.balanceOf(positionsOf.owner));
  }
  const cdTokenBalances: bigint[] = (
    await Promise.all(promiseCDTokenBalances)
  ).map((x) => x.toBigInt());

  const promiseCDTokenOwnerIndex: any[][] = [];
  for (const [index, cdToken] of cdTokens.entries()) {
    const temp = [];
    for (let i = 0n; i < cdTokenBalances[index]; i++) {
      temp.push(cdToken.tokenOfOwnerByIndex(positionsOf.owner, i));
    }
    promiseCDTokenOwnerIndex.push(temp);
  }
  const cdTokenOwnerIndex: string[][] = (
    await Promise.all(
      promiseCDTokenOwnerIndex.map(async (x) => await Promise.all(x))
    )
  ).map((x) => x.map((y) => y.toString()));

  const promiseCDTokenDues: any[][] = [];
  for (const [index, cdToken] of cdTokens.entries()) {
    const temp = [];
    for (const ownerIndex of cdTokenOwnerIndex[index]) {
      temp.push(cdToken.dueOf(ownerIndex));
    }
    promiseCDTokenDues.push(temp);
  }
  const cdTokenDues: {
    debt: string;
    collateral: string;
    startBlock: string;
  }[][] = (
    await Promise.all(promiseCDTokenDues.map(async (x) => await Promise.all(x)))
  ).map((x) =>
    x.map((y) => ({
      debt: y[0].toString(),
      collateral: y[1].toString(),
      startBlock: y[2].toString(),
    }))
  );

  return positionsOf.natives.map(({ pool }, index) => ({
    pool,
    dues: cdTokenOwnerIndex[index].map((tokenId, tokenIdIndex) => ({
      tokenId,
      due: {
        debt: cdTokenDues[index][tokenIdIndex].debt,
        collateral: cdTokenDues[index][tokenIdIndex].collateral,
      },
    })),
  }));
}

export function borrowPositionsUpdate(
  app: ElmApp<Ports>,
  gp: GlobalParams,
  positionsOf: PositionsOf
) {
  const cdMulticallTokens = positionsOf.natives.map(
    ({ natives: { collateralizedDebt } }) =>
      new Contract(collateralizedDebt, cdTokenAbi, gp.metamaskProvider)
  );

  const cdTokens = positionsOf.natives.map(
    ({ natives: { collateralizedDebt } }) =>
      new Contract(collateralizedDebt, cdTokenAbi, gp.metamaskProvider)
  );

  const updateFunction = async (pool: Pool, index: number) => {
    const cdTokenBalance: bigint = (
      await cdMulticallTokens[index].balanceOf(positionsOf.owner)
    ).toBigInt();

    const promiseCDTokenOwnerIndex = [];
    for (let i = 0n; i < cdTokenBalance; i++) {
      promiseCDTokenOwnerIndex.push(
        cdMulticallTokens[index].tokenOfOwnerByIndex(positionsOf.owner, i)
      );
    }
    const cdTokenOwnerIndex: string[] = (
      await Promise.all(promiseCDTokenOwnerIndex)
    ).map((x) => x.toString());

    const promiseCDTokenDues = [];
    for (const ownerIndex of cdTokenOwnerIndex) {
      promiseCDTokenDues.push(cdMulticallTokens[index].dueOf(ownerIndex));
    }
    const cdTokenDues: {
      debt: string;
      collateral: string;
      startBlock: string;
    }[] = (await Promise.all(promiseCDTokenDues)).map((x) => ({
      debt: x[0].toString(),
      collateral: x[1].toString(),
      startBlock: x[2].toString(),
    }));

    app.ports.receivePositions.send({
      ...positionsOf,
      positions: {
        claims: [],
        dues: [
          {
            pool,
            dues: cdTokenOwnerIndex.map((tokenId, tokenIdIndex) => ({
              tokenId,
              due: {
                debt: cdTokenDues[tokenIdIndex].debt,
                collateral: cdTokenDues[tokenIdIndex].collateral,
              },
            })),
          },
        ],
        liqs: [],
      },
    });
  };

  positionsOf.natives.map(async ({ pool }, index) => {
    updateTransferEventBalance(cdTokens[index], positionsOf.owner, async () =>
      updateFunction(pool, index)
    );

    const pairContract = new Contract(
      await cdTokens[index].pair(),
      pairAbi,
      gp.metamaskProvider
    );

    const payFilter = pairContract.filters.Pay(
      CONVENIENCE[positionsOf.chain],
      null,
      CONVENIENCE[positionsOf.chain]
    );

    pairContract.on(payFilter, (maturity) => {
      if (pool.maturity == maturity.toString()) {
        updateFunction(pool, index);
      }
    });
  });
}
