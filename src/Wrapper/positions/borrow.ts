import { Contract } from "@ethersproject/contracts";
import { GlobalParams } from "../global";
import cdTokenAbi from "../abi/cdToken";
import pairAbi from "../abi/pair";
import { updateTransferEventBalance } from "../helper";
import { CONVENIENCE } from "@timeswap-labs/timeswap-v1-biconomy-sdk";

export async function borrowPositionsInit(
  gp: GlobalParams,
  positionsOf: PositionsOf
): Promise<Dues> {
  const currentConvNatives = positionsOf.allNatives.find(
    (convData) =>
      convData.convAddress === CONVENIENCE[positionsOf.chain.chainId]
  );

  if (currentConvNatives) {
    const cdTokens = currentConvNatives.nativeResponse.map(
      ({ natives: { collateralizedDebt } }) =>
        new Contract(collateralizedDebt, cdTokenAbi, gp.walletProviderMulti)
    );

    const promiseCDTokenBalances = [];
    for (const cdToken of cdTokens) {
      // "balanceOf" gets the number of CD Tokens owned in that CDT contract
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
      await Promise.all(
        promiseCDTokenDues.map(async (x) => await Promise.all(x))
      )
    ).map((x) =>
      x.map((y) => ({
        debt: y[0].toString(),
        collateral: y[1].toString(),
        startBlock: y[2].toString(),
      }))
    );

    return currentConvNatives.nativeResponse.map(
      ({ pool, natives: { collateralizedDebt } }, index) => ({
        pool,
        collateralizedDebt,
        dues: cdTokenOwnerIndex[index].map((tokenId, tokenIdIndex) => ({
          tokenId,
          due: {
            debt: cdTokenDues[index][tokenIdIndex].debt,
            collateral: cdTokenDues[index][tokenIdIndex].collateral,
          },
        })),
      })
    );
  } else {
    return [];
  }
}

export function borrowPositionsUpdate(
  app: ElmApp<Ports>,
  gp: GlobalParams,
  positionsOf: PositionsOf
) {
  const currentConvNatives = positionsOf.allNatives.find(
    (convData) =>
      convData.convAddress === CONVENIENCE[positionsOf.chain.chainId]
  );
  if (currentConvNatives) {
    const cdMulticallTokens = currentConvNatives.nativeResponse.map(
      ({ natives: { collateralizedDebt } }) =>
        new Contract(collateralizedDebt, cdTokenAbi, gp.walletProvider)
    );

    const cdTokens = currentConvNatives.nativeResponse.map(
      ({ natives: { collateralizedDebt } }) =>
        new Contract(collateralizedDebt, cdTokenAbi, gp.walletProvider)
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

    currentConvNatives.nativeResponse.map(async ({ pool }, index) => {
      updateTransferEventBalance(cdTokens[index], positionsOf.owner, async () =>
        updateFunction(pool, index)
      );

      const pairContract = new Contract(
        await cdTokens[index].pair(),
        pairAbi,
        gp.walletProvider
      );

      const payFilter = pairContract.filters.Pay(
        CONVENIENCE[positionsOf.chain.chainId],
        null,
        CONVENIENCE[positionsOf.chain.chainId]
      );

      pairContract.on(payFilter, (maturity) => {
        if (pool.maturity == maturity.toString()) {
          updateFunction(pool, index);
        }
      });
    });
  }
}
