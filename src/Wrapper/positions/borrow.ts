import { Contract } from "@ethersproject/contracts";
import { GlobalParams } from "../global";
import cdTokenAbi from "../abi/cdToken";

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

  console.log("cdTokenBalances", cdTokenBalances);

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

  console.log("cdTokenOwnerIndex", cdTokenOwnerIndex);

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

  console.log("cdTokenDues", cdTokenDues);

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
