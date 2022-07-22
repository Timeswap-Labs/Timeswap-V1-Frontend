import { Contract } from "@ethersproject/contracts";
import { GlobalParams } from "../global";
import erc20Abi from "../abi/erc20";
import { updateTransferEventBalance } from "../helper";

export async function liquidityPositionsInit(
  gp: GlobalParams,
  positionsOf: PositionsOf
): Promise<Liqs> {
  const allLiqs = positionsOf.allNatives.map(async (convData) => {
    const liquidityTokens = convData.nativeResponse.map(
      ({ natives: { liquidity } }) =>
        new Contract(liquidity, erc20Abi, gp.walletProviderMulti)
    );

    const promiseLiquidityBalances = [];
    for (const liquidityToken of liquidityTokens)
      promiseLiquidityBalances.push(
        liquidityToken.balanceOf(positionsOf.owner)
      );

    const liquidityBalances: string[] = (
      await Promise.all(promiseLiquidityBalances)
    ).map((x) => x.toString());

    const convLiqs = convData.nativeResponse.map(({ pool }, index) => ({
      pool,
      liq: liquidityBalances[index],
    }));

    return {
      convAddress: convData.convAddress,
      pools: convLiqs,
    };
  });

  return Promise.all(allLiqs);
}

export function liquidityPositionsUpdate(
  app: ElmApp<Ports>,
  gp: GlobalParams,
  positionsOf: PositionsOf
) {
  positionsOf.allNatives.map(async (convData) => {
    const liquidityTokens = convData.nativeResponse.map(
      ({ natives: { liquidity } }) =>
        new Contract(liquidity, erc20Abi, gp.walletProvider)
    );

    liquidityTokens.map((liquidityToken, index) => {
      updateTransferEventBalance(
        liquidityToken,
        positionsOf.owner,
        async () => {
          const promiseLiquidityBalances = [];
          for (const liquidityToken of liquidityTokens)
            promiseLiquidityBalances.push(
              liquidityToken.balanceOf(positionsOf.owner)
            );

          const liquidityBalances: string[] = (
            await Promise.all(promiseLiquidityBalances)
          ).map((x) => x.toString());

          const pools = convData.nativeResponse.map(({ pool }, index) => ({
            pool,
            liq: liquidityBalances[index],
          }));

          app.ports.receivePositions.send({
            ...positionsOf,
            positions: {
              claims: [],
              dues: [],
              liqs: [
                {
                  convAddress: convData.convAddress,
                  pools,
                },
              ],
            },
          });
        }
      );
    });
  });
}
