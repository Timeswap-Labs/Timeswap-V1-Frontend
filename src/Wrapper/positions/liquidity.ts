import { Contract } from "@ethersproject/contracts";
import { GlobalParams } from "../global";
import erc20Abi from "../abi/erc20";
import { updateTransferEventBalance } from "../helper";

export async function liquidityPositionsInit(
  gp: GlobalParams,
  positionsOf: PositionsOf
): Promise<Liqs> {
  const liquidityTokens = positionsOf.natives.map(
    ({ natives: { liquidity } }) =>
      new Contract(liquidity, erc20Abi, gp.metamaskProviderMulti)
  );

  const promiseLiquidityBalances = [];
  for (const liquidityToken of liquidityTokens)
    promiseLiquidityBalances.push(liquidityToken.balanceOf(positionsOf.owner));

  const liquidityBalances: string[] = (
    await Promise.all(promiseLiquidityBalances)
  ).map((x) => x.toString());

  return positionsOf.natives.map(({ pool }, index) => ({
    pool,
    liq: liquidityBalances[index],
  }));
}

export function liquidityPositionsUpdate(
  app: ElmApp<Ports>,
  gp: GlobalParams,
  positionsOf: PositionsOf
) {
  const liquidityTokens = positionsOf.natives.map(
    ({ natives: { liquidity } }) =>
      new Contract(liquidity, erc20Abi, gp.metamaskProvider)
  );

  liquidityTokens.map((liquidityToken, index) => {
    updateTransferEventBalance(liquidityToken, positionsOf.owner, async () => {
      const balance = await liquidityToken.balanceOf(positionsOf.owner);
      const liq = balance.toString();

      app.ports.receivePositions.send({
        ...positionsOf,
        positions: {
          claims: [],
          dues: [],
          liqs: [{ pool: positionsOf.natives[index].pool, liq }],
        },
      });
    });
  });
}
