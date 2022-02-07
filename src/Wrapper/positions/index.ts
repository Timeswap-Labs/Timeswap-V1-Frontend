import { GlobalParams } from "../global";
import { lendPositionsInit } from "./lend";
import { borrowPositionsInit } from "./borrow";
import { liquidityPositionsInit } from "./liquidity";

export async function positionsInit(app: ElmApp<Ports>, gp: GlobalParams) {
  app.ports.positionsOf.subscribe(async (positionsOf) => {
    const claimsPromise = lendPositionsInit(gp, positionsOf);
    const duesPromise = borrowPositionsInit(gp, positionsOf);
    const liqsPromise = liquidityPositionsInit(gp, positionsOf);

    const claims = await claimsPromise;
    const dues = await duesPromise;
    const liqs = await liqsPromise;

    app.ports.receivePositions.send({
      chain: positionsOf.chain,
      owner: positionsOf.owner,
      positions: {
        claims,
        dues,
        liqs,
      },
    });
  });
}
