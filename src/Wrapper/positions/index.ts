import { GlobalParams } from "../global";
import { lendPositionsInit, lendPositionsUpdate } from "./lend";
import { borrowPositionsInit, borrowPositionsUpdate } from "./borrow";
import { liquidityPositionsInit, liquidityPositionsUpdate } from "./liquidity";

export async function positionsInit(app: ElmApp<Ports>, gp: GlobalParams) {
  app.ports.positionsOf.subscribe(async (positionsOf) => {
    const claimsPromise = lendPositionsInit(gp, positionsOf);
    const duesPromise = borrowPositionsInit(gp, positionsOf);
    const liqsPromise = liquidityPositionsInit(gp, positionsOf);

    const claims = await claimsPromise;
    const dues = await duesPromise;
    const liqs = await liqsPromise;

    console.log({
      claims,
      dues,
      liqs,
    });

    app.ports.receivePositions.send({
      ...positionsOf,
      positions: {
        claims,
        dues,
        liqs,
      },
    });

    lendPositionsUpdate(app, gp, positionsOf);
    borrowPositionsUpdate(app, gp, positionsOf);
    liquidityPositionsUpdate(app, gp, positionsOf);
  });
}
