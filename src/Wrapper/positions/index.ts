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

  // const pools: PositionsOf = {
  //   chain: 4,
  //   owner: account,
  //   natives: [
  //     {
  //       pool: {
  //         asset: {
  //           address: "0x559a70084eb180d649080048a7e966602eacea9e",
  //           name: "TS Dai Stablecoin",
  //           symbol: "TS-DAI",
  //           decimals: 18,
  //         },
  //         collateral: {
  //           address: "0x521c1c37fdd245c313457fcbc6d5ed156647c2f3",
  //           name: "TS Matic Token",
  //           symbol: "TS-MATIC",
  //           decimals: 18,
  //         },
  //         maturity: 1642262400,
  //       },
  //       natives: {
  //         bond: "0xa44fb936c6fd7768511369e58633c0be6cb1c4c6",
  //         insurance: "0xb65f947a49229eccbcd822f9e06267fe6541d73e",
  //         liquidity: "0xeb6f38f88366cb085df1c24d65aabec01d184f45",
  //         collateralizedDebt: "0xb2db1106ba2e371a827b6cd3e02e5ecd32130653",
  //       },
  //     },
  //     {
  //       pool: {
  //         asset: {
  //           address: "0x559a70084eb180d649080048a7e966602eacea9e",
  //           name: "TS Dai Stablecoin",
  //           symbol: "TS-DAI",
  //           decimals: 18,
  //         },
  //         collateral: {
  //           address: "0x521c1c37fdd245c313457fcbc6d5ed156647c2f3",
  //           name: "TS Matic Token",
  //           symbol: "TS-MATIC",
  //           decimals: 18,
  //         },
  //         maturity: 1642262400,
  //       },
  //       natives: {
  //         bond: "0xd666b8c6c55c70364c73b7a0c63c7307a2a27909",
  //         insurance: "0xf8fc9712cc74d51ea55753dae064f3138a6c52b3",
  //         liquidity: "0xb9e8e68fce471766bcb016993d2473fb2ee01fc2",
  //         collateralizedDebt: "0x726c9448adec7bd555fb1cac0aa82d1a60adfcbd",
  //       },
  //     },
  //     {
  //       pool: {
  //         asset: {
  //           address: "0x559a70084eb180d649080048a7e966602eacea9e",
  //           name: "TS Dai Stablecoin",
  //           symbol: "TS-DAI",
  //           decimals: 18,
  //         },
  //         collateral: {
  //           address: "0x521c1c37fdd245c313457fcbc6d5ed156647c2f3",
  //           name: "TS Matic Token",
  //           symbol: "TS-MATIC",
  //           decimals: 18,
  //         },
  //         maturity: 1642262400,
  //       },
  //       natives: {
  //         bond: "0xbf4d42d8ba3b97508acc5741d241a7bdb52edf73",
  //         insurance: "0x288e54ca2d70dfac9e6b9ecb77255bbb56c8e289",
  //         liquidity: "0xdee7d8f36771de7a605a9727768668a61afe8f77",
  //         collateralizedDebt: "0xb7e17b16e8f08638f0a10393f882aae16767bb29",
  //       },
  //     },
  //     {
  //       pool: {
  //         asset: {
  //           address: "0x559a70084eb180d649080048a7e966602eacea9e",
  //           name: "TS Dai Stablecoin",
  //           symbol: "TS-DAI",
  //           decimals: 18,
  //         },
  //         collateral: {
  //           address: "0x521c1c37fdd245c313457fcbc6d5ed156647c2f3",
  //           name: "TS Matic Token",
  //           symbol: "TS-MATIC",
  //           decimals: 18,
  //         },
  //         maturity: 1642262400,
  //       },
  //       natives: {
  //         bond: "0x1298e8862fc2fbbcde40879bbcd3fc6e1e95b295",
  //         insurance: "0x1e98b96865f4e33f0a3dbccba419bd5d881c8df0",
  //         liquidity: "0x44d806040530b595094c99da4a012a6a868cebdb",
  //         collateralizedDebt: "0x2ece230c0682213b5686767554721b49468de62a",
  //       },
  //     },
  //   ],
  // };

  // lendPositionsInit(gp, pools);
  // borrowPositionsInit(gp, pools);
  // liquidityPositionsInit(gp, pools);
}
