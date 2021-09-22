import { Provider } from "@ethersproject/providers";
import { WhiteList } from "./whitelist";
import { updateErc20Balance } from "./helper";
import { Uint128 } from "@timeswap-labs/timeswap-v1-sdk-core";

export async function positionsInit(
  app: ElmApp<Ports>,
  whitelist: WhiteList,
  provider: Provider,
  address: string
) {
  const positions: SdkPositionsMsg[] = [];

  for (const {
    asset,
    collateral,
    maturity,
    pool,
    bond,
    insurance,
    collateralizedDebt,
  } of whitelist.poolEntries()) {
    const currentTime = Math.floor(Date.now() / 1000);

    const bondToken = bond.connect(provider);
    const insuranceToken = insurance.connect(provider);

    const bondBalance = await bondToken.balanceOf(address);
    const insuranceBalance = await insuranceToken.balanceOf(address);

    const updateBalances = async () => {
      const bondBalance = await bondToken.balanceOf(address);
      const insuranceBalance = await insuranceToken.balanceOf(address);

      app.ports.sdkPositionsMsg.send([
        {
          asset,
          collateral,
          maturity,
          bond: bondBalance.value.toString(),
          insurance: insuranceBalance.value.toString(),
        },
      ]);
    };

    updateErc20Balance(bondToken.contract(), address, updateBalances);
    updateErc20Balance(insuranceToken.contract(), address, updateBalances);

    if (!(bondBalance.value === 0n && insuranceBalance.value === 0n)) {
      if (maturity <= currentTime) {
        const { asset: assetOut, collateral: collateralOut } =
          await pool.calculateWithdraw({
            bond: new Uint128(bondBalance),
            insurance: new Uint128(0),
          });
        positions.push({
          asset,
          collateral,
          maturity,
          bond: bondBalance.value.toString(),
          insurance: insuranceBalance.value.toString(),
          assetOut: assetOut.value.toString(),
          collateralOut: collateralOut.value.toString(),
        });
      } else {
        positions.push({
          asset,
          collateral,
          maturity,
          bond: bondBalance.value.toString(),
          insurance: insuranceBalance.value.toString(),
        });
      }
    }

    const tokenBalanceIn = collateralizedDebt.filters.Transfer(null, address);
    const tokenBalanceOut = collateralizedDebt.filters.Transfer(address);

    const cdToken = collateralizedDebt.connect(provider);

    const startBlock = (await cdToken.dueOf(0)).startBlock - 1;

    const tokenIdsIn = (
      await cdToken.queryFilter(tokenBalanceIn, startBlock)
    ).map((event) => event.args.tokenId);
    const tokenIdsOut = (
      await cdToken.queryFilter(tokenBalanceOut, startBlock)
    ).map((event) => event.args.tokenId);

    const tokenIds = tokenIdsIn.filter((id) => !tokenIdsOut.includes(id));
    const dues = await Promise.all(
      tokenIds.map(async (id) => {
        const due = await cdToken.dueOf(id);

        if (maturity > currentTime) {
          console.log("Asset : ", whitelist.getToken(asset).symbol);
          console.log("Collateral : ", whitelist.getToken(collateral).symbol);
          console.log("Maturity : ", new Date(maturity * 1000));
          console.log("ID : ", id.toString());
          console.log("Debt : ", due.debt.toString());
          console.log("Collateral : ", due.collateral.toString());
        }

        return {
          id: id.toString(),
          debt: due.debt.toString(),
          collateral: due.collateral.toString(),
        };
      })
    );

    if (dues.length !== 0) {
      positions.push({ asset, collateral, maturity, dues });
    }
  }

  app.ports.sdkPositionsMsg.send(positions);
}
