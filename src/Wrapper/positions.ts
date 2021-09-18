import { Provider } from "@ethersproject/providers";
import { WhiteList } from "./whitelist";
import { updateErc20Balance } from "./helper";

export async function positionsInit(
  app: ElmApp<Ports>,
  whitelist: WhiteList,
  provider: Provider,
  address: string
) {
  const positions: SdkPositionsMsg[] = [];

  for (const [
    json,
    { bond, insurance, collateralizedDebt },
  ] of whitelist.poolEntries()) {
    const {
      asset,
      collateral,
      maturity,
    }: { asset: string; collateral: string; maturity: number } =
      JSON.parse(json);

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
        // Withdraw math get the asset and collateral and return it.
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
