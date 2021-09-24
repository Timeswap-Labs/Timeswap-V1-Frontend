import { BaseProvider } from "@ethersproject/providers";
import { WhiteList } from "./whitelist";
import { updateErc20Balance } from "./helper";
import { Uint128 } from "@timeswap-labs/timeswap-v1-sdk-core";
import { ERC20Token, Pool } from "@timeswap-labs/timeswap-v1-sdk";
import { Contract as MultiCallContract, Provider } from "ethcall";
import erc20 from "./abi/erc20";
import { FormatTypes, Interface } from "@ethersproject/abi";
import { Contract } from "@ethersproject/contracts";

const erc20Abi = JSON.parse(
  new Interface(erc20).format(FormatTypes.json) as string
);

export async function lendPositionsInit(
  app: ElmApp<Ports>,
  whitelist: WhiteList,
  provider: BaseProvider,
  address: string
) {
  const multiCallProvider = new Provider();
  multiCallProvider.init(provider);

  const lendPositionsToken: {
    asset: string;
    collateral: string;
    maturity: number;
    pool: Pool;
  }[] = [];
  const lendPositions: any[] = [];

  for (const {
    asset,
    collateral,
    maturity,
    pool,
    bond,
    insurance,
  } of whitelist.poolEntries()) {
    const bondToken = bond.connect(provider);
    const insuranceToken = insurance.connect(provider);
    const multiCallBondToken = new MultiCallContract(bond.address, erc20Abi);
    const multiCallInsuranceToken = new MultiCallContract(
      insurance.address,
      erc20Abi
    );

    lendPositionsToken.push({
      asset,
      collateral,
      maturity,
      pool,
    });
    lendPositions.push(multiCallBondToken.balanceOf(address));
    lendPositions.push(multiCallInsuranceToken.balanceOf(address));

    updateBalances(
      app,
      asset,
      collateral,
      maturity,
      pool,
      bondToken,
      insuranceToken,
      multiCallBondToken,
      multiCallInsuranceToken,
      multiCallProvider,
      address
    );
  }

  const result = await multiCallProvider.tryAll(lendPositions);

  for (const [
    index,
    { asset, collateral, maturity, pool },
  ] of lendPositionsToken.entries()) {
    const base = index * 2;
    const now = Math.floor(Date.now() / 1000);

    const bond = result[base].toString();
    const insurance = result[base + 1].toString();

    if (!(bond === "0" && insurance === "0")) {
      if (maturity <= now) {
        const { asset: assetOut, collateral: collateralOut } =
          await pool.calculateWithdraw({
            bond: new Uint128(bond),
            insurance: new Uint128(insurance),
          });
        app.ports.sdkPositionsMsg.send([
          {
            asset,
            collateral,
            maturity,
            bond,
            insurance,
            assetOut: assetOut.value.toString(),
            collateralOut: collateralOut.value.toString(),
          },
        ]);
      } else {
        app.ports.sdkPositionsMsg.send([
          {
            asset,
            collateral,
            maturity,
            bond,
            insurance,
          },
        ]);
      }
    }
  }
}

export async function borrowPositionsInit(
  app: ElmApp<Ports>,
  whitelist: WhiteList,
  provider: BaseProvider,
  address: string
) {
  const borrowPositions: SdkPositionsMsg[] = [];

  for (const {
    asset,
    collateral,
    maturity,
    collateralizedDebt,
  } of whitelist.poolEntries()) {
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
    const dues: {
      id: string;
      debt: string;
      collateral: string;
    }[] = [];

    for (const id of tokenIds) {
      const due = await cdToken.dueOf(id);

      if (!(due.debt.toBigInt() === 0n && due.collateral.toBigInt() === 0n)) {
        dues.push({
          id: id.toString(),
          debt: due.debt.toString(),
          collateral: due.collateral.toString(),
        });
      }
    }

    if (dues.length !== 0) {
      borrowPositions.push({ asset, collateral, maturity, dues });
    }
  }

  app.ports.sdkPositionsMsg.send(borrowPositions);
}

function updateBalances(
  app: ElmApp<Ports>,
  asset: string,
  collateral: string,
  maturity: number,
  pool: Pool,
  bondToken: ERC20Token,
  insuranceToken: ERC20Token,
  multiCallBondToken: MultiCallContract,
  multiCallInsuranceToken: MultiCallContract,
  multiCallProvider: Provider,
  address: string
) {
  const now = Math.floor(Date.now() / 1000);

  const updateErc20BM = async () => {
    const bondBalance = multiCallBondToken.balanceOf(address);
    const insuranceBalance = multiCallInsuranceToken.balanceOf(address);
    const result = await multiCallProvider.tryAll([
      bondBalance,
      insuranceBalance,
    ]);

    app.ports.sdkPositionsMsg.send([
      {
        asset,
        collateral,
        maturity,
        bond: result[0].toString(),
        insurance: result[1].toString(),
      },
    ]);
  };

  const updateErc20AM = async () => {
    const bondBalance = multiCallBondToken.balanceOf(address);
    const insuranceBalance = multiCallInsuranceToken.balanceOf(address);
    const result = await multiCallProvider.tryAll([
      bondBalance,
      insuranceBalance,
    ]);

    const { asset: assetOut, collateral: collateralOut } =
      await pool.calculateWithdraw({
        bond: new Uint128(result[0].toBigInt()),
        insurance: new Uint128(result[1].toBigInt()),
      });

    app.ports.sdkPositionsMsg.send([
      {
        asset,
        collateral,
        maturity,
        bond: result[0].toString(),
        insurance: result[1].toString(),
        assetOut: assetOut.value.toString(),
        collateralOut: collateralOut.value.toString(),
      },
    ]);
  };

  const bondTokenContract = new Contract(
    bondToken.address,
    erc20Abi,
    bondToken.provider()
  );
  const insuranceTokenContract = new Contract(
    insuranceToken.address,
    erc20Abi,
    insuranceToken.provider()
  );

  const updateBalancesBM = () => {
    updateErc20Balance(bondTokenContract, address, updateErc20BM);
    updateErc20Balance(insuranceTokenContract, address, updateErc20BM);
  };

  const updateBalancesAM = () => {
    updateErc20Balance(bondTokenContract, address, updateErc20AM);
    updateErc20Balance(insuranceTokenContract, address, updateErc20AM);
  };

  if (now < maturity) {
    updateBalancesBM();
    setTimeout(() => {
      updateErc20AM();
      updateBalancesAM();
    }, (maturity - now) * 1000);
  } else {
    updateBalancesAM();
  }
}
