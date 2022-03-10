import { Contract } from "@ethersproject/contracts";
import { GlobalParams } from "../global";
import erc20Abi from "../abi/erc20";
import { updateTransferEventBalance } from "../helper";

export async function lendPositionsInit(
  gp: GlobalParams,
  positionsOf: PositionsOf
): Promise<Claims> {
  const bondPrincipalTokens = positionsOf.natives.map(
    ({ natives: { bondPrincipal } }) =>
      new Contract(bondPrincipal, erc20Abi, gp.walletProviderMulti)
  );
  const bondInterestTokens = positionsOf.natives.map(
    ({ natives: { bondInterest } }) =>
      new Contract(bondInterest, erc20Abi, gp.walletProviderMulti)
  );
  const insurancePrincipalTokens = positionsOf.natives.map(
    ({ natives: { insurancePrincipal } }) =>
      new Contract(insurancePrincipal, erc20Abi, gp.walletProviderMulti)
  );
  const insuranceInterestTokens = positionsOf.natives.map(
    ({ natives: { insuranceInterest } }) =>
      new Contract(insuranceInterest, erc20Abi, gp.walletProviderMulti)
  );

  const promiseBondPrincipalBalances = [];
  const promiseBondInterestBalances = [];
  const promiseInsurancePrincipalBalances = [];
  const promiseInsuranceInterestBalances = [];

  for (const bondPrincipalToken of bondPrincipalTokens)
    promiseBondPrincipalBalances.push(
      bondPrincipalToken.balanceOf(positionsOf.owner)
    );
  for (const bondInterestToken of bondInterestTokens)
    promiseBondInterestBalances.push(
      bondInterestToken.balanceOf(positionsOf.owner)
    );
  for (const insurancePrincipalToken of insurancePrincipalTokens)
    promiseInsurancePrincipalBalances.push(
      insurancePrincipalToken.balanceOf(positionsOf.owner)
    );
  for (const insuranceInterestToken of insuranceInterestTokens)
    promiseInsuranceInterestBalances.push(
      insuranceInterestToken.balanceOf(positionsOf.owner)
    );

  const bondPrincipalBalances: Uint[] = (
    await Promise.all(promiseBondPrincipalBalances)
  ).map((x) => x.toString());
  const bondInterestBalances: Uint[] = (
    await Promise.all(promiseBondInterestBalances)
  ).map((x) => x.toString());
  const insurancePrincipalBalances: Uint[] = (
    await Promise.all(promiseInsurancePrincipalBalances)
  ).map((x) => x.toString());
  const insuranceInterestBalances: Uint[] = (
    await Promise.all(promiseInsuranceInterestBalances)
  ).map((x) => x.toString());

  return positionsOf.natives.map(({ pool }, index) => ({
    pool,
    claim: {
      bondPrincipal: bondPrincipalBalances[index],
      bondInterest: bondInterestBalances[index],
      insurancePrincipal: insurancePrincipalBalances[index],
      insuranceInterest: insuranceInterestBalances[index],
    },
  }));
}

export function lendPositionsUpdate(
  app: ElmApp<Ports>,
  gp: GlobalParams,
  positionsOf: PositionsOf
) {
  const bondPrincipalMulticallTokens = positionsOf.natives.map(
    ({ natives: { bondPrincipal } }) =>
      new Contract(bondPrincipal, erc20Abi, gp.walletProviderMulti)
  );
  const bondInterestMulticallTokens = positionsOf.natives.map(
    ({ natives: { bondInterest } }) =>
      new Contract(bondInterest, erc20Abi, gp.walletProviderMulti)
  );
  const insurancePrincipalMulticallTokens = positionsOf.natives.map(
    ({ natives: { insurancePrincipal } }) =>
      new Contract(insurancePrincipal, erc20Abi, gp.walletProviderMulti)
  );
  const insuranceInterestMulticallTokens = positionsOf.natives.map(
    ({ natives: { insuranceInterest } }) =>
      new Contract(insuranceInterest, erc20Abi, gp.walletProviderMulti)
  );

  const bondPrincipalTokens = positionsOf.natives.map(
    ({ natives: { bondPrincipal } }) =>
      new Contract(bondPrincipal, erc20Abi, gp.walletProvider)
  );
  const bondInterestTokens = positionsOf.natives.map(
    ({ natives: { bondInterest } }) =>
      new Contract(bondInterest, erc20Abi, gp.walletProvider)
  );
  const insurancePrincipalTokens = positionsOf.natives.map(
    ({ natives: { insurancePrincipal } }) =>
      new Contract(insurancePrincipal, erc20Abi, gp.walletProvider)
  );
  const insuranceInterestTokens = positionsOf.natives.map(
    ({ natives: { insuranceInterest } }) =>
      new Contract(insuranceInterest, erc20Abi, gp.walletProvider)
  );

  const updateFunction = async (pool: Pool, index: number) => {
    const promiseBalances = [
      bondPrincipalMulticallTokens[index].balanceOf(positionsOf.owner),
      bondInterestMulticallTokens[index].balanceOf(positionsOf.owner),
      insurancePrincipalMulticallTokens[index].balanceOf(positionsOf.owner),
      insuranceInterestMulticallTokens[index].balanceOf(positionsOf.owner),
    ];
    const balances = await Promise.all(promiseBalances);
    const claim = {
      bondPrincipal: balances[0].toString(),
      bondInterest: balances[1].toString(),
      insurancePrincipal: balances[2].toString(),
      insuranceInterest: balances[3].toString(),
    };

    app.ports.receivePositions.send({
      ...positionsOf,
      positions: {
        claims: [{ pool, claim }],
        dues: [],
        liqs: [],
      },
    });
  };

  bondPrincipalTokens.map((bondPrincipalToken, index) => {
    updateTransferEventBalance(bondPrincipalToken, positionsOf.owner, () =>
      updateFunction(positionsOf.natives[index].pool, index)
    );
  });
  bondInterestTokens.map((bondInterestToken, index) => {
    updateTransferEventBalance(bondInterestToken, positionsOf.owner, () =>
      updateFunction(positionsOf.natives[index].pool, index)
    );
  });
  insurancePrincipalTokens.map((insurancePrincipalToken, index) => {
    updateTransferEventBalance(insurancePrincipalToken, positionsOf.owner, () =>
      updateFunction(positionsOf.natives[index].pool, index)
    );
  });
  insuranceInterestTokens.map((insuranceInterestToken, index) => {
    updateTransferEventBalance(insuranceInterestToken, positionsOf.owner, () =>
      updateFunction(positionsOf.natives[index].pool, index)
    );
  });
}
