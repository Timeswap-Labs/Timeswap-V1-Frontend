import { Contract } from "@ethersproject/contracts";
import { GlobalParams } from "../global";
import erc20Abi from "../abi/erc20";

export async function lendPositionsInit(
  gp: GlobalParams,
  positionsOf: PositionsOf
): Promise<Claims> {
  const bondPrincipalTokens = positionsOf.natives.map(
    ({ natives: { bondPrincipal } }) =>
      new Contract(bondPrincipal, erc20Abi, gp.metamaskProviderMulti)
  );
  const bondInterestTokens = positionsOf.natives.map(
    ({ natives: { bondInterest } }) =>
      new Contract(bondInterest, erc20Abi, gp.metamaskProviderMulti)
  );
  const insurancePrincipalTokens = positionsOf.natives.map(
    ({ natives: { insurancePrincipal } }) =>
      new Contract(insurancePrincipal, erc20Abi, gp.metamaskProviderMulti)
  );
  const insuranceInterestTokens = positionsOf.natives.map(
    ({ natives: { insuranceInterest } }) =>
      new Contract(insuranceInterest, erc20Abi, gp.metamaskProviderMulti)
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
