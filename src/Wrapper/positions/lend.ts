import { Contract } from "@ethersproject/contracts";
import { GlobalParams } from "../global";
import erc20Abi from "../abi/erc20";
import { updateTransferEventBalance } from "../helper";

export async function lendPositionsInit(
  gp: GlobalParams,
  positionsOf: PositionsOf
): Promise<Claims> {
  const allClaims = positionsOf.allNatives.map(async (convData) => {
    const promiseBondPrincipalBalances: any = [];
    const promiseBondInterestBalances: any = [];
    const promiseInsurancePrincipalBalances: any = [];
    const promiseInsuranceInterestBalances: any = [];

    convData.nativeResponse.map(
      ({
        natives: {
          bondPrincipal,
          bondInterest,
          insurancePrincipal,
          insuranceInterest,
        },
      }) => {
        const bondPrincipalToken = new Contract(
          bondPrincipal,
          erc20Abi,
          gp.walletProviderMulti
        );
        promiseBondPrincipalBalances.push(
          bondPrincipalToken.balanceOf(positionsOf.owner)
        );

        const bondInterestToken = new Contract(
          bondInterest,
          erc20Abi,
          gp.walletProviderMulti
        );
        promiseBondInterestBalances.push(
          bondInterestToken.balanceOf(positionsOf.owner)
        );

        const insurancePrincipalToken = new Contract(
          insurancePrincipal,
          erc20Abi,
          gp.walletProviderMulti
        );
        promiseInsurancePrincipalBalances.push(
          insurancePrincipalToken.balanceOf(positionsOf.owner)
        );

        const insuranceInterestToken = new Contract(
          insuranceInterest,
          erc20Abi,
          gp.walletProviderMulti
        );
        promiseInsuranceInterestBalances.push(
          insuranceInterestToken.balanceOf(positionsOf.owner)
        );
      }
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

    const convClaims = convData.nativeResponse.map(({ pool }, index) => ({
      pool,
      claim: {
        bondPrincipal: bondPrincipalBalances[index],
        bondInterest: bondInterestBalances[index],
        insurancePrincipal: insurancePrincipalBalances[index],
        insuranceInterest: insuranceInterestBalances[index],
      },
    }));

    return {
      convAddress: convData.convAddress,
      pools: convClaims,
    };
  });

  return Promise.all(allClaims);
}

export function lendPositionsUpdate(
  app: ElmApp<Ports>,
  gp: GlobalParams,
  positionsOf: PositionsOf
) {
  positionsOf.allNatives.map(async (convData) => {
    const bondPrincipalMulticallTokens: any = [];
    const bondInterestMulticallTokens: any = [];
    const insurancePrincipalMulticallTokens: any = [];
    const insuranceInterestMulticallTokens: any = [];
    const bondPrincipalTokens: any = [];
    const bondInterestTokens: any = [];
    const insurancePrincipalTokens: any = [];
    const insuranceInterestTokens: any = [];

    convData.nativeResponse.map(
      ({
        natives: {
          bondPrincipal,
          bondInterest,
          insurancePrincipal,
          insuranceInterest,
        },
      }) => {
        bondPrincipalMulticallTokens.push(
          new Contract(bondPrincipal, erc20Abi, gp.walletProviderMulti)
        );

        bondInterestMulticallTokens.push(
          new Contract(bondInterest, erc20Abi, gp.walletProviderMulti)
        );
        insurancePrincipalMulticallTokens.push(
          new Contract(insurancePrincipal, erc20Abi, gp.walletProviderMulti)
        );

        insuranceInterestMulticallTokens.push(
          new Contract(insuranceInterest, erc20Abi, gp.walletProviderMulti)
        );

        bondPrincipalTokens.push(
          new Contract(bondPrincipal, erc20Abi, gp.walletProvider)
        );

        bondInterestTokens.push(
          new Contract(bondInterest, erc20Abi, gp.walletProvider)
        );

        insurancePrincipalTokens.push(
          new Contract(insurancePrincipal, erc20Abi, gp.walletProvider)
        );

        insuranceInterestTokens.push(
          new Contract(insuranceInterest, erc20Abi, gp.walletProvider)
        );
      }
    );

    const updateFunction = async (
      convAddress: string,
      pool: Pool,
      index: number
    ) => {
      const promiseBalances = [
        bondPrincipalMulticallTokens[index].balanceOf(positionsOf.owner),
        bondInterestMulticallTokens[index].balanceOf(positionsOf.owner),
        insurancePrincipalMulticallTokens[index].balanceOf(positionsOf.owner),
        insuranceInterestMulticallTokens[index].balanceOf(positionsOf.owner),
      ];
      const balances = await Promise.all(promiseBalances);

      const pools = convData.nativeResponse.map((_) => ({
        pool,
        claim: {
          bondPrincipal: balances[0].toString(),
          bondInterest: balances[1].toString(),
          insurancePrincipal: balances[2].toString(),
          insuranceInterest: balances[3].toString(),
        },
      }));

      app.ports.receivePositions.send({
        ...positionsOf,
        positions: {
          claims: [{ convAddress, pools }],
          dues: [],
          liqs: [],
        },
      });
    };

    bondPrincipalTokens.map((bondPrincipalToken: Contract, index: number) => {
      updateTransferEventBalance(bondPrincipalToken, positionsOf.owner, () =>
        updateFunction(
          convData.convAddress,
          convData.nativeResponse[index].pool,
          index
        )
      );
    });

    bondInterestTokens.map((bondInterestToken: Contract, index: number) => {
      updateTransferEventBalance(bondInterestToken, positionsOf.owner, () =>
        updateFunction(
          convData.convAddress,
          convData.nativeResponse[index].pool,
          index
        )
      );
    });

    bondInterestTokens.map(
      (insurancePrincipalToken: Contract, index: number) => {
        updateTransferEventBalance(
          insurancePrincipalToken,
          positionsOf.owner,
          () =>
            updateFunction(
              convData.convAddress,
              convData.nativeResponse[index].pool,
              index
            )
        );
      }
    );

    bondInterestTokens.map(
      (insuranceInterestToken: Contract, index: number) => {
        updateTransferEventBalance(
          insuranceInterestToken,
          positionsOf.owner,
          () =>
            updateFunction(
              convData.convAddress,
              convData.nativeResponse[index].pool,
              index
            )
        );
      }
    );
  });
}
