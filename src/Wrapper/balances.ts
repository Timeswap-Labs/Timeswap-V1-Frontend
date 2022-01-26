import { CONVENIENCE } from "@timeswap-labs/timeswap-v1-sdk/src/constants/index";
import { Uint256 } from "@timeswap-labs/timeswap-v1-sdk-core";
import { updateErc20Balance } from "./helper";
import erc20Abi from "./abi/erc20";
import { Contract } from "@ethersproject/contracts";
import { GlobalParams } from "./global";
import { BalancesOf, ERC20Token, NativeToken, Ports } from "./declaration";

export async function balancesInit(
  app: ElmApp<Ports>,
  gp: GlobalParams,
  balancesOf: BalancesOf
) {
  const balancesToken: (ERC20Token | NativeToken)[] = [];
  const balances: Promise<any>[] = [];
  const allowancesToken: ERC20Token[] = [];
  const allowances: Promise<any>[] = [];

  for (const token of balancesOf.tokens) {
    if (token && (token as ERC20Token).address) {
      const contract = new Contract(
        (token as ERC20Token).address,
        erc20Abi,
        gp.metamaskProviderMulti
      );

      balancesToken.push(token as ERC20Token);
      balances.push(contract.balanceOf(balancesOf.address));

      allowancesToken.push(token as ERC20Token);
      allowances.push(
        contract.allowance(
          balancesOf.address,
          CONVENIENCE[balancesOf.chain.chainId]
        )
      );

      const allowanceFilter = contract.filters.Approval(
        balancesOf.address,
        CONVENIENCE[balancesOf.chain.chainId]
      );

      updateErc20Balance(contract, balancesOf.address, async () => {
        const balance = await contract.balanceOf(balancesOf.address);
        app.ports.receiveBalances.send({
          chain: balancesOf.chain,
          address: balancesOf.address,
          tokens: balancesOf.tokens,
          balances: [balance.toString()],
        });
      });

      contract.on(allowanceFilter, (_owner, _spender, value) => {
        app.ports.receiveAllowances.send({
          chain: balancesOf.chain,
          address: balancesOf.address,
          erc20s: balancesOf.tokens as ERC20Token[],
          allowances: [value.toString()],
        });
      });
    } else {
      balancesToken.push(token);
      balances.push(gp.metamaskProviderMulti.getBalance(balancesOf.address));

      gp.metamaskProvider.on("block", async () => {
        const balance = await gp.metamaskProvider.getBalance(
          balancesOf.address
        );
        app.ports.receiveBalances.send({
          chain: balancesOf.chain,
          address: balancesOf.address,
          tokens: balancesOf.tokens,
          balances: [balance.toString()],
        });
      });
    }
  }

  const balancesResult = await Promise.all(balances);
  const allowancesResult = await Promise.all(allowances);

  app.ports.receiveBalances.send({
    chain: balancesOf.chain,
    address: balancesOf.address,
    tokens: balancesOf.tokens,
    balances: balancesToken.map((token, index) =>
      balancesResult[index].toString()
    ),
  });
  app.ports.receiveAllowances.send({
    chain: balancesOf.chain,
    address: balancesOf.address,
    erc20s: allowancesToken,
    allowances: allowancesToken.map((token, index) =>
      allowancesResult[index].toString()
    ),
  });
}
