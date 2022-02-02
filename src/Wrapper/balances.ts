import { CONVENIENCE } from "@timeswap-labs/timeswap-v1-sdk";
import { updateErc20Balance } from "./helper";
import erc20Abi from "./abi/erc20";
import { Contract } from "@ethersproject/contracts";
import { GlobalParams } from "./global";
// import { BalancesOf, ERC20Token, NativeToken, Ports } from "./declaration";

export async function balancesAllowancesInit(
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

      const subscriptionContract = new Contract(
        (token as ERC20Token).address,
        erc20Abi,
        gp.metamaskProvider
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

      const allowanceFilter = subscriptionContract.filters.Approval(
        balancesOf.address,
        CONVENIENCE[balancesOf.chain.chainId]
      );

      updateErc20Balance(subscriptionContract, balancesOf.address, async () => {
        const balance = await subscriptionContract.balanceOf(balancesOf.address);

        app.ports.receiveBalances.send({
          chain: balancesOf.chain,
          address: balancesOf.address,
          tokens: [token],
          balances: [balance.toString()],
        });
      });

      subscriptionContract.on(allowanceFilter, (_owner, _spender, value) => {
        app.ports.receiveAllowances.send({
          chain: balancesOf.chain,
          address: balancesOf.address,
          erc20s: [token as ERC20Token],
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
          tokens: [token],
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

export async function fetchBalancesOf(app: ElmApp<Ports>, gp: GlobalParams, balancesOf: BalancesOf) {
  const balancesToken: (ERC20Token | NativeToken)[] = [];
  const balances: Promise<any>[] = [];

  for (const token of balancesOf.tokens) {
    if (token && (token as ERC20Token).address) {
      const contract = new Contract(
        (token as ERC20Token).address,
        erc20Abi,
        gp.metamaskProviderMulti
      );

      balancesToken.push(token as ERC20Token);
      balances.push(contract.balanceOf(balancesOf.address));

      updateErc20Balance(contract, balancesOf.address, async () => {
        const balance = await contract.balanceOf(balancesOf.address);
        app.ports.receiveBalances.send({
          chain: balancesOf.chain,
          address: balancesOf.address,
          tokens: [token],
          balances: [balance.toString()],
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
          tokens: [token],
          balances: [balance.toString()],
        });
      });
    }
  }

  const balancesResult = await Promise.all(balances);

  app.ports.receiveBalances.send({
    chain: balancesOf.chain,
    address: balancesOf.address,
    tokens: balancesOf.tokens,
    balances: balancesToken.map((token, index) =>
      balancesResult[index].toString()
    ),
  });
}

export async function fetchAllowancesOf(
  app: ElmApp<Ports>,
  gp: GlobalParams,
  allowancesOf: { chain: Chain, address: string, erc20s: ERC20Token[]}
) {
  const allowancesToken: ERC20Token[] = [];
  const allowances: Promise<any>[] = [];

  for (const token of allowancesOf.erc20s) {
    if (token.address) {
      const contract = new Contract(
        token.address,
        erc20Abi,
        gp.metamaskProviderMulti
      );

      allowancesToken.push(token);
      allowances.push(
        contract.allowance(
          allowancesOf.address,
          CONVENIENCE[allowancesOf.chain.chainId]
        )
      );

      const allowanceFilter = contract.filters.Approval(
        allowancesOf.address,
        CONVENIENCE[allowancesOf.chain.chainId]
      );

      contract.on(allowanceFilter, (_owner, _spender, value) => {
        app.ports.receiveAllowances.send({
          chain: allowancesOf.chain,
          address: allowancesOf.address,
          erc20s: [token],
          allowances: [value.toString()],
        });
      });
    }
  }

  const allowancesResult = await Promise.all(allowances);

  app.ports.receiveAllowances.send({
    chain: allowancesOf.chain,
    address: allowancesOf.address,
    erc20s: allowancesToken,
    allowances: allowancesToken.map((token, index) =>
      allowancesResult[index].toString()
    ),
  });
}
