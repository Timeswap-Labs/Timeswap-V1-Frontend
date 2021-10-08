import { ERC20Token, NativeToken } from "@timeswap-labs/timeswap-v1-sdk";
import { WhiteList } from "./whitelist";
import { Uint256 } from "@timeswap-labs/timeswap-v1-sdk-core";
import { updateErc20Balance } from "./helper";
import erc20 from "./abi/erc20";
import { Contract } from "@ethersproject/contracts";
import { GlobalParams } from "./global";

export async function balancesInit(
  app: ElmApp<Ports>,
  whitelist: WhiteList,
  gp: GlobalParams,
  address: string
) {
  const balancesToken: string[] = [];
  const balances: Promise<Uint256>[] = [];
  const allowancesToken: string[] = [];
  const allowances: Promise<Uint256>[] = [];

  for (const [tokenAddress, token] of whitelist.tokenEntries()) {
    if (token instanceof ERC20Token) {
      const connectedToken = token.connect(gp.metamaskProviderMulti);

      balancesToken.push(tokenAddress);
      balances.push(connectedToken.balanceOf(address));

      allowancesToken.push(tokenAddress);
      allowances.push(connectedToken.allowance(address, whitelist.convenience));

      const contract = new Contract(
        connectedToken.address,
        erc20,
        gp.metamaskProvider
      );

      const allowanceFilter = contract.filters.Approval(
        address,
        whitelist.convenience
      );

      updateErc20Balance(contract, address, async () => {
        const balance = await contract.balanceOf(address);
        app.ports.sdkBalancesMsg.send([
          {
            token: tokenAddress,
            balance: balance.toString(),
          },
        ]);
      });

      contract.on(allowanceFilter, (_owner, _spender, value) => {
        app.ports.sdkAllowancesMsg.send([
          {
            erc20: tokenAddress,
            allowance: value.toString(),
          },
        ]);
      });
    } else if (token instanceof NativeToken) {
      balancesToken.push(tokenAddress);
      balances.push(
        token.connect(gp.metamaskProviderMulti).getBalance(address)
      );

      gp.metamaskProvider.on("block", async () => {
        const balance = await token
          .connect(gp.metamaskProviderMulti)
          .getBalance(address);
        app.ports.sdkBalancesMsg.send([
          { token: tokenAddress, balance: balance.toString() },
        ]);
      });
    }
  }

  const balancesResult = await Promise.all(balances);
  const allowancesResult = await Promise.all(allowances);

  app.ports.sdkBalancesMsg.send(
    balancesToken.map((tokenAddress, index) => {
      return {
        token: tokenAddress,
        balance: balancesResult[index].toString(),
      };
    })
  );
  app.ports.sdkAllowancesMsg.send(
    allowancesToken.map((tokenAddress, index) => {
      return {
        erc20: tokenAddress,
        allowance: allowancesResult[index].toString(),
      };
    })
  );
}
