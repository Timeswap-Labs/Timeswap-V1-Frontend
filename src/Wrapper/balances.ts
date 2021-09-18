import "regenerator-runtime/runtime";
import { Provider } from "@ethersproject/providers";
import { ERC20Token } from "@timeswap-labs/timeswap-v1-sdk";
import { WhiteList } from "./whitelist";
import { NativeToken } from "@timeswap-labs/timeswap-v1-sdk-core";
import type { IERC20 } from "./typechain";
import { updateErc20Balance } from "./helper";

export async function balancesInit(
  app: ElmApp<Ports>,
  whitelist: WhiteList,
  provider: Provider,
  address: string
) {
  const balances: SdkBalancesMsg[] = [];
  const allowances: SdkAllowancesMsg[] = [];

  for (const [tokenAddress, token] of whitelist.tokenEntries()) {
    if (token instanceof ERC20Token) {
      const connectedToken = token.connect(provider);

      const balance = await connectedToken.balanceOf(address);
      balances.push({ token: tokenAddress, balance: balance.value.toString() });

      const allowance = await connectedToken.allowance(
        address,
        whitelist.convenience
      );
      allowances.push({
        erc20: tokenAddress,
        allowance: allowance.value.toString(),
      });

      const contract = connectedToken.contract() as IERC20;

      const allowanceFilter = contract.filters.Approval(
        address,
        whitelist.convenience
      );

      updateErc20Balance(contract, address, async () => {
        const balance = await connectedToken.balanceOf(address);
        app.ports.sdkBalancesMsg.send([
          {
            token: tokenAddress,
            balance: balance.value.toString(),
          },
        ]);
      });

      contract.removeAllListeners(allowanceFilter);
      contract.on(allowanceFilter, (_owner, _spender, value) => {
        app.ports.sdkAllowancesMsg.send([
          {
            erc20: tokenAddress,
            allowance: value.toString(),
          },
        ]);
      });
    } else if (token instanceof NativeToken) {
      const balance = await token.connect(provider).getBalance(address);
      balances.push({ token: tokenAddress, balance: balance.value.toString() });

      provider.removeAllListeners("block");
      provider.on("block", async () => {
        const balance = await token.connect(provider).getBalance(address);
        app.ports.sdkBalancesMsg.send([
          { token: tokenAddress, balance: balance.value.toString() },
        ]);
      });
    }
  }

  app.ports.sdkBalancesMsg.send(balances);
  app.ports.sdkAllowancesMsg.send(allowances);
}
