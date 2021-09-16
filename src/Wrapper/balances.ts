import "regenerator-runtime/runtime";
import { Provider } from "@ethersproject/providers";
import { ERC20Token } from "@timeswap-labs/timeswap-v1-sdk";
import { WhiteList } from "./whitelist";
import { NativeToken } from "@timeswap-labs/timeswap-v1-sdk-core";
// import type { Erc20 } from "./typechain";

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

      // const contract = connectedToken.contract() as Erc20;

      // const filter = contract.filters.Transfer([address], [address]);
      // contract.on(filter, (from, to, amount, event) => {});
    } else if (token instanceof NativeToken) {
      const balance = await token.connect(provider).getBalance(address);
      balances.push({ token: tokenAddress, balance: balance.value.toString() });
    }
  }

  app.ports.sdkBalancesMsg.send(balances);
  app.ports.sdkAllowancesMsg.send(allowances);
}
