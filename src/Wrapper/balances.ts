import rinkeby from "../../whitelist/rinkeby.json";

import "regenerator-runtime/runtime";
import { Provider } from "@ethersproject/providers";
import { ERC20Token } from "@timeswap-labs/timeswap-v1-sdk";

export async function balancesInit(
  app: ElmApp<Ports>,
  provider: Provider,
  address: string
) {
  const network = await provider.getNetwork();

  const params = await Promise.all(
    rinkeby.erc20s.map(
      async ({ address: tokenAddress, name, symbol, decimals }) => {
        const token = new ERC20Token(
          provider,
          network.chainId,
          decimals,
          tokenAddress,
          symbol,
          name
        );

        const balance = await token.balanceOf(address);
        const allowance = await token.allowance(address, rinkeby.convenience);
        app.ports.sdkAllowancesMsg.send([
          { erc20: tokenAddress, allowance: allowance.value.toString() },
        ]);
        return { token: tokenAddress, balance: balance.value.toString() };
      }
    )
  );

  const ethBalance = await provider.getBalance(address);
  params.push({ token: "ETH", balance: ethBalance.toString() });

  app.ports.sdkBalancesMsg.send(params);
}
