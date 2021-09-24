import "regenerator-runtime/runtime";
import { BaseProvider } from "@ethersproject/providers";
import { ERC20Token } from "@timeswap-labs/timeswap-v1-sdk";
import { WhiteList } from "./whitelist";
import { NativeToken } from "@timeswap-labs/timeswap-v1-sdk-core";
import { updateErc20Balance } from "./helper";
import { Contract as MultiCallContract, Provider } from "ethcall";
import erc20 from "./abi/erc20";
import { FormatTypes, Interface } from "@ethersproject/abi";
import { Contract } from "@ethersproject/contracts";

const erc20Abi = JSON.parse(
  new Interface(erc20).format(FormatTypes.json) as string
);

export async function balancesInit(
  app: ElmApp<Ports>,
  whitelist: WhiteList,
  provider: BaseProvider,
  address: string
) {
  const multiCallProvider = new Provider();
  multiCallProvider.init(provider);

  const balancesToken: string[] = [];
  const balances: any[] = [];
  const allowances: SdkAllowancesMsg[] = [];

  for (const [tokenAddress, token] of whitelist.tokenEntries()) {
    if (token instanceof ERC20Token) {
      const connectedToken = token.connect(provider);
      const multiCallToken = new MultiCallContract(token.address, erc20Abi);

      balancesToken.push(tokenAddress);
      balances.push(multiCallToken.balanceOf(address));

      const allowance = await connectedToken.allowance(
        address,
        whitelist.convenience
      );
      allowances.push({
        erc20: tokenAddress,
        allowance: allowance.value.toString(),
      });

      const contract = new Contract(connectedToken.address, erc20, provider);

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
      balancesToken.push(tokenAddress);
      balances.push(multiCallProvider.getEthBalance(address));

      provider.removeAllListeners("block");
      provider.on("block", async () => {
        const balance = await token.connect(provider).getBalance(address);
        app.ports.sdkBalancesMsg.send([
          { token: tokenAddress, balance: balance.value.toString() },
        ]);
      });
    }
  }

  const balancesResult = await multiCallProvider.tryAll(balances);

  app.ports.sdkBalancesMsg.send(
    balancesToken.map((tokenAddress, index) => {
      return {
        token: tokenAddress,
        balance: balancesResult[index].toString(),
      };
    })
  );
  app.ports.sdkAllowancesMsg.send(allowances);
}
