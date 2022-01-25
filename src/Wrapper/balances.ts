import { ERC20Token, NativeToken } from "@timeswap-labs/timeswap-v1-sdk";
import { CONVENIENCE } from "@timeswap-labs/timeswap-v1-sdk/src/constants/index"
import { Uint256 } from "@timeswap-labs/timeswap-v1-sdk-core";
import { updateErc20Balance } from "./helper";
import erc20Abi from "./abi/erc20";
import { Contract } from "@ethersproject/contracts";
import { GlobalParams } from "./global";
import { BalancesOf, Ports } from "./declaration";

export async function balancesInit(
  app: ElmApp<Ports>,
  gp: GlobalParams,
  balancesOf: BalancesOf
) {
  const balancesToken: (ERC20Token | NativeToken)[] = [];
  const balances: Promise<Uint256>[] = [];
  const allowancesToken: ERC20Token[] = [];
  const allowances: Promise<Uint256>[] = [];

  console.log("baalance", balancesOf);

  for (const token of balancesOf.tokens) {
    if (token && (token as ERC20Token).address) {
      const connectedToken : ERC20Token = new ERC20Token(
        gp.metamaskProviderMulti,
        balancesOf.chain.chainId,
        token.decimals,
        (token as ERC20Token).address
      );

      balancesToken.push(token as ERC20Token);
      balances.push(connectedToken.balanceOf(balancesOf.address));

      allowancesToken.push(token as ERC20Token);
      allowances.push(connectedToken.allowance(balancesOf.address, CONVENIENCE[balancesOf.chain.chainId]));

      const contract = new Contract(
        connectedToken.address,
        erc20Abi,
        gp.metamaskProvider
      );

      const allowanceFilter = contract.filters.Approval(
        balancesOf.address,
        CONVENIENCE[balancesOf.chain.chainId]
      );

      updateErc20Balance(contract, balancesOf.address, async () => {
        const balance = await contract.balanceOf(balancesOf.address);
        app.ports.receiveBalances.send(
          { chain: balancesOf.chain,
            address: balancesOf.address,
            tokens: balancesOf.tokens,
            balances: [balance.toString()],
          },
        );
      });

      contract.on(allowanceFilter, (_owner, _spender, value) => {
        app.ports.receiveAllowances.send(
          { chain: balancesOf.chain,
            address: balancesOf.address,
            erc20s: balancesOf.tokens as ERC20Token[],
            allowances: [value.toString()],
          },
        );
      });
    } else if (token instanceof NativeToken) {
      balancesToken.push(token);
      balances.push(
        token.connect(gp.metamaskProviderMulti).getBalance(balancesOf.address)
      );

      gp.metamaskProvider.on("block", async () => {
        const balance = await token
          .connect(gp.metamaskProviderMulti)
          .getBalance(balancesOf.address);
        app.ports.receiveBalances.send(
          { chain: balancesOf.chain,
            address: balancesOf.address,
            tokens: balancesOf.tokens,
            balances: [balance.toString()]
          },
        );
      });
    }
  }

  console.log("bef prom all", balances, allowances);

  const balancesResult = await Promise.all(balances);
  const allowancesResult = await Promise.all(allowances);

  app.ports.receiveBalances.send({
    chain: balancesOf.chain,
    address: balancesOf.address,
    tokens: balancesOf.tokens,
    balances: balancesToken.map((token, index) => balancesResult[index].toString())
  });

  app.ports.receiveAllowances.send({
    chain: balancesOf.chain,
    address: balancesOf.address,
    erc20s: allowancesToken,
    allowances: allowancesToken.map((token, index) => allowancesResult[index].toString())
  });
}

// export async function getBalances(
//   app: ElmApp<Ports>,
//   gp: GlobalParams,
//   userAddress: string,
//   tokens: (ERC20Token | NativeToken)[],
// ) {
//   const balancesToken: string[] = [];
//   const balances: Promise<Uint256>[] = [];

//   for (const token of tokens) {
//     if (token instanceof ERC20Token) {
//       const connectedToken = token.connect(gp.metamaskProviderMulti);

//       balancesToken.push(token.address);
//       balances.push(connectedToken.balanceOf(userAddress));

//       const contract = new Contract(
//         connectedToken.address,
//         erc20Abi,
//         gp.metamaskProvider
//       );

//       updateErc20Balance(contract, userAddress, async () => {
//         const balance = await contract.balanceOf(userAddress);
//         app.ports.receiveBalances.send(
//           { chain: balancesOf.chain,
//             address: balancesOf.address,
//             tokens: balancesOf.tokens,
//             balances: [balance.toString()],
//           },
//         );
//       });
//     } else if (token instanceof NativeToken) {
//       balancesToken.push(tokenAddress);
//       balances.push(
//         token.connect(gp.metamaskProviderMulti).getBalance(userAddress)
//       );

//       gp.metamaskProvider.on("block", async () => {
//         const balance = await token
//           .connect(gp.metamaskProviderMulti)
//           .getBalance(userAddress);
//         app.ports.receiveBalances.send([
//           { token: tokenAddress, balance: balance.toString() },
//         ]);
//       });
//     }
//   }

//   const balancesResult = await Promise.all(balances);
// }
