import { Provider } from "@ethersproject/providers";
import { ERC20Token } from "@timeswap-labs/timeswap-v1-sdk";
import type { IERC20 } from "./typechain";
import { WhiteList } from "./whitelist";

export function faucet(
  app: ElmApp<Ports>,
  whitelist: WhiteList,
  provider: Provider,
  address: string
) {
  app.ports.faucetMint.subscribe((params) => {
    const token = whitelist.getToken(params.erc20) as ERC20Token;
    const contract = token.connect(provider).contract() as IERC20;
    // contract.mint(address);
  });
}
