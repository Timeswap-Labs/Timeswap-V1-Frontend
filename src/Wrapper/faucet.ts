import { Contract } from "@ethersproject/contracts";
import testTokenAbi from "./abi/testToken";
import { GlobalParams } from "./global";

export function faucetSigner(app: ElmApp<Ports>, gp: GlobalParams) {
  app.ports.faucetMint.subscribe((params) => {
    const token = new Contract(
      params.erc20,
      testTokenAbi,
      gp.metamaskProvider!
    );
    token.connect(gp.metamaskSigner!).mint();
  });
}
