import { GlobalParams } from "./global";
import { TestToken__factory } from "./typechain";

export function faucetSigner(app: ElmApp<Ports>, gp: GlobalParams) {
  app.ports.faucetMint.subscribe((params) => {
    const token = TestToken__factory.connect(
      params.erc20,
      gp.metamaskProvider!
    );
    token.connect(gp.metamaskSigner!)["mint()"]();
  });
}
