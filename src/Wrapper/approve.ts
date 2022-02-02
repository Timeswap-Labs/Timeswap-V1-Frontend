import { Uint256 } from "@timeswap-labs/timeswap-v1-sdk-core";
import { CONVENIENCE, ERC20Token } from "@timeswap-labs/timeswap-v1-sdk";

import { GlobalParams } from './global';

export function approveSigner(
  app: ElmApp<Ports>,
  gp: GlobalParams
) {
  app.ports.approve.subscribe(async (approveErc20) => {
    const erc20 = new ERC20Token(
      gp.metamaskProvider,
      approveErc20.chain.chainId,
      approveErc20.erc20.decimals,
      approveErc20.erc20.address
    );

    try {
      const txnConfirmation = await erc20
        .upgrade(gp.metamaskSigner)
        .approve(CONVENIENCE[approveErc20.chain.chainId], new Uint256((1n << 256n) - 1n));

      app.ports.receiveConfirm.send({
        id: approveErc20.id,
        chain: approveErc20.chain,
        address: approveErc20.address,
        hash: txnConfirmation ? txnConfirmation.hash : null
      });
    } catch (error) {
      app.ports.receiveConfirm.send({
        id: approveErc20.id,
        chain: approveErc20.chain,
        address: approveErc20.address,
        hash: null
      });
    }
  });
}
