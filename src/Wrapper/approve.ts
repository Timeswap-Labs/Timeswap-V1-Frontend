import { Uint256 } from "@timeswap-labs/timeswap-v1-biconomy-sdk";
import { CONVENIENCE, ERC20Token } from "@timeswap-labs/timeswap-v1-biconomy-sdk";

import { GlobalParams } from './global';
import { handleTxnErrors } from "./helper";

export function approveSigner(
  app: ElmApp<Ports>,
  gp: GlobalParams
) {
  app.ports.approve.subscribe(async (params) => {
    const erc20 = new ERC20Token(
      gp.biconomyProvider,
      params.chain.chainId,
      params.erc20.decimals,
      params.erc20.address
    );

    try {
      const txnConfirmation = await erc20
        .upgrade(await gp.getSigner())
        .approve(CONVENIENCE[137], new Uint256((1n << 256n) - 1n));

      app.ports.receiveConfirm.send({
        id: params.id,
        chain: params.chain,
        address: params.address,
        hash: txnConfirmation ? txnConfirmation.hash : null
      });

      const txnReceipt = await txnConfirmation.wait();
      const receiveReceipt = {
        chain: params.chain,
        address: params.address,
        hash: txnConfirmation.hash,
        state: txnReceipt.status ? "success" : "failed"
      }
      app.ports.receiveReceipt.send(receiveReceipt);
    } catch (error) {
      handleTxnErrors(error, app, gp, params);
    }
  });
}
