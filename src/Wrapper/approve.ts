import { Uint256 } from "@timeswap-labs/timeswap-v1-sdk-core";
import { CONVENIENCE, ERC20Token as ERC20TokenSDK } from "@timeswap-labs/timeswap-v1-sdk";
import { Contract } from "@ethersproject/contracts";
import cdTokenAbi from "./abi/cdToken";

import { GlobalParams } from './global';
import { fetchRecentTxns, handleTxnErrors } from "./helper";
import { lendHandler } from "./lend";
import { borrowHandler } from "./borrow";

const flashTSRepayAddress = "0xd88afb2186d1b6974de255fd18a04552d4f87b50";

export function approveSigner(
  app: ElmApp<Ports>,
  gp: GlobalParams
) {
  app.ports.approve.subscribe(async (params) => {
    const erc20 = new ERC20TokenSDK(
      gp.walletProvider,
      params.chain.chainId,
      params.erc20.decimals,
      params.erc20.address
    );
    try {
      const txnConfirmation = await erc20
        .upgrade(gp.walletSigner)
        .approve(CONVENIENCE[params.chain.chainId], new Uint256((1n << 256n) - 1n));

      app.ports.receiveConfirm.send({
        id: params.id,
        chain: params.chain,
        address: params.address,
        hash: txnConfirmation ? txnConfirmation.hash : null
      });

      const txnReceipt = await txnConfirmation.wait();
      const receiveReceipt = {
        id: params.id,
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

  app.ports.approveAndLend.subscribe(async (params) => {
    if ((params.send.asset as ERC20Token).address) {
      const erc20 = new ERC20TokenSDK(
        gp.walletProvider,
        params.chain.chainId,
        params.send.asset.decimals,
        (params.send.asset as ERC20Token).address
      );

      try {
        const txnConfirmation = await erc20
          .upgrade(gp.walletSigner)
          .approve(CONVENIENCE[params.chain.chainId], new Uint256((1n << 256n) - 1n));

        app.ports.receiveConfirm.send({
          id: params.id,
          chain: params.chain,
          address: params.address,
          hash: txnConfirmation ? txnConfirmation.hash : null
        });

        const txnReceipt = await txnConfirmation.wait();
        const pool = {
          asset: params.send.asset,
          collateral: params.send.collateral,
          maturity: params.send.maturity
        }

        const receiveReceipt = {
          id: params.id,
          chain: params.chain,
          address: params.address,
          hash: txnConfirmation.hash,
          state: txnReceipt.status ? "success" : "failed",
          txnType: {
            txn: "approveAndLend",
            pool
          }
        }

        // If approve is successful, initiate Lend txn
        if (txnReceipt.status) {
          const recentTxns = fetchRecentTxns(gp, params.address)
          const newTxnId = recentTxns.confirmed.length + recentTxns.uncomfirmed.length + 1;
          recentTxns.uncomfirmed.push({ id: newTxnId, write: { txn: "lend", pool }});

          app.ports.receiveUpdatedTxns.send({
            chain: params.chain,
            address: params.address,
            txns: recentTxns
          });

          app.ports.receiveReceipt.send(receiveReceipt);
          lendHandler(app, gp, {...params, id: newTxnId});
        } else {
          app.ports.receiveReceipt.send(receiveReceipt);
        }
      } catch (error) {
        handleTxnErrors(error, app, gp, params);
      }
    }
  });

  app.ports.approveAndBorrow.subscribe(async (params) => {
    if ((params.send.collateral as ERC20Token).address) {
      const erc20 = new ERC20TokenSDK(
        gp.walletProvider,
        params.chain.chainId,
        params.send.collateral.decimals,
        (params.send.collateral as ERC20Token).address
      );

      try {
        const txnConfirmation = await erc20
          .upgrade(gp.walletSigner)
          .approve(CONVENIENCE[params.chain.chainId], new Uint256((1n << 256n) - 1n));

        app.ports.receiveConfirm.send({
          id: params.id,
          chain: params.chain,
          address: params.address,
          hash: txnConfirmation ? txnConfirmation.hash : null
        });

        const txnReceipt = await txnConfirmation.wait();
        const pool = {
          asset: params.send.asset,
          collateral: params.send.collateral,
          maturity: params.send.maturity
        }

        const receiveReceipt = {
          id: params.id,
          chain: params.chain,
          address: params.address,
          hash: txnConfirmation.hash,
          state: txnReceipt.status ? "success" : "failed",
          txnType: {
            txn: "approveAndBorrow",
            pool
          }
        }

        console.log("approve success", receiveReceipt);

        // If approve is successful, initiate Borrow txn
        if (txnReceipt.status) {
          const recentTxns = fetchRecentTxns(gp, params.address)
          const newTxnId = recentTxns.confirmed.length + recentTxns.uncomfirmed.length + 1;
          recentTxns.uncomfirmed.push({ id: newTxnId, write: { txn: "borrow", pool }});

          app.ports.receiveUpdatedTxns.send({
            chain: params.chain,
            address: params.address,
            txns: recentTxns
          });

          app.ports.receiveReceipt.send(receiveReceipt);
          borrowHandler(app, gp, {...params, id: newTxnId});
        } else {
          app.ports.receiveReceipt.send(receiveReceipt);
        }
      } catch (error) {
        handleTxnErrors(error, app, gp, params);
      }
    }
  });

  app.ports.approveCDT.subscribe(async (params) => {
    try {
      const cdtContract = new Contract(params.send, cdTokenAbi, gp.walletSigner)
      const txnConfirmation = await cdtContract.setApprovalForAll(flashTSRepayAddress, true);

      app.ports.receiveConfirm.send({
        id: params.id,
        chain: params.chain,
        address: params.address,
        hash: txnConfirmation ? txnConfirmation.hash : null
      });

      const txnReceipt = await txnConfirmation.wait();
      const receiveReceipt = {
        id: params.id,
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
