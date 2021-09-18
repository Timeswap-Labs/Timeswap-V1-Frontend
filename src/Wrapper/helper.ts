import type { IERC20 } from "./typechain";
import { Uint256 } from "@timeswap-labs/timeswap-v1-sdk-core";

export function updateErc20Balance(
  contract: IERC20,
  address: string,
  updateBalance: () => void
) {
  const balancesInFilter = contract.filters.Transfer(null, address);
  const balancesOutFilter = contract.filters.Transfer(address);

  // chain change
  contract.removeAllListeners(balancesInFilter);
  contract.removeAllListeners(balancesOutFilter);

  contract.on(balancesInFilter, updateBalance);
  contract.on(balancesOutFilter, updateBalance);
}

export function getCurrentTime(): Uint256 {
  return new Uint256(Date.now()).div(1000);
}
