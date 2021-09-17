import { Provider } from "@ethersproject/providers";
import { WhiteList } from "./whitelist";

export async function positionsInit(
  app: ElmApp<Ports>,
  whitelist: WhiteList,
  provider: Provider,
  address: string
) {
    for (const x of whitelist.poolEntries())
}
