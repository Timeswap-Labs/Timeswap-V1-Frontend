import { getCustomTokens } from "./helper";

export const whitelistChains: Whitelist = {
  default: {
    chainId: 80001,
    name: "Polygon Testnet",
    rpcUrl: "https://rpc-mumbai.matic.today",
    blockExplorerUrl: "https://mumbai.polygonscan.com",
    nftExplorerUrl: "https://testnets.opensea.io/assets/mumbai",
    native: {
      name: "MATIC",
      symbol: "MATIC",
      decimals: 18,
    },
    whitelist: [
      {
        address: "0x1a82c228450c2c8694a208de6ed2767f7aff7acc",
        name: "Dai Stablecoin",
        symbol: "DAI",
        decimals: 18,
      },
      {
        address: "0x008e7c4fb5fe4a897e7903564f085318e3e76987",
        name: "Ethereum",
        symbol: "ETH",
        decimals: 18,
      }
    ],
    custom: getCustomTokens(String(80001)),
  },
  others: [],
};

export function getChainData(chainId: number) {
  let chain = null;

  if (whitelistChains.default.chainId === chainId) {
    chain = whitelistChains.default;
  } else {
    whitelistChains.others.forEach((whitelistChain) => {
      if (whitelistChain.chainId === chainId) {
        chain = whitelistChain;
      }
    });
  }

  return chain;
}

export function getTokenList(chainId: number): (NativeToken | ERC20Token)[] {
  const tokenList: (NativeToken | ERC20Token)[] = [];
  let currentChain;

  if (whitelistChains.default.chainId === chainId) {
    currentChain = whitelistChains.default;
  } else {
    whitelistChains.others.forEach((whitelistChain) => {
      if (whitelistChain.chainId === chainId) {
        currentChain = whitelistChain;
      }
    });
  }

  if (currentChain) {
    tokenList.push(currentChain.native as NativeToken);

    currentChain.whitelist.forEach((token) => {
      tokenList.push(token as ERC20Token);
    });

    currentChain.custom.forEach((token) => {
      tokenList.push(token as ERC20Token);
    });
  }

  return tokenList;
}

interface Whitelist {
  default: WhitelistChain;
  others: WhitelistChain[];
}

interface WhitelistChain {
  chainId: number;
  name: string;
  rpcUrl: string;
  blockExplorerUrl: string;
  nftExplorerUrl: string;
  native: NativeToken;
  whitelist: ERC20Token[];
  custom: ERC20Token[];
}
