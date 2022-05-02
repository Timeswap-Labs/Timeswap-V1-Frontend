import { getCustomTokens } from "./helper";
import { WNATIVE_ADDRESS } from "@timeswap-labs/timeswap-v1-sdk";

export const whitelistChains: Whitelist = {
  default: {
    chainId: 137,
    name: "Polygon Mainnet",
    rpcUrl: "https://polygon-rpc.com/",
    blockExplorerUrl: "https://polygonscan.com/",
    nftExplorerUrl: "https://opensea.io/assets/matic",
    native: {
      name: "MATIC",
      symbol: "MATIC",
      decimals: 18,
    },
    wrapper: {
      address: WNATIVE_ADDRESS[137],
      name: "Wrapped Matic",
      symbol: "WMATIC",
      decimals: 18,
    },
    whitelist: [
      {
        address: "0x2791Bca1f2de4661ED88A30C99A7a9449Aa84174",
        name: "USDC",
        symbol: "USDC",
        decimals: 6,
      }
    ],
    custom: getCustomTokens(String(137)),
  },
  others: [{
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
    wrapper: {
      address: WNATIVE_ADDRESS[80001],
      name: "Wrapped Ether",
      symbol: "WETH",
      decimals: 18,
    },
    whitelist: [
      {
        address: "0xA4abf1B77d9171Eb910DD6f7ae863cF77c4225A4",
        name: "Dai Stablecoin",
        symbol: "DAI",
        decimals: 18,
      },
      {
        address: "0x2193fBf6F024aD9DCa5c7D615259CE4DDb8a9F89",
        name: "Ethereum",
        symbol: "ETH",
        decimals: 18,
      }
    ],
    custom: getCustomTokens(String(80001)),
  }]
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

export function getNativeToken(chainId: number): NativeToken | null {
  let chain = getChainData(chainId);

  if (!chain) {
    return null;
  }

  return chain.native;
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
  wrapper: ERC20Token;
  whitelist: ERC20Token[];
  custom: ERC20Token[];
}
