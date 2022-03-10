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
        address: "0xc17eb97831a5ccf75aee5fa28c2b38a77dc2cf9c",
        name: "TS USDC",
        symbol: "TS-USDC",
        decimals: 18,
      },
      {
        address: "0x9355c1d6b16a1185881ca193abf19d5c193080b9",
        name: "TS Ethereum",
        symbol: "TS-ETH",
        decimals: 18,
      },
      {
        address: "0xcb4faf1866e4b20d95b51abbf6c8fffdc355f8a2",
        name: "TS Matic",
        symbol: "TS-MATIC",
        decimals: 18,
      },
      {
        address: "0xea3b4b0582e759d09efc4e21b081e82a15cefed9",
        name: "TS Avalanche",
        symbol: "TS-AVAX",
        decimals: 18,
      },
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
