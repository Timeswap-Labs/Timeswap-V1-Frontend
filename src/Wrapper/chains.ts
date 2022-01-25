import { NativeToken, ERC20Token } from './declaration';
import { getCustomTokens } from "./helper";

export const whitelistChains = {
  default: {
    chainId: 0x4,
    name: "Rinkeby",
    etherscan: "https://rinkeby.etherscan.io",
    native: {
      name: "Ether",
      symbol: "ETH",
      decimals: 18,
    },
    whitelist: [
      {
        address: "0x559a70084eb180d649080048a7e966602eacea9e",
        name: "DAI TEST TOKEN",
        symbol: "DAI",
        decimals: 18,
      },
      {
        address: "0x521c1c37fdd245c313457fcbc6d5ed156647c2f3",
        name: "Matic TEST TOKEN",
        symbol: "MATIC",
        decimals: 18,
      }
    ],
    custom: [
      {
        address: "0xa0b86991c6218b36c1d19d4a2e9eb0ce3606eb48",
        name: "USDC TEST TOKEN with a long name",
        symbol: "USDC",
        decimals: 18
      },
      {
        address: "0xdac17f958d2ee523a2206206994597c13d831ec7",
        name: "USDT",
        symbol: "USDT",
        decimals: 18,
      },
      {
        address: "0xdac17f958d2ee523a2206206994597c13d831ff9",
        name: "DDDT",
        symbol: "DDDT",
        decimals: 18,
      }
    ]
  },
  others: [
    {
      chainId: 0x3,
      name: "Ropsten",
      etherscan: "https://ropsten.etherscan.io",
      native: {
        name: "Ether",
        symbol: "ETH",
        decimals: 18,
      },
      whitelist : [],
      custom: getCustomTokens(String(0x3)),
    }
  ],
}

export function getTokenList(chainId: number): (NativeToken | ERC20Token)[] {
  const tokenList: (NativeToken | ERC20Token)[] = [];
  let currentChain;

  if (whitelistChains.default.chainId === chainId) {
    currentChain = whitelistChains.default
  } else {
    whitelistChains.others.forEach(whitelistChain => {
      if (whitelistChain.chainId === chainId) {
        currentChain = whitelistChain;
      }
    });
  }

  if (currentChain) {
    tokenList.push(currentChain.native as NativeToken);

    currentChain.whitelist.forEach(token => {
      tokenList.push(token as ERC20Token);
    });

    currentChain.custom.forEach(token => {
      tokenList.push(token as ERC20Token);
    });
  }

  return tokenList;
}
