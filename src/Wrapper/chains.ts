import { getCustomTokens } from "./helper";

export const whitelistChains = {
  default: {
    chainId: 80001,
    name: "Polygon Testnet",
    rpcUrl: "https://rpc-mumbai.matic.today",
    blockExplorerUrl: "https://mumbai.polygonscan.com",
    native: {
      name: "MATIC",
      symbol: "MATIC",
      decimals: 18,
    },
    whitelist: [
      {
        address: "0xcDA0060b60Ac4204102EA11D0ff8bbFfd690A863",
        name: "TS USDC",
        symbol: "TS-USDC",
        decimals: 18,
      },
      {
        address: "0xce8AdB08e99C0AaB0cA5f3A2661e4394FE6cB6Fa",
        name: "TS Ethereum",
        symbol: "TS-ETH",
        decimals: 18,
      },
      {
        address: "0x9eB6e17790b19Ee29a3557DB74Ef6c85557F99a9",
        name: "TS Matic",
        symbol: "TS-MATIC",
        decimals: 18,
      },
      {
        address: "0x9b2898CB884588E3b64bFBED585b65220871F98a",
        name: "TS Avalanche",
        symbol: "TS-AVAX",
        decimals: 18,
      },
    ],
    custom: getCustomTokens(String(80001)),
  },
  others: [
    {
      chainId: 0x4,
      name: "Rinkeby",
      rpcUrl: "https://rinkeby.infura.io/v3/",
      blockExplorerUrl: "https://rinkeby.etherscan.io",
      native: {
        name: "Ether",
        symbol: "ETH",
        decimals: 18,
      },
      whitelist: [
        {
          address: "0x559a70084eb180d649080048a7e966602eacea9e",
          name: "TS Dai Stablecoin",
          symbol: "TS-DAI",
          decimals: 18,
        },
        {
          address: "0x521c1c37fdd245c313457fcbc6d5ed156647c2f3",
          name: "TS Matic Token",
          symbol: "TS-MATIC",
          decimals: 18,
        },
        {
          address: "0x339a7cce9e5a20833da4f195ad15174c8ba554ad",
          name: "TS Ethereum",
          symbol: "TS-ETH",
          decimals: 18,
        },
      ],
      custom: getCustomTokens(String(0x4)),
    },
  ],
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
