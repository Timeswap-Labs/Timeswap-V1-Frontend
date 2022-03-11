import { getCustomTokens } from "./helper";

export const whitelistChains: Whitelist = {
  default: {
    chainId: 4,
    name: "Rinkeby",
    rpcUrl: "https://rinkeby.infura.io/v3/",
    blockExplorerUrl: "https://rinkeby.etherscan.io",
    nftExplorerUrl: "https://testnets.opensea.io/assets/rinkeby",
    native: {
      name: "Ether",
      symbol: "ETH",
      decimals: 18,
    },
    whitelist: [
      {
        address: "0x7d0690ac7112148E6084909b19CcFD125d55d5C9",
        name: "TS Dai Stablecoin",
        symbol: "TS-DAI",
        decimals: 18,
      },
      {
        address: "0xAFB3BE940C1499750d1292D28820c7E479C9Dbd0",
        name: "TS Ethereum",
        symbol: "TS-ETH",
        decimals: 18,
      },
      {
        address: "0x408A2da72B14cA97e824bAAfD9c0ffBa63d6cdcd",
        name: "TS Matic",
        symbol: "TS-MATIC",
        decimals: 18,
      },
      {
        address: "0x3C067c8f7ecb674c6329EAcb0B155b6c65A8e90C",
        name: "TS Shiba Inu",
        symbol: "TS-SHIB",
        decimals: 18,
      },
      {
        address: "0xDB6433fd6b05991Bc1EC2BDb8c9b1AdD577F37DA",
        name: "TS Dogecoin",
        symbol: "TS-DOGE",
        decimals: 18,
      }
    ],
    custom: getCustomTokens(String(0x4)),
  },
  others: [
    {
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
