import { getCustomTokens } from "./helper";
import { WNATIVE_ADDRESS } from "@timeswap-labs/timeswap-v1-biconomy-sdk";

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
      },
      {
        address: "0x8f3Cf7ad23Cd3CaDbD9735AFf958023239c6A063",
        name: "DAI",
        symbol: "DAI",
        decimals: 18,
      },
      {
        address: "0xa3fa99a148fa48d14ed51d610c367c61876997f1",
        name: "Mai Finance",
        symbol: "MAI",
        decimals: 18,
      },
      {
        address: "0x3A58a54C066FdC0f2D55FC9C89F0415C92eBf3C4",
        name: "Staked MATIC",
        symbol: "STMATIC",
        decimals: 18,
      },
      // {
      //   address: "0x7ceB23fD6bC0adD59E62ac25578270cFf1b9f619",
      //   name: "WETH",
      //   symbol: "WETH",
      //   decimals: 18,
      // },
    ],
    custom: getCustomTokens(String(137)),
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

// Transform pool token-data from server to the data provided in whitelist
export function transformPool(pool: Pool, chain: Chain): Pool {
  const tokenList: (NativeToken | ERC20Token)[] = getTokenList(chain.chainId);
  let asset = pool.asset;
  let collateral = pool.collateral;

  if ((pool.asset as ERC20Token).address) {
    asset = tokenList.find(token =>
      (token as ERC20Token).address?.toLowerCase() === (pool.asset as ERC20Token).address?.toLowerCase()
    )
    || pool.asset;
  }

  if ((pool.collateral as ERC20Token).address) {
    collateral = tokenList.find(token =>
      (token as ERC20Token).address?.toLowerCase() === (pool.collateral as ERC20Token).address?.toLowerCase()
    )
    || pool.collateral;
  }

  const transformedPool: Pool = {
    asset,
    collateral,
    maturity: pool.maturity
  }

  return transformedPool;
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
