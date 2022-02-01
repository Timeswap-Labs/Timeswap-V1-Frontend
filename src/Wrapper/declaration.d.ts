declare module "*.svg" {}

declare const ethereum;

declare module "*.elm" {
  const Elm: ElmInstance<Ports>;
}

declare interface Ports {
  connect: PortFromElm<string>;
  receiveUser: PortToElm<ReceiveUser | null>;
  receiveNoConnect: PortToElm<string>;
  noMetamask: PortToElm;
  disconnect: PortFromElm;

  sdkPoolsMsg: PortToElm<SdkPoolsMsg[]>;

  balancesOf: PortFromElm<BalancesOf>;
  allowancesOf: PortFromElm<AllowancesOf>;
  positionsOf: PortFromElm<PositionsOf>;

  receiveBalances: PortToElm<ReceiveBalances>;
  receiveAllowances: PortToElm<ReceiveAllowances>;
  receivePositions: PortToElm<ReceivePositions>;

  queryLend: PortFromElm<LendQuery>;
  queryLendPerSecond: PortFromElm<LendQuery>;
  lend: PortFromElm<Lend>;
  receiveLendAnswer: PortToElm<LendCalculate>;
  querySum: PortFromElm<ClaimsSum>;
  receiveSum: PortToElm<ReceiveSum>;
  queryClaim: PortFromElm<ClaimsReturn>;
  receiveReturn: PortToElm<ReceiveReturn>;

  queryBorrow: PortFromElm<BorrowQuery>;
  queryBorrowPerSecond: PortFromElm<BorrowQuery>;
  borrow: PortFromElm<Borrow>;
  receiveBorrowAnswer: PortToElm<BorrowCalculate>;
  queryFull: PortFromElm<QueryFull>;
  receiveFull: PortToElm<ReceiveFull>;
  queryCustom: PortFromElm<QueryCustom>;
  receiveCustom: PortToElm<ReceiveCustom>;

  queryPay: PortFromElm<PayQuery>;
  pay: PortFromElm<Pay>;
  sdkPayMsg: PortToElm<PayCalculate>;

  approve: PortFromElm<Approve>;

  withdraw: PortFromElm<Withdraw>;

  faucetMint: PortFromElm<Faucet>;

  scroll: PortsToElm;

  copyToClipboard: PortFromElm<string>;

  cacheSlippage: PortFromElm<number>;
  cacheDeadline: PortFromElm<number>;
  cachePriceFeed: PortFromElm<string>;
  cacheChosenZone: PortFromElm<string>;
  cacheTheme: PortFromElm<string>;
  cacheCustom: PortFromElm<{ string: ERC20Token }>;
  cacheTxns: PortFromElm<ReceiveUser>;

  changeChain: PortFromElm<Chain>;
}

interface NativeToken {
  name: string;
  symbol: string;
  decimals: number;
}

interface ERC20Token {
  address: string;
  name: string;
  symbol: string;
  decimals: number;
}

interface BalancesOf {
  chain: Chain;
  address: string;
  tokens: (NativeToken | ERC20Token)[];
}

interface ReceiveBalances {
  chain: Chain;
  address: string;
  tokens: (NativeToken | ERC20Token)[];
  balances: string[];
}

interface AllowancesOf {
  chain: Chain;
  address: string;
  erc20s: ERC20Token[];
}

interface ReceiveAllowances {
  chain: Chain;
  address: string;
  erc20s: ERC20Token[];
  allowances: string[];
}

interface PositionsOf {
  chain: number;
  owner: string;
  natives: {
    pool: Pool;
    natives: {
      bondPrincipal: string;
      bondInterest: string;
      insurancePrincipal: string;
      insuranceInterest: string;
      liquidity: string;
      collateralizedDebt: string;
    };
  }[];
}

interface ReceivePositions {
  chain: number;
  owner: string;
  positions: {
    claims: Claims;
    dues: Dues;
    liqs: Liqs;
  };
}

interface Claim {
  bondPrincipal: Uint;
  bondInterest: Uint;
  insurancePrincipal: Uint;
  insuranceInterest: Uint;
}

type Claims = {
  pool: Pool;
  claim: Claim;
}[];

type ClaimsSum = {
  chain: Chain,
  pool: Pool;
  claim: Claim;
};

type ReceiveSum = {
  chain: Chain;
  pool: Pool;
  claim: Claim;
  result: {
    asset: string;
    collateral: string;
  };
};

type ClaimsReturn = {
  chain: Chain;
  pool: Pool;
  poolInfo: PoolInfo;
  claimsIn: Claim;
};

type ReceiveReturn = {
  chain: Chain;
  pool: Pool;
  poolInfo: PoolInfo;
  claimsIn: Claim;
  result: {
    asset: string;
    collateral: string;
  } | string;
};

type Dues = {
  pool: Pool;
  dues: {
    tokenId: Uint;
    due: {
      debt: Uint;
      collateral: Uint;
    };
  }[];
}[];

type QueryFull = {
  chain: Chain,
  pool: Pool;
  dues: {
    tokenId: Uint;
    due: {
      debt: Uint;
      collateral: Uint;
    };
  }[];
};

type ReceiveFull = {
  chain: Chain;
  pool: Pool;
  dues: {
    tokenId: Uint;
    due: {
      debt: Uint;
      collateral: Uint;
    };
  }[];
  result: {
    assetIn: Uint;
    collateralOut: Uint;
  } | string;
};

type QueryCustom = {
  chain: Chain,
  pool: Pool;
  dues: {
    tokenId: Uint;
    due: {
      debt: Uint;
      collateral: Uint;
    };
  }[];
  assetsIn: {
    tokenId: Uint;
    assetIn: Uint;
  }[];
};

type ReceiveCustom = {
  chain: Chain,
  pool: Pool;
  dues: {
    tokenId: Uint;
    due: {
      debt: Uint;
      collateral: Uint;
    };
  }[];
  assetsIn: {
    tokenId: Uint;
    assetIn: Uint;
  }[];
  result: {
    collateralsOut: {
      tokenId: Uint;
      collateralOut: Uint;
    }[];
    total?: {
      assetIn: Uint;
      collateralOut: Uint;
    } | string
  };
};

type Liqs = {
  pool: Pool;
  liq: Uint;
}[];

interface ReceiveUser {
  chainId: int;
  wallet: string;
  address: string;
  txns: Txns;
}

interface Txns {
  confirmed: Confirmed[];
  uncomfirmed: Uncomfirmed[];
}

interface Confirmed {
  id: number;
  hash: string;
  write: string;
  state: string;
}

interface Uncomfirmed {
  id: number;
  write: string;
}

type Uint = string;

interface Chain {
  chainId: number;
  name: string;
  etherscan: string;
}

interface CDP {
  ratio: Uint;
  percent: number | null;
}

interface Pool {
  asset: NativeToken | ERC20Token;
  collateral: NativeToken | ERC20Token;
  maturity: number | string;
}

interface PoolInfo {
  x: Uint;
  y: Uint;
  z: Uint;
  assetReserve: Uint;
  collateralReserve: Uint;
  totalLiquidity: Uint;
  totalBondInterest: Uint;
  totalBondPrincipal: Uint;
  totalInsuranceInterest: Uint;
  totalInsurancePrincipal: Uint;
  totalDebtCreated: Uint;
  assetSpot: number | null;
  collateralSpot: number | null;
  fee: number;
  protocolFee: number;
}

interface Approve {
  id: number;
  chain: Chain;
  address: string;
  erc20: ERC20Token;
}

interface LendQuery {
  assetIn: string;
  pool: Pool;
  poolInfo: PoolInfo;
  chain: Chain;
  bondOut?: string;
  insuranceOut?: string;
  percent?: number;
  slippage: number;
}

interface BorrowQuery {
  assetOut: string;
  pool: Pool;
  poolInfo: PoolInfo;
  chain: Chain;
  debtIn?: string;
  collateralIn?: string;
  percent?: number;
  slippage: number;
}

interface Lend {
  id: number;
  chain: Chain;
  address: string;
  send: {
    asset: NativeToken | ERC20Token;
    collateral: NativeToken | ERC20Token;
    maturity: number | string;
    bondTo: string;
    insuranceTo: string;
    assetIn: string;
    bondOut?: string;
    insuranceOut?: string;
    percent?: number;
    minBond?: string;
    minInsurance?: string;
    deadline: number;
  }
}


interface Borrow {
  id: number;
  chain: Chain;
  address: string;
  send: {
    asset: NativeToken | ERC20Token;
    collateral: NativeToken | ERC20Token;
    maturity: number | string;
    assetTo: string;
    dueTo: string;
    assetOut: string;
    debtIn?: string;
    collateralIn?: string;
    percent?: number;
    maxDebt?: string;
    maxCollateral?: string;
    deadline: number;
  }
}

// type BorrowCalculate =
//   | BorrowGivenPercent
//   | BorrowGivenDebt
//   | BorrowGivenCollateral;

// interface BorrowGivenPercent {
//   asset: string;
//   collateral: string;
//   maturity: number;
//   assetOut: string;
//   percent: number;
//   result:
//     | {
//         debtIn: string;
//         collateralIn: string;
//         maxDebt: string;
//         maxCollateral: string;
//         apr: number;
//         cf: string;
//       }
//     | number;
// }

// interface BorrowGivenDebt {
//   asset: string;
//   collateral: string;
//   maturity: number;
//   assetOut: string;
//   debtIn: string;
//   result:
//     | {
//         percent: number;
//         collateralIn: string;
//         maxCollateral: string;
//         apr: number;
//         cf: string;
//       }
//     | number;
// }

// interface BorrowGivenCollateral {
//   asset: string;
//   collateral: string;
//   maturity: number;
//   assetOut: string;
//   collateralIn: string;
//   result:
//     | {
//         percent: number;
//         debtIn: string;
//         maxDebt: string;
//         apr: number;
//         cf: string;
//       }
//     | number;
// }

// interface PayQuery {
//   asset: string;
//   collateral: string;
//   maturity: number;
//   dues: { id: string; debt: string; collateral: string }[];
// }

// interface Pay {
//   asset: string;
//   collateral: string;
//   maturity: number;
//   collateralTo: string;
//   ids: string[];
//   maxAssetsIn: string[];
//   deadline: number;
// }

// interface PayCalculate {
//   asset: string;
//   collateral: string;
//   maturity: number;
//   dues: { id: string; debt: string; collateral: string }[];
//   assetIn: string;
//   collateralOut: string;
// }

// interface Withdraw {
//   asset: string;
//   collateral: string;
//   maturity: number;
//   assetTo: string;
//   collateralTo: string;
//   claimsIn: { bond: string; insurance: string };
// }

// interface Faucet {
//   erc20: string;
// }
