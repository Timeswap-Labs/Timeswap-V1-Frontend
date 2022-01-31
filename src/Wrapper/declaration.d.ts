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
  sdkPositionsMsg: PortToElm<SdkPositionsMsg[]>;

  balancesOf: PortFromElm<BalancesOf>;
  allowancesOf: PortFromElm<AllowancesOf>;
  positionsOf: PortFromElm<PositionsOf>;

  receiveBalances: PortToElm<ReceiveBalances>;
  receiveAllowances: PortToElm<ReceiveAllowances>;
  receivePositions: PortToElm<ReceivePositions>;

  queryLend: PortFromElm<LendQuery>;
  queryLendPerSecond: PortFromElm<LendQuery>;
  approveLend: PortFromElm<Approve>;
  lend: PortFromElm<Lend>;
  receiveLendAnswer: PortToElm<LendCalculate>;

  queryBorrow: PortFromElm<BorrowQuery>;
  queryBorrowPerSecond: PortFromElm<BorrowQuery>;
  approveBorrow: PortFromElm<Approve>;
  borrow: PortFromElm<Borrow>;
  receiveBorrowAnswer: PortToElm<BorrowCalculate>;

  queryPay: PortFromElm<PayQuery>;
  approvePay: PortFromElm<Approve>;
  pay: PortFromElm<Pay>;
  sdkPayMsg: PortToElm<PayCalculate>;

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

type Claims = {
  pool: Pool;
  claim: {
    bondPrincipal: Uint;
    bondInterest: Uint;
    insurancePrincipal: Uint;
    insuranceInterest: Uint;
  };
}[];

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
  totalBond: Uint;
  totalInsurance: Uint;
  totalDebtCreated: Uint;
  assetSpot: number | null;
  collateralSpot: number | null;
  fee: number;
  protocolFee: number;
}

// type SdkPositionsMsg =
//   | SdkPositionsMsg1
//   | SdkPositionsMsg2
//   | SdkPositionsMsg3
//   | SdkPositionsMsg4
//   | SdkPositionsMsg5;

// interface SdkPositionsMsg1 {
//   asset: string;
//   collateral: string;
//   maturity: number;
//   bond: string;
// }

// interface SdkPositionsMsg2 {
//   asset: string;
//   collateral: string;
//   maturity: number;
//   insurance: string;
// }

// interface SdkPositionsMsg3 {
//   asset: string;
//   collateral: string;
//   maturity: number;
//   bond: string;
//   assetOut: string;
// }

// interface SdkPositionsMsg4 {
//   asset: string;
//   collateral: string;
//   maturity: number;
//   insurance: string;
//   collateralOut: string;
// }

// interface SdkPositionsMsg5 {
//   asset: string;
//   collateral: string;
//   maturity: number;
//   dues: {
//     id: string;
//     debt: string;
//     collateral: string;
//   }[];
// }

// interface Approve {
//   erc20: string;
// }

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

// interface Lend {
//   asset: string;
//   collateral: string;
//   maturity: number;
//   bondTo: string;
//   insuranceTo: string;
//   assetIn: string;
//   bondOut?: string;
//   insuranceOut?: string;
//   percent?: number;
//   minBond?: string;
//   minInsurance?: string;
//   deadline: number;
// }

type LendCalculate = LendGivenPercent | LendGivenBond | LendGivenInsurance;

// interface LendGivenPercent {
//   asset: string;
//   collateral: string;
//   maturity: number;
//   assetIn: string;
//   percent: number;
//   result:
//     | {
//         bondOut: string;
//         insuranceOut: string;
//         minBond: string;
//         minInsurance: string;
//         apr: number;
//         cf: string;
//       }
//     | number;
// }

// interface LendGivenBond {
//   asset: string;
//   collateral: string;
//   maturity: number;
//   assetIn: string;
//   bondOut: string;
//   result:
//     | {
//         percent: number;
//         insuranceOut: string;
//         minInsurance: string;
//         apr: number;
//         cf: string;
//       }
//     | number;
// }

// interface Borrow {
//   asset: string;
//   collateral: string;
//   maturity: number;
//   assetTo: string;
//   dueTo: string;
//   assetOut: string;
//   debtIn?: string;
//   collateralIn?: string;
//   percent?: number;
//   maxDebt?: string;
//   maxCollateral?: string;
//   deadline: number;
// }

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
