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
  approveAndLend: PortFromElm<Lend>;
  approveAndBorrow: PortFromElm<Borrow>;

  withdraw: PortFromElm<Withdraw>;

  queryLiquidity: PortFromElm<LiquidityQuery>;
  queryLiquidityPerSecond: PortFromElm<LiquidityQuery>;
  receiveAddLiqAnswer: PortToElm<LiquidityCalculate>;
  liquidity: PortFromElm<Liquidity>;
  queryLiq: PortFromElm<LiqReturn>;
  receiveLiqReturn: PortToElm<ReceiveLiqReturn>;

  burn: PortFromElm<Burn>;
  approveAndFlashRepay: PortFromElm<ApproveAndFlashRepay>;
  flashRepay: PortFromElm<FlashRepay>;

  queryCreate: PortFromElm<NewLiquidityQuery>;
  receiveNewLiqAnswer: PortToElm<NewLiquidityCalculate>;
  create: PortFromElm<NewLiquidity>;

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

  receiveConfirm: PortToElm<ReceiveConfirm>;
  receiveReceipt: PortToElm<ReceiveReceipt>;
  receiveUpdatedTxns: PortToElm<ReceiveUpdatedTxns>;
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

interface Natives {
  bondPrincipal: string;
  bondInterest: string;
  insurancePrincipal: string;
  insuranceInterest: string;
  liquidity: string;
  collateralizedDebt: string;
}

interface ConvNatives {
  convAddress: string;
  nativeResponse: {
    pool: Pool;
    natives: Natives;
  }[];
}

interface PositionsOf {
  chain: Chain;
  owner: string;
  allNatives: ConvNatives[];
}

interface ReceivePositions {
  chain: Chain;
  owner: string;
  allNatives: ConvNatives[];
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
  convAddress: string;
  pools: {
    pool: Pool;
    claim: Claim;
  }[]
}[];

type ClaimsSum = {
  chain: Chain;
  pool: Pool;
  claims: Claim;
};

type ReceiveSum = {
  chain: Chain;
  pool: Pool;
  claims: Claim;
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
  result:
    | {
        asset: string;
        collateral: string;
      }
    | string;
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
  chain: Chain;
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
  result:
    | {
        assetIn: Uint;
        collateralOut: Uint;
      }
    | string;
};

type QueryCustom = {
  chain: Chain;
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
  chain: Chain;
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
    total?:
      | {
          assetIn: Uint;
          collateralOut: Uint;
        }
      | string;
  };
};

type Liqs = {
  convAddress: string;
  pools: {
    pool: Pool;
    liq: Uint;
  }[]
}[];

type LiqReturn = {
  chain: Chain;
  pool: Pool;
  poolInfo: PoolInfo;
  liquidityIn: Uint;
  tokenIds: string[];
  cdtAddress: string | null;
};

type ReceiveLiqReturn = {
  chain: Chain;
  pool: Pool;
  poolInfo: PoolInfo;
  result:
    | {
        asset: string;
        collateral: string;
      }
    | {
        liqPercent: number;
        isFlashRepayAllowed: boolean;
        isCDTApproved: boolean;
      };
};

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
  write: Write;
  state: string;
}

interface Uncomfirmed {
  id: number;
  write: Write;
}

interface Write {
  txn: string;
  pool: Pool
}

type Uint = string;

interface Chain {
  chainId: number;
  name: string;
  rpcUrl: string;
  blockExplorerUrl: string;
  nftExplorerUrl: string;
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
  feeStored: Uint;
  protocolFee: number;
  convAddress: string;
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
  };
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
  };
}

interface Withdraw {
  id: number;
  chain: Chain;
  address: string;
  send: {
    asset: NativeToken | ERC20Token;
    collateral: NativeToken | ERC20Token;
    maturity: number | string;
    convAddress: string;
    assetTo: string;
    collateralTo: string;
    claimsIn: Claim;
  };
}

interface Pay {
  id: number;
  chain: Chain;
  address: string;
  send: {
    asset: NativeToken | ERC20Token;
    collateral: NativeToken | ERC20Token;
    maturity: number | string;
    collateralTo: string;
    ids: string[];
    maxAssetsIn: string[];
    deadline: number;
  };
}
interface Liquidity {
  id: number;
  chain: Chain;
  address: string;
  send: {
    asset: NativeToken | ERC20Token;
    collateral: NativeToken | ERC20Token;
    maturity: number | string;
    liquidityTo: string;
    dueTo: string;
    assetIn?: string;
    debtIn?: string;
    collateralIn?: string;
    minLiquidity: string;
    maxAsset?: string;
    maxDebt?: string;
    maxCollateral?: string;
    deadline: number;
  };
}

interface LiquidityQuery {
  chain: Chain;
  pool: Pool;
  poolInfo: PoolInfo;
  slippage: number;
  assetIn?: string;
  debtIn?: string;
  collateralIn?: string;
}

interface NewLiquidityQuery {
  chain: Chain;
  pool: Pool;
  price: {
    assetSpot: number;
    collateralSpot: number;
  };
  assetIn: string;
  debtIn: string;
  collateralIn: string;
}

interface NewLiquidity {
  id: number;
  chain: Chain;
  address: string;
  send: {
    asset: NativeToken | ERC20Token;
    collateral: NativeToken | ERC20Token;
    maturity: number | string;
    liquidityTo: string;
    dueTo: string;
    assetIn?: string;
    debtIn?: string;
    collateralIn?: string;
    deadline: number;
  };
}

interface Burn {
  id: number;
  chain: Chain;
  address: string;
  send: {
    asset: NativeToken | ERC20Token;
    collateral: NativeToken | ERC20Token;
    maturity: number | string;
    convAddress?: string;
    assetTo: string;
    collateralTo: string;
    liquidityIn: string;
  };
}

interface ApproveAndFlashRepay {
  id: number;
  chain: Chain;
  address: string;
  send: {
    asset: NativeToken | ERC20Token;
    collateral: NativeToken | ERC20Token;
    cdtAddress: string;
    maturity: number | string;
    ids: string[];
  };
}

interface FlashRepay {
  id: number;
  chain: Chain;
  address: string;
  send: {
    asset: NativeToken | ERC20Token;
    collateral: NativeToken | ERC20Token;
    maturity: number | string;
    ids: string[];
  };
}

interface ReceiveConfirm {
  id: number;
  chain: Chain;
  address: string;
  hash: string | null;
}

interface ReceiveReceipt {
  chain: Chain;
  address: string;
  hash: string;
  state: string;
  txnType?: Write;
}

interface ReceiveUpdatedTxns {
  chain: Chain;
  address: string;
  txns: Txns;
}


