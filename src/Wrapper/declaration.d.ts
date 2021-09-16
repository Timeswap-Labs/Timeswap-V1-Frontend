declare module "*.svg" {}

declare const ethereum;

declare module "*.elm" {
  const Elm: ElmInstance<Ports>;
}

declare interface Ports {
  connectMetamask: PortFromElm;
  metamaskMsg: PortToElm<MetamaskMsg | null>;
  noMetamask: PortToElm;
  disconnect: PortFromElm;

  sdkPoolsMsg: PortToElm<SdkPoolsMsg[]>;
  sdkBalancesMsg: PortToElm<SdkBalancesMsg[]>;
  sdkAllowancesMsg: PortToElm<SdkAllowancesMsg[]>;
  sdkPositionsMsg: PortToElm<SdkPositionsMsg[]>;

  queryLend: PortFromElm<LendQuery>;
  queryLendPerSecond: PortFromElm<LendQuery>;
  approveLend: PortFromElm<Approve>;
  lend: PortFromElm<Lend>;
  sdkLendMsg: PortToElm<LendGivenPercentCalculate>;

  queryBorrow: PortFromElm<BorrowQuery>;
  queryBorrowPerSecond: PortFromElm<BorrowQuery>;
  approveBorrow: PortFromElm<Approve>;
  borrow: PortFromElm<Borrow>;
  sdkBorrowMsg: PortToElm<BorrowGivenPercentCalculate>;

  queryPay: PortFromElm<PayQuery>;
}

interface MetamaskMsg {
  chainId: string;
  user: string;
}

interface SdkPoolsMsg {
  asset: string;
  collateral: string;
  maturity: number;
  assetLiquidity: string;
  collateralLiquidity: string;
  apr: number;
  cf: string;
}

interface SdkBalancesMsg {
  token: string;
  balance: string;
}

interface SdkAllowancesMsg {
  erc20: string;
  allowance: string;
}

type SdkPositionsMsg = SdkPositionsMsg1 | SdkPositionsMsg2 | SdkPositionsMsg3;

interface SdkPositionsMsg1 {
  asset: string;
  collateral: string;
  maturity: number;
  bond: string;
  insurance: string;
}

interface SdkPositionsMsg2 {
  asset: string;
  collateral: string;
  maturity: number;
  bond: string;
  insurance: string;
  assetOut: string;
  collateralOut: string;
}

interface SdkPositionsMsg3 {
  asset: string;
  collateral: string;
  maturity: number;
  dues: {
    id: string;
    debt: string;
    collateral: string;
  }[];
}

interface Approve {
  erc20: string;
}

interface LendQuery {
  asset: string;
  collateral: string;
  maturity: number;
  assetIn: string;
  bondOut?: string;
  insuranceOut?: string;
  percent?: number;
  slippage: number;
}

interface Lend {
  asset: string;
  collateral: string;
  maturity: number;
  bondTo: string;
  insuranceTo: string;
  assetIn: string;
  percent: number;
  minBond: string;
  minInsurance: string;
  deadline: number;
}

interface LendGivenPercentCalculate {
  asset: string;
  collateral: string;
  maturity: number;
  assetIn: string;
  percent: number;
  bondOut: string;
  insuranceOut: string;
  minBond?: string;
  minInsurance?: string;
  apr: number;
  cf: string;
}

interface BorrowQuery {
  asset: string;
  collateral: string;
  maturity: number;
  assetOut: string;
  debtIn?: string;
  collateralIn?: string;
  percent?: number;
  slippage: number;
}

interface Borrow {
  asset: string;
  collateral: string;
  maturity: number;
  assetTo: string;
  dueTo: string;
  assetOut: string;
  percent: number;
  maxDebt: string;
  maxCollateral: string;
  deadline: number;
}

interface BorrowGivenPercentCalculate {
  asset: string;
  collateral: string;
  maturity: number;
  assetOut: string;
  percent: number;
  debtIn: string;
  collateralIn: string;
  maxDebt?: string;
  maxCollateral?: string;
  apr: number;
  cf: string;
}

interface PayQuery {
  asset: string;
  collateral: string;
  maturity: number;
  dues: { id: string; debt: string; collateral: string }[];
}
