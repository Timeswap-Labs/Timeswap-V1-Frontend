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
  sdkLendMsg: PortToElm<LendCalculate>;

  queryBorrow: PortFromElm<BorrowQuery>;
  queryBorrowPerSecond: PortFromElm<BorrowQuery>;
  approveBorrow: PortFromElm<Approve>;
  borrow: PortFromElm<Borrow>;
  sdkBorrowMsg: PortToElm<BorrowCalculate>;

  queryPay: PortFromElm<PayQuery>;
  approvePay: PortFromElm<Approve>;
  pay: PortFromElm<Pay>;
  sdkPayMsg: PortToElm<PayCalculate>;

  withdraw: PortFromElm<Withdraw>;
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

type SdkPositionsMsg =
  | SdkPositionsMsg1
  | SdkPositionsMsg2
  | SdkPositionsMsg3
  | SdkPositionsMsg4
  | SdkPositionsMsg5;

interface SdkPositionsMsg1 {
  asset: string;
  collateral: string;
  maturity: number;
  bond: string;
}

interface SdkPositionsMsg2 {
  asset: string;
  collateral: string;
  maturity: number;
  insurance: string;
}

interface SdkPositionsMsg3 {
  asset: string;
  collateral: string;
  maturity: number;
  bond: string;
  assetOut: string;
}

interface SdkPositionsMsg4 {
  asset: string;
  collateral: string;
  maturity: number;
  insurance: string;
  collateralOut: string;
}

interface SdkPositionsMsg5 {
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

type LendCalculate =
  | LendGivenPercentError
  | LendGivenPercent
  | LendGivenBondError
  | LendGivenBond
  | LendGivenInsuranceError
  | LendGivenInsurance;

interface LendGivenPercentError {
  asset: string;
  collateral: string;
  maturity: number;
  assetIn: string;
  percent: number;
}

interface LendGivenPercent {
  asset: string;
  collateral: string;
  maturity: number;
  assetIn: string;
  percent: number;
  result: {
    bondOut: string;
    insuranceOut: string;
    minBond: string;
    minInsurance: string;
    apr: number;
    cf: string;
  };
}

interface LendGivenBondError {
  asset: string;
  collateral: string;
  maturity: number;
  assetIn: string;
  bondOut: string;
}

interface LendGivenBond {
  asset: string;
  collateral: string;
  maturity: number;
  assetIn: string;
  bondOut: string;
  result: {
    percent: number;
    insuranceOut: string;
    minInsurance: string;
    apr: number;
    cf: string;
  };
}

interface LendGivenInsuranceError {
  asset: string;
  collateral: string;
  maturity: number;
  assetIn: string;
  insuranceOut: string;
}

interface LendGivenInsurance {
  asset: string;
  collateral: string;
  maturity: number;
  assetIn: string;
  insuranceOut: string;
  result: {
    percent: number;
    bondOut: string;
    minBond: string;
    apr: number;
    cf: string;
  };
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

type BorrowCalculate =
  | BorrowGivenPercentError
  | BorrowGivenPercent
  | BorrowGivenDebtError
  | BorrowGivenDebt
  | BorrowGivenCollateralError
  | BorrowGivenCollateral;

interface BorrowGivenPercentError {
  asset: string;
  collateral: string;
  maturity: number;
  assetOut: string;
  percent: number;
}

interface BorrowGivenPercent {
  asset: string;
  collateral: string;
  maturity: number;
  assetOut: string;
  percent: number;
  result: {
    debtIn: string;
    collateralIn: string;
    maxDebt: string;
    maxCollateral: string;
    apr: number;
    cf: string;
  };
}

interface BorrowGivenDebtError {
  asset: string;
  collateral: string;
  maturity: number;
  assetOut: string;
  debtIn: string;
}

interface BorrowGivenDebt {
  asset: string;
  collateral: string;
  maturity: number;
  assetOut: string;
  debtIn: string;
  result: {
    percent: number;
    collateralIn: string;
    maxCollateral: string;
    apr: number;
    cf: string;
  };
}

interface BorrowGivenCollateralError {
  asset: string;
  collateral: string;
  maturity: number;
  assetOut: string;
  collateralIn: string;
}

interface BorrowGivenCollateral {
  asset: string;
  collateral: string;
  maturity: number;
  assetOut: string;
  collateralIn: string;
  result: {
    percent: number;
    debtIn: string;
    maxDebt: string;
    apr: number;
    cf: string;
  };
}

interface PayQuery {
  asset: string;
  collateral: string;
  maturity: number;
  dues: { id: string; debt: string; collateral: string }[];
}

interface Pay {
  asset: string;
  collateral: string;
  maturity: number;
  collateralTo: string;
  ids: string[];
  maxAssetsIn: string[];
  deadline: number;
}

interface PayCalculate {
  asset: string;
  collateral: string;
  maturity: number;
  dues: { id: string; debt: string; collateral: string }[];
  assetIn: string;
  collateralOut: string;
}

interface Withdraw {
  asset: string;
  collateral: string;
  maturity: number;
  assetTo: string;
  collateralTo: string;
  claimsIn: { bond: string; insurance: string };
}
