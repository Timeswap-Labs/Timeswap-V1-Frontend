interface Borrow {
  chainId: Chain;
  pool: Pool;
  poolInfo: PoolInfo;
  slippage: number;
}

interface BorrowResult {
  apr: number;
  cdp: {
    percent: number | null;
    ratio: Uint;
  };
}

interface BorrowGivenPercentQuery extends Borrow {
  assetOut: Uint;
  percent: number;
}

interface BorrowGivenPercentAnswer extends BorrowGivenPercentQuery {
  result: BorrowGivenPercentResult;
}

interface BorrowGivenPercentResult extends BorrowResult {
  debtOut: Uint;
  collateralOut: Uint;
  maxDebt: Uint;
  maxCollateral: Uint;
}

interface BorrowGivenMaxQuery extends Borrow {
  collateralOut: Uint;
  percent: number;
}

interface BorrowGivenMaxAnswer extends BorrowGivenMaxQuery {
  result: BorrowGivenMaxResult;
}

interface BorrowGivenMaxResult extends BorrowResult {
  assetOut: Uint;
  debtOut: Uint;
  maxDebt: Uint;
}

interface BorrowGivenDebtQuery extends Borrow {
  assetOut: Uint;
  debtOut: Uint;
}

interface BorrowGivenDebtAnswer extends BorrowGivenDebtQuery {
  result: BorrowGivenDebtResult;
}

interface BorrowGivenDebtResult extends BorrowResult {
  percent: number;
  collateralOut: Uint;
  maxCollateral: Uint;
}

interface BorrowGivenCollateralQuery extends Borrow {
  assetOut: Uint;
  collateralOut: Uint;
}

interface BorrowGivenCollateralAnswer extends BorrowGivenCollateralQuery {
  result: BorrowGivenCollateralResult;
}

interface BorrowGivenCollateralResult extends BorrowResult {
  percent: number;
  debtOut: Uint;
  maxDebt: Uint;
}
