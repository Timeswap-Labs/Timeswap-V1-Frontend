interface Lend {
  chainId: Chain;
  pool: Pool;
  poolInfo: PoolInfo;
  assetIn: Uint;
  slippage: number;
}

interface LendResult {
  apr: number;
  cdp: {
    percent: number | null;
    ratio: Uint;
  };
}

interface LendGivenPercentQuery extends Lend {
  percent: number;
}

interface LendGivenPercentAnswer extends LendGivenPercentQuery {
  result: LendGivenPercentResult;
}

interface LendGivenPercentResult extends LendResult {
  bondOut: Uint;
  insuranceOut: Uint;
  minBond: Uint;
  minInsurance: Uint;
}

interface LendGivenBondQuery extends Lend {
  bondOut: Uint;
}

interface LendGivenBondAnswer extends LendGivenBondQuery {
  result: LendGivenBondResult;
}

interface LendGivenBondResult extends LendResult {
  percent: number;
  insuranceOut: Uint;
  minInsurance: Uint;
}

interface LendGivenInsuranceQuery extends Lend {
  insuranceOut: Uint;
}

interface LendGivenInsuranceAnswer extends LendGivenInsuranceQuery {
  result: LendGivenInsuranceResult;
}

interface LendGivenInsuranceResult extends LendResult {
  percent: number;
  bondOut: Uint;
  minBond: Uint;
}
