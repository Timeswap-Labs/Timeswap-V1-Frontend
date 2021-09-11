declare module "*.svg" {}

declare const ethereum;

declare module "*.elm" {
  const Elm: ElmInstance<{
    connectMetamask: PortFromElm;
    metamaskMsg: PortToElm<{ chainId: string; user: string } | null>;
    noMetamask: PortToElm;
    sdkPoolsMsg: PortToElm<
      {
        asset: string;
        collateral: string;
        maturity: number;
        assetLiquidity: string;
        collateralLiquidity: string;
        apr: number;
        cf: string;
      }[]
    >;
    sdkBalancesMsg: PortToElm<
      {
        token: string;
        balance: string;
      }[]
    >;
  }>;
}
