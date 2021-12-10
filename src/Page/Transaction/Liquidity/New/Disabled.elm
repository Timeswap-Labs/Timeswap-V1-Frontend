module Page.Transaction.Liquidity.New.Disabled exposing (Transaction, init)


type alias Transaction =
    { assetIn : String
    , debtOut : String
    , collateralOut : String
    }


init : Transaction
init =
    { assetIn = ""
    , debtOut = ""
    , collateralOut = ""
    }
