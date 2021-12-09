module Page.Transaction.Liquidity.NewError exposing (Transaction, init)


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
