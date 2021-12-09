module Page.Transaction.Lend.LendError exposing
    ( BondInput
    , ClaimsOut(..)
    , InsuranceInput
    , Transaction
    , init
    )

import Data.Percent exposing (Percent)


type alias Transaction =
    { assetIn : String
    , claimsOut : ClaimsOut
    }


type ClaimsOut
    = Default
    | Slider Percent
    | Bond BondInput
    | Insurance InsuranceInput


type alias BondInput =
    { percent : Percent
    , bondOut : String
    }


type alias InsuranceInput =
    { percent : Percent
    , insuranceOut : String
    }


init : Transaction
init =
    { assetIn = ""
    , claimsOut = Default
    }
