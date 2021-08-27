module Data.Settings exposing
    ( Settings
    , chooseDeadlineOption
    , chooseSlippageOption
    , init
    , inputDeadline
    , inputSlippage
    )

import Data.Deadline as Deadline exposing (Deadline)
import Data.Slippage as Slippage exposing (Slippage)


type alias Settings =
    { slippage : Slippage
    , deadline : Deadline
    }


init : Settings
init =
    { slippage = Slippage.init Slippage.MediumOption
    , deadline = Deadline.init Deadline.MediumOption
    }


chooseSlippageOption : Slippage.Option -> Settings -> Settings
chooseSlippageOption option settings =
    { settings | slippage = Slippage.init option }


chooseDeadlineOption : Deadline.Option -> Settings -> Settings
chooseDeadlineOption option settings =
    { settings | deadline = Deadline.init option }


inputSlippage : String -> Settings -> Settings
inputSlippage input settings =
    { settings | slippage = Slippage.input input |> Maybe.withDefault (Slippage.init Slippage.MediumOption) }


inputDeadline : String -> Settings -> Settings
inputDeadline input settings =
    { settings | deadline = Deadline.input input |> Maybe.withDefault (Deadline.init Deadline.MediumOption) }
