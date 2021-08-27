module Data.Slippage exposing
    ( Option(..)
    , Output(..)
    , Slippage
    , init
    , input
    , toOutput
    )


type Slippage
    = Slippage_ Int
    | Small_
    | Medium_
    | Large_


type Output
    = Slippage String
    | Small
    | Medium
    | Large


type Option
    = SmallOption
    | MediumOption
    | LargeOption


init : Option -> Slippage
init option =
    case option of
        SmallOption ->
            Small_

        MediumOption ->
            Medium_

        LargeOption ->
            Large_


input : String -> Maybe Slippage
input string =
    string
        |> String.toFloat
        |> Maybe.map ((*) 100)
        |> Maybe.map truncate
        --|> Maybe.andThen ((<) 0 |> Utility.require)
        --|> Maybe.andThen ((>=) 5000 |> Utility.require)
        |> Maybe.map Slippage_


toOutput : Slippage -> Output
toOutput slippage =
    case slippage of
        Slippage_ basisPoint ->
            let
                string : String
                string =
                    basisPoint
                        |> String.fromInt
                        |> String.padLeft 3 '0'

                whole : String
                whole =
                    string
                        |> String.dropRight 2

                decimals : String
                decimals =
                    string
                        |> String.right 2
            in
            whole
                ++ "."
                ++ decimals
                |> Slippage

        Small_ ->
            Small

        Medium_ ->
            Medium

        Large_ ->
            Large
