module Data.Slippage exposing
    ( Option(..)
    , Slippage
    , fromOption
    , fromString
    , init
    , isCorrect
    , toOption
    , toString
    )


type Slippage
    = Slippage Float


type Option
    = Small
    | Medium
    | Large


init : Slippage
init =
    Slippage 0.005


toOption : Slippage -> Maybe Option
toOption (Slippage float) =
    if float == 0.001 then
        Just Small

    else if float == 0.005 then
        Just Medium

    else if float == 0.01 then
        Just Large

    else
        Nothing


toString : Slippage -> Maybe String
toString (Slippage float) =
    if float == 0.001 || float == 0.005 || float == 0.01 then
        Nothing

    else
        float
            |> (*) 100
            |> String.fromFloat
            |> Just


isCorrect : String -> Bool
isCorrect string =
    string
        |> String.toFloat
        |> Maybe.map ((*) 100)
        |> Maybe.map truncate
        |> Maybe.map
            (\int ->
                if int > 0 && int <= 5000 then
                    True

                else
                    False
            )
        |> Maybe.withDefault False


fromOption : Option -> Slippage
fromOption option =
    case option of
        Small ->
            Slippage 0.001

        Medium ->
            Slippage 0.005

        Large ->
            Slippage 0.01


fromString : String -> Slippage
fromString string =
    string
        |> String.toFloat
        |> Maybe.map ((*) 100)
        |> Maybe.map truncate
        |> Maybe.andThen
            (\int ->
                if int > 0 && int <= 5000 then
                    int
                        |> toFloat
                        |> (\basisPoint -> basisPoint / 10000)
                        |> Slippage
                        |> Just

                else
                    Nothing
            )
        |> Maybe.withDefault (Slippage 0.005)
