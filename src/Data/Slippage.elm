module Data.Slippage exposing
    ( Option(..)
    , Slippage
    , encode
    , fromOption
    , fromString
    , init
    , isCorrect
    , toOption
    , toPercent
    , toString
    )

import Json.Encode as Encode exposing (Value)


type Slippage
    = Slippage Int


type Option
    = Small
    | Medium
    | Large


encode : Slippage -> Value
encode (Slippage int) =
    int
        |> toFloat
        |> (\float -> float / 1000)
        |> Encode.float


init : Slippage
init =
    Slippage 50


toOption : Slippage -> Maybe Option
toOption (Slippage int) =
    if int == 10 then
        Just Small

    else if int == 50 then
        Just Medium

    else if int == 100 then
        Just Large

    else
        Nothing


toString : Slippage -> Maybe String
toString (Slippage int) =
    if int == 10 || int == 50 || int == 100 then
        Nothing

    else
        int
            |> String.fromInt
            |> String.padLeft 3 '0'
            |> (\string ->
                    [ string |> String.dropRight 2
                    , string |> String.right 2 |> String.padRight 2 '0'
                    ]
                        |> String.join "."
               )
            |> Just


toPercent : Slippage -> String
toPercent (Slippage int) =
    int
        |> String.fromInt
        |> String.padLeft 3 '0'
        |> (\string ->
                [ string |> String.dropRight 2
                , string |> String.right 2
                , "%"
                ]
                    |> String.join "."
           )


isCorrect : String -> Bool
isCorrect string =
    string
        |> String.toFloat
        |> Maybe.map ((*) 100)
        |> Maybe.map floor
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
            Slippage 10

        Medium ->
            Slippage 50

        Large ->
            Slippage 100


fromString : String -> Slippage
fromString string =
    string
        |> String.toFloat
        |> Maybe.map ((*) 100)
        |> Maybe.map floor
        |> Maybe.andThen
            (\int ->
                if int > 0 && int <= 5000 then
                    int
                        |> Slippage
                        |> Just

                else
                    Nothing
            )
        |> Maybe.withDefault (Slippage 50)
