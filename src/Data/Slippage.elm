module Data.Slippage exposing
    ( Flag
    , Option(..)
    , Slippage
    , encode
    , encodeGivenPercent
    , fromSettings
    , init
    , isCorrect
    , toSettings
    )

import Data.Or exposing (Or(..))
import Json.Encode as Encode exposing (Value)


type Slippage
    = Slippage Int


type alias Flag =
    Maybe Float


type Option
    = Small
    | Medium
    | Large


init : Flag -> Slippage
init maybeFloat =
    maybeFloat
        |> Maybe.andThen
            (\float ->
                float
                    |> (*) 1000
                    |> truncate
                    |> (\int ->
                            if int > 0 && int <= 5000 then
                                int
                                    |> Slippage
                                    |> Just

                            else
                                Nothing
                       )
            )
        |> Maybe.withDefault (Slippage 50)


encode : Slippage -> Value
encode (Slippage int) =
    int
        |> toFloat
        |> (\float -> float / 1000)
        |> Encode.float


encodeGivenPercent : Slippage -> Value
encodeGivenPercent (Slippage int) =
    int
        |> toFloat
        |> (\float -> float / 2000)
        |> Encode.float


toSettings : Slippage -> Or Option String
toSettings (Slippage int) =
    if int == 10 then
        Small |> Left

    else if int == 50 then
        Medium |> Left

    else if int == 100 then
        Large |> Left

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
            |> Right


fromSettings : Or Option String -> Slippage
fromSettings or =
    case or of
        Left Small ->
            Slippage 10

        Left Medium ->
            Slippage 50

        Left Large ->
            Slippage 100

        Right string ->
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
