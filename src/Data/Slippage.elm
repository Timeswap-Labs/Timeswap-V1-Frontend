module Data.Slippage exposing
    ( Flag
    , Option(..)
    , Slippage
    , decoder
    , decoderGivenPercent
    , encode
    , encodeGivenPercent
    , fromOption
    , fromString
    , init
    , isCorrect
    , toOption
    , toSettings
    , toString
    )

import Data.Or exposing (Or(..))
import Json.Decode as Decode exposing (Decoder)
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
                    |> (*) 10000
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


decoder : Decoder Slippage
decoder =
    Decode.float
        |> Decode.andThen
            (\float ->
                float
                    |> (*) 10000
                    |> truncate
                    |> (\int ->
                            if int > 0 && int <= 5000 then
                                int
                                    |> Slippage
                                    |> Decode.succeed

                            else
                                Decode.fail "Not a slippage"
                       )
            )


decoderGivenPercent : Decoder Slippage
decoderGivenPercent =
    Decode.float
        |> Decode.andThen
            (\float ->
                float
                    |> (*) 20000
                    |> truncate
                    |> (\int ->
                            if int > 0 && int <= 5000 then
                                int
                                    |> Slippage
                                    |> Decode.succeed

                            else
                                Decode.fail "Not a slippage"
                       )
            )


encode : Slippage -> Value
encode (Slippage int) =
    int
        |> toFloat
        |> (\float -> float / 10000)
        |> Encode.float


encodeGivenPercent : Slippage -> Value
encodeGivenPercent (Slippage int) =
    int
        |> toFloat
        |> (\float -> float / 20000)
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
        |> Maybe.map truncate
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


toOption : Or Slippage string -> Maybe Option
toOption or =
    case or of
        Left (Slippage int) ->
            if int == 10 then
                Just Small

            else if int == 50 then
                Just Medium

            else if int == 100 then
                Just Large

            else
                Nothing

        _ ->
            Nothing


toString : Or Slippage String -> String
toString or =
    case or of
        Left (Slippage int) ->
            if int == 10 || int == 50 || int == 100 then
                ""

            else
                int
                    |> String.fromInt
                    |> String.padLeft 3 '0'
                    |> (\string ->
                            [ string |> String.dropRight 2
                            , string |> String.right 2
                            ]
                                |> String.join "."
                       )

        Right string ->
            string


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
