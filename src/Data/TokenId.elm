module Data.TokenId exposing
    ( TokenId
    , decoder
    , encode
    , exampleList
    , fromFragment
    , sorter
    , toFragment
    )

import Data.Uint as Uint exposing (Uint)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Sort exposing (Sorter)
import Sort.Set as Set exposing (Set)


type TokenId
    = TokenId Uint


decoder : Decoder TokenId
decoder =
    Uint.decoder
        |> Decode.map TokenId


fromFragment : String -> Maybe (Set TokenId)
fromFragment string =
    string
        |> String.split "="
        |> List.concatMap (String.split ",")
        |> (\list ->
                case list of
                    "tokenIds" :: numbers ->
                        numbers
                            |> List.foldl
                                (\number accumulator ->
                                    number
                                        |> Uint.fromString
                                        |> Maybe.map TokenId
                                        |> Maybe.map (\tokenId -> accumulator |> Set.insert tokenId)
                                        |> Maybe.withDefault accumulator
                                )
                                (Set.empty sorter)
                            |> (\set ->
                                    if set |> Set.isEmpty then
                                        Nothing

                                    else
                                        Just set
                               )

                    _ ->
                        Nothing
           )


toFragment : Set TokenId -> String
toFragment set =
    set
        |> Set.toList
        |> List.map (\(TokenId uint) -> uint)
        |> List.map Uint.toString
        |> String.join ","
        |> (++) "tokenIds="


encode : TokenId -> Value
encode (TokenId uint) =
    uint
        |> Uint.encode


sorter : Sorter TokenId
sorter =
    Uint.sorter
        |> Sort.by (\(TokenId uint) -> uint)


exampleList : List TokenId
exampleList =
    Uint.example |> List.map TokenId
