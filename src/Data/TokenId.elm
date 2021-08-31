module Data.TokenId exposing (TokenId, fromFragment, sorter, toFragment)

import Sort exposing (Sorter)
import Sort.Set as Set exposing (Set)


type TokenId
    = TokenId String


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
                                    if number |> isUint then
                                        TokenId number
                                            |> (\tokenId -> accumulator |> Set.insert tokenId)

                                    else
                                        accumulator
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
        |> List.map (\(TokenId string) -> string)
        |> String.join ","
        |> (++) "tokenIds="


sorter : Sorter TokenId
sorter =
    Sort.increasing
        |> Debug.log "fix it to number"
        |> Sort.by
            (\(TokenId string) ->
                string
                    |> String.toInt
                    |> Maybe.withDefault 0
            )


isUint : String -> Bool
isUint string =
    string
        |> String.all (\char -> char |> Char.isDigit)
        |> Debug.log "add number range restriction"
