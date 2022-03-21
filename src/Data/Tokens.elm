module Data.Tokens exposing
    ( Tokens
    , decoderERC20
    , decoderToken
    , encodeCustom
    , fromFragment
    , getDoesNotExist
    , getGivenAddress
    , init
    , insert
    , insertAll
    , isMemberOf
    , remove
    , removeAll
    , toCustom
    , toCustomList
    , toERC20List
    , toList
    , toNative
    , toWrapper
    )

import Data.Address as Address exposing (Address)
import Data.ERC20 as ERC20 exposing (ERC20)
import Data.ERC20s as ERC20s exposing (ERC20s)
import Data.Native as Native exposing (Native)
import Data.Token as Token exposing (Token)
import Data.TokenParam as TokenParam exposing (TokenParam)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Sort.Set as Set exposing (Set)


type Tokens
    = Tokens
        { native : Native
        , wrapper : ERC20
        , whitelist : ERC20s
        , custom : ERC20s
        }


init : Native -> ERC20 -> ERC20s -> ERC20s -> Tokens
init native wrapper whitelist custom =
    { native = native
    , wrapper = wrapper
    , whitelist = whitelist
    , custom = custom
    }
        |> Tokens


decoderToken : Tokens -> Decoder Token
decoderToken tokens =
    Decode.oneOf
        [ decoderNative tokens
            |> Decode.map Token.Native
        , decoderERC20 tokens
            |> Decode.map Token.ERC20
        ]


decoderNative : Tokens -> Decoder Native
decoderNative (Tokens { native }) =
    Decode.string
        |> Decode.andThen
            (\string ->
                if string == (native |> Native.toSymbol) then
                    native |> Decode.succeed

                else
                    Decode.fail "Not an ETH"
            )


decoderERC20 : Tokens -> Decoder ERC20
decoderERC20 tokens =
    Address.decoder
        |> Decode.andThen
            (\address ->
                tokens
                    |> toSet
                    |> Set.foldl
                        (\erc20 accumulator ->
                            if (erc20 |> ERC20.toAddress) == address then
                                Decode.succeed erc20

                            else
                                accumulator
                        )
                        (Decode.fail "not in whitelist")
            )


encodeCustom : Tokens -> Value
encodeCustom (Tokens { custom }) =
    custom
        |> ERC20s.encode


toNative : Tokens -> Native
toNative (Tokens { native }) =
    native


toWrapper : Tokens -> ERC20
toWrapper (Tokens { wrapper }) =
    wrapper


toCustom : Tokens -> ERC20s
toCustom (Tokens { custom }) =
    custom


toSet : Tokens -> Set ERC20
toSet (Tokens { whitelist, custom }) =
    whitelist
        |> Set.union ERC20.sorter custom


fromFragment : TokenParam -> Tokens -> String -> Maybe Token
fromFragment chosenParam ((Tokens { native }) as tokens) fragment =
    fragment
        |> String.split "="
        |> (\list ->
                case list of
                    tokenParamString :: tokenString :: _ ->
                        tokenParamString
                            |> TokenParam.fromFragment
                            |> Maybe.andThen
                                (\tokenParam ->
                                    if tokenParam == chosenParam then
                                        Just ()

                                    else
                                        Nothing
                                )
                            |> Maybe.andThen
                                (\_ ->
                                    if
                                        (native |> Native.toSymbol)
                                            == tokenString
                                    then
                                        native
                                            |> Token.Native
                                            |> Just

                                    else
                                        tokens
                                            |> toSet
                                            |> Set.foldl
                                                (\erc20 accumulator ->
                                                    if
                                                        (erc20
                                                            |> ERC20.toAddress
                                                            |> Address.toString
                                                            |> String.toLower
                                                        )
                                                            == (tokenString |> String.toLower)
                                                    then
                                                        Token.ERC20 erc20
                                                            |> Just

                                                    else
                                                        accumulator
                                                )
                                                Nothing
                                )

                    _ ->
                        Nothing
           )


isMemberOf : Tokens -> Address -> Bool
isMemberOf tokens address =
    tokens
        |> toSet
        |> Set.foldl
            (\erc20 accumulator ->
                if
                    (erc20 |> ERC20.toAddress)
                        == address
                then
                    True

                else
                    accumulator
            )
            False


insert : ERC20 -> Tokens -> Tokens
insert erc20 (Tokens tokens) =
    { tokens
        | custom =
            tokens.custom
                |> Set.insert erc20
    }
        |> Tokens


insertAll : ERC20s -> Tokens -> Tokens
insertAll erc20s (Tokens tokens) =
    { tokens
        | custom =
            tokens.custom
                |> Set.union ERC20.sorter
                    (erc20s
                        |> Set.dropIf (Set.memberOf tokens.whitelist)
                    )
    }
        |> Tokens


remove : ERC20 -> Tokens -> Tokens
remove erc20 (Tokens tokens) =
    { tokens
        | custom =
            tokens.custom
                |> Set.remove erc20
    }
        |> Tokens


removeAll : Tokens -> Tokens
removeAll (Tokens tokens) =
    { tokens
        | custom = Set.empty ERC20.sorter
    }
        |> Tokens


toList : Tokens -> List Token
toList ((Tokens { native }) as tokens) =
    tokens
        |> toERC20List
        |> List.map Token.ERC20
        |> (::) (Token.Native native)


toERC20List : Tokens -> List ERC20
toERC20List tokens =
    tokens
        |> toSet
        |> Set.toList


toCustomList : Tokens -> List Token
toCustomList (Tokens { custom }) =
    custom
        |> Set.toList
        |> List.map Token.ERC20


getGivenAddress : Address -> Tokens -> Maybe ERC20
getGivenAddress address tokens =
    tokens
        |> toSet
        |> Set.foldl
            (\erc20 accumulator ->
                if (erc20 |> ERC20.toAddress) == address then
                    Just erc20

                else
                    accumulator
            )
            Nothing


getDoesNotExist : Tokens -> ERC20s -> ERC20s
getDoesNotExist (Tokens tokens) erc20s =
    erc20s
        |> Set.dropIf (Set.memberOf tokens.whitelist)
        |> Set.dropIf (Set.memberOf tokens.custom)
