module Data.Chains exposing
    ( Chains
    , Flags
    , decoderChain
    , encodeCustom
    , fromFragment
    , getGivenAddress
    , getGivenChainId
    , head
    , init
    , insert
    , isMemberOf
    , remove
    , removeAll
    , toCustomTokenList
    , toList
    )

import Data.Address exposing (Address)
import Data.Chain as Chain exposing (Chain(..))
import Data.ERC20 as ERC20 exposing (ERC20)
import Data.ERC20s as ERC20s exposing (ERC20s)
import Data.Native as Native
import Data.Token as Token exposing (Token)
import Data.TokenParam exposing (TokenParam)
import Data.Tokens as Tokens exposing (Tokens)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Sort.Dict as Dict exposing (Dict)
import Sort.Set as Set exposing (Set)


type Chains
    = Chains
        { defaultChain : Chain
        , defaultTokens : Tokens
        , others : Dict Chain Tokens
        }


type alias Flags =
    { default :
        { chainId : Int
        , name : String
        , etherscan : String
        , native : Native.Flag
        , whitelist : ERC20s.Flags
        , custom : ERC20s.Flags
        }
    , others :
        List
            { chainId : Int
            , name : String
            , etherscan : String
            , native : Native.Flag
            , whitelist : ERC20s.Flags
            , custom : ERC20s.Flags
            }
    }


init : Flags -> Chains
init { default, others } =
    { defaultChain =
        { chainId = default.chainId
        , name = default.name
        , etherscan = default.etherscan
        }
            |> Chain
    , defaultTokens =
        Tokens.init
            (default.native |> Native.init)
            (default.whitelist |> ERC20s.init)
            (default.custom |> ERC20s.init)
    , others =
        others
            |> List.map
                (\{ chainId, name, etherscan, native, whitelist, custom } ->
                    ( { chainId = chainId
                      , name = name
                      , etherscan = etherscan
                      }
                        |> Chain
                    , Tokens.init
                        (native |> Native.init)
                        (whitelist |> ERC20s.init)
                        (custom |> ERC20s.init)
                    )
                )
            |> Dict.fromList Chain.sorter
    }
        |> Chains


encodeCustom : Chains -> Value
encodeCustom chains =
    chains
        |> toDict
        |> Dict.map (\_ tokens -> tokens |> Tokens.toCustom)
        |> Dict.toList
        |> List.map
            (\( chain, custom ) ->
                ( chain |> Chain.toChainId |> String.fromInt
                , custom |> ERC20s.encode
                )
            )
        |> Encode.object


toDict : Chains -> Dict Chain Tokens
toDict (Chains { defaultChain, defaultTokens, others }) =
    others
        |> Dict.insert defaultChain defaultTokens


decoderChain : Chains -> Decoder Chain
decoderChain chains =
    Decode.int
        |> Decode.andThen
            (\int ->
                chains
                    |> getGivenChainId int
                    |> Maybe.map Decode.succeed
                    |> Maybe.withDefault
                        (Decode.fail "Not a chain")
            )


fromFragment : TokenParam -> Chain -> Chains -> String -> Maybe Token
fromFragment chosenParam chain chains fragment =
    chains
        |> toDict
        |> Dict.get chain
        |> Maybe.andThen
            (\tokens ->
                fragment
                    |> Tokens.fromFragment chosenParam tokens
            )


isMemberOf : Chain -> Chains -> Address -> Bool
isMemberOf chain chains address =
    chains
        |> toDict
        |> Dict.get chain
        |> Maybe.map
            (\tokens ->
                address
                    |> Tokens.isMemberOf tokens
            )
        |> Maybe.withDefault False


insert : Chain -> ERC20 -> Chains -> Chains
insert chain erc20 (Chains ({ defaultChain, defaultTokens, others } as chains)) =
    if chain == defaultChain then
        { chains | defaultTokens = defaultTokens |> Tokens.insert erc20 }
            |> Chains

    else
        others
            |> Dict.get chain
            |> Maybe.map
                (\tokens ->
                    { chains
                        | others =
                            others
                                |> Dict.insert chain
                                    (tokens |> Tokens.insert erc20)
                    }
                        |> Chains
                )
            |> Maybe.withDefault (Chains chains)


remove : Chain -> ERC20 -> Chains -> Chains
remove chain erc20 (Chains ({ defaultChain, defaultTokens, others } as chains)) =
    if chain == defaultChain then
        { chains | defaultTokens = defaultTokens |> Tokens.remove erc20 }
            |> Chains

    else
        others
            |> Dict.get chain
            |> Maybe.map
                (\tokens ->
                    { chains
                        | others =
                            others
                                |> Dict.insert chain
                                    (tokens |> Tokens.remove erc20)
                    }
                        |> Chains
                )
            |> Maybe.withDefault (Chains chains)


removeAll : Chain -> Chains -> Chains
removeAll chain (Chains ({ defaultChain, defaultTokens, others } as chains)) =
    if chain == defaultChain then
        { chains | defaultTokens = defaultTokens |> Tokens.removeAll }
            |> Chains

    else
        others
            |> Dict.get chain
            |> Maybe.map
                (\tokens ->
                    { chains
                        | others =
                            others
                                |> Dict.insert chain
                                    (tokens |> Tokens.removeAll)
                    }
                        |> Chains
                )
            |> Maybe.withDefault (Chains chains)


toCustomTokenList : Chain -> Chains -> List Token
toCustomTokenList chain chains =
    chains
        |> toDict
        |> Dict.get chain
        |> Maybe.map Tokens.toCustomList
        |> Maybe.withDefault []


toList : Chain -> Chains -> List Token
toList chain chains =
    chains
        |> toDict
        |> Dict.get chain
        |> Maybe.map Tokens.toList
        |> Maybe.withDefault []


head : Chains -> Chain
head (Chains { defaultChain }) =
    defaultChain


getGivenChainId : Int -> Chains -> Maybe Chain
getGivenChainId givenId chains =
    chains
        |> toDict
        |> Dict.foldl
            (\((Chain { chainId }) as chain) _ accumulator ->
                if givenId == chainId then
                    Just chain

                else
                    accumulator
            )
            Nothing


getGivenAddress : Chain -> Address -> Chains -> Maybe ERC20
getGivenAddress chain address chains =
    chains
        |> toDict
        |> Dict.get chain
        |> Maybe.andThen (Tokens.getGivenAddress address)
