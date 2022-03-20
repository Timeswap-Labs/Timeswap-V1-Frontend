module Data.Chains exposing
    ( Chains
    , Flags
    , decoderChain
    , encodeCustom
    , fromFragment
    , getDoesNotExist
    , getGivenAddress
    , getGivenChainId
    , head
    , init
    , insert
    , insertAll
    , isMemberOf
    , remove
    , removeAll
    , toCustomTokenList
    , toERC20List
    , toList
    , toNative
    , toTokenList
    , toWrapper
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
import Sort.Set as Set


type Chains
    = Chains
        { defaultChain : Chain
        , defaultTokens : Tokens
        , others : Dict Chain Tokens
        }


type alias Flags =
    { default : FlagInfo
    , others : List FlagInfo
    }


type alias FlagInfo =
    { chainId : Int
    , name : String
    , rpcUrl : String
    , blockExplorerUrl : String
    , nftExplorerUrl : String
    , native : Native.Flag
    , wrapper : ERC20.Flag
    , whitelist : ERC20s.Flags
    , custom : ERC20s.Flags
    }


init : Flags -> Chains
init { default, others } =
    { defaultChain =
        { chainId = default.chainId
        , name = default.name
        , rpcUrl = default.rpcUrl
        , blockExplorerUrl = default.blockExplorerUrl
        , nftExplorerUrl = default.nftExplorerUrl
        }
            |> Chain
    , defaultTokens =
        Tokens.init
            (default.native |> Native.init)
            (default.wrapper |> ERC20.initWrapper)
            (default.whitelist |> ERC20s.init)
            (default.custom |> ERC20s.init)
    , others =
        others
            |> List.map
                (\{ chainId, name, rpcUrl, blockExplorerUrl, nftExplorerUrl, native, wrapper, whitelist, custom } ->
                    ( { chainId = chainId
                      , name = name
                      , rpcUrl = rpcUrl
                      , blockExplorerUrl = blockExplorerUrl
                      , nftExplorerUrl = nftExplorerUrl
                      }
                        |> Chain
                    , Tokens.init
                        (native |> Native.init)
                        (wrapper |> ERC20.initWrapper)
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


toList : Chains -> List Chain
toList (Chains { defaultChain, others }) =
    defaultChain
        :: (others
                |> Dict.toList
                |> List.map (\( chain, _ ) -> chain)
           )


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
                )
            |> Maybe.withDefault chains
            |> Chains


insertAll : Chain -> ERC20s -> Chains -> Chains
insertAll chain erc20s (Chains ({ defaultChain, defaultTokens, others } as chains)) =
    if chain == defaultChain then
        { chains | defaultTokens = defaultTokens |> Tokens.insertAll erc20s }
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
                                    (tokens |> Tokens.insertAll erc20s)
                    }
                )
            |> Maybe.withDefault chains
            |> Chains


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


toTokenList : Chain -> Chains -> List Token
toTokenList chain chains =
    chains
        |> toDict
        |> Dict.get chain
        |> Maybe.map Tokens.toList
        |> Maybe.withDefault []


toERC20List : Chain -> Chains -> List ERC20
toERC20List chain chains =
    chains
        |> toDict
        |> Dict.get chain
        |> Maybe.map Tokens.toERC20List
        |> Maybe.withDefault []


toNative : Chain -> Chains -> Token
toNative chain chains =
    chains
        |> toDict
        |> Dict.get chain
        |> Maybe.map Tokens.toNative
        |> Maybe.map Token.Native
        |> Maybe.withDefault (ERC20.zero |> Token.ERC20)


toWrapper : Chain -> Chains -> Token
toWrapper chain chains =
    chains
        |> toDict
        |> Dict.get chain
        |> Maybe.map Tokens.toWrapper
        |> Maybe.withDefault ERC20.zero
        |> Token.ERC20


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


getDoesNotExist : Chain -> Chains -> ERC20s -> ERC20s
getDoesNotExist chain (Chains { defaultChain, defaultTokens, others }) erc20s =
    if chain == defaultChain then
        erc20s |> Tokens.getDoesNotExist defaultTokens

    else
        others
            |> Dict.get chain
            |> Maybe.map (\tokens -> erc20s |> Tokens.getDoesNotExist tokens)
            |> Maybe.withDefault (Set.empty ERC20.sorter)
