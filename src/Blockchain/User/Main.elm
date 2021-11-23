module Blockchain.User.Main exposing
    ( Flag
    , NotSupported
    , User
    , decoder
    , init
    )

import Blockchain.User.Allowances exposing (Allowances)
import Blockchain.User.Balances exposing (Balances)
import Data.Address as Address exposing (Address)
import Data.Chain exposing (Chain)
import Data.Chains as Chains exposing (Chains)
import Data.Remote exposing (Remote(..))
import Data.Wallet as Wallet exposing (Wallet)
import Data.Web exposing (Web)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline


type alias User =
    { wallet : Wallet
    , address : Address
    , name : Maybe String
    , balances : Web Balances
    , allowances : Web Allowances
    }


type alias NotSupported =
    { wallet : Wallet
    , address : Address
    }


type alias Flag =
    { wallet : Wallet.Flag
    , chainId : Int
    , address : String
    }


init : Chains -> Flag -> Maybe (Result NotSupported ( Chain, User ))
init chains flag =
    case
        ( chains |> Chains.getGivenChain flag.chainId
        , flag.wallet |> Wallet.init
        , flag.address |> Address.fromString
        )
    of
        ( Just chain, Just wallet, Just address ) ->
            ( chain
            , { wallet = wallet
              , address = address
              , name = Nothing
              , balances = Loading
              , allowances = Loading
              }
            )
                |> Ok
                |> Just

        ( Nothing, Just wallet, Just address ) ->
            { wallet = wallet
            , address = address
            }
                |> Err
                |> Just

        _ ->
            Nothing


decoder : Chains -> Decoder (Maybe (Result NotSupported ( Chain, User )))
decoder chains =
    Decode.field "chainId" Decode.int
        |> Decode.andThen
            (\int ->
                chains
                    |> Chains.getGivenChain int
                    |> Maybe.map
                        (\chain ->
                            Decode.succeed
                                (\wallet address name ->
                                    ( chain
                                    , { wallet = wallet
                                      , address = address
                                      , name = name
                                      , balances = Loading
                                      , allowances = Loading
                                      }
                                    )
                                        |> Ok
                                )
                                |> Pipeline.required "wallet" Wallet.decoder
                                |> Pipeline.required "address" Address.decoder
                                |> Pipeline.required "name"
                                    (Decode.string |> Decode.nullable)
                        )
                    |> Maybe.withDefault
                        (Decode.succeed
                            (\wallet address ->
                                { wallet = wallet
                                , address = address
                                }
                                    |> Err
                            )
                            |> Pipeline.required "wallet" Wallet.decoder
                            |> Pipeline.required "address" Address.decoder
                        )
            )
        |> Decode.nullable
