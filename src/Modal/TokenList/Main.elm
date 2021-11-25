module Modal.TokenList.Main exposing
    ( Effect(..)
    , Modal
    , Msg
    , init
    , update
    )

import Blockchain.Main as Blockchain exposing (Blockchain)
import Data.Address as Address
import Data.Chains as Chains exposing (Chains)
import Data.ERC20 exposing (ERC20)
import Data.Remote as Remote exposing (Remote(..))
import Data.Token exposing (Token)
import Data.TokenParam exposing (TokenParam)
import Data.Web exposing (Web)
import Modal.TokenList.Error as Error exposing (Error)


type Modal
    = Modal
        { tokenParam : TokenParam
        , state : State
        }


type State
    = AllTokens
        { input : String
        , erc20 : Web (Result Error ERC20)
        }
    | CustomERC20s
    | ImportingERC20 ERC20


type Msg
    = GoToCustomERC20s
    | GoToAllTokens
    | GoToImportERC20
    | InputAddress String
    | ChooseToken Token
    | ImportERC20
    | ClearERC20 ERC20
    | ClearAll
    | Exit


init : TokenParam -> Modal
init tokenParam =
    { tokenParam = tokenParam
    , state =
        { input = ""
        , erc20 = Error.NoResult |> Err |> Success
        }
            |> AllTokens
    }
        |> Modal


type Effect
    = InputToken TokenParam Token
    | AddERC20 TokenParam ERC20
    | RemoveERC20 ERC20
    | RemoveAll


update :
    { model | chains : Chains }
    -> Blockchain
    -> Msg
    -> Modal
    -> ( Maybe Modal, Cmd Msg, Maybe Effect )
update { chains } blockchain msg (Modal modal) =
    case ( msg, modal.state ) of
        ( GoToCustomERC20s, AllTokens _ ) ->
            ( { modal | state = CustomERC20s }
                |> Modal
                |> Just
            , Cmd.none
            , Nothing
            )

        ( GoToAllTokens, CustomERC20s ) ->
            ( { modal
                | state =
                    { input = ""
                    , erc20 = Error.NoResult |> Err |> Success
                    }
                        |> AllTokens
              }
                |> Modal
                |> Just
            , Cmd.none
            , Nothing
            )

        ( GoToAllTokens, ImportingERC20 _ ) ->
            ( { modal
                | state =
                    { input = ""
                    , erc20 = Error.NoResult |> Err |> Success
                    }
                        |> AllTokens
              }
                |> Modal
                |> Just
            , Cmd.none
            , Nothing
            )

        ( InputAddress "", AllTokens _ ) ->
            ( { modal
                | state =
                    { input = ""
                    , erc20 = Error.NoResult |> Err |> Success
                    }
                        |> AllTokens
              }
                |> Modal
                |> Just
            , Cmd.none
            , Nothing
            )

        ( InputAddress string, AllTokens { input } ) ->
            if string == input then
                ( modal |> Modal |> Just
                , Cmd.none
                , Nothing
                )

            else
                string
                    |> Address.fromString
                    |> Maybe.map
                        (\address ->
                            chains
                                |> Chains.getGivenAddress
                                    (blockchain |> Blockchain.toChain)
                                    address
                                |> Maybe.map
                                    (\erc20 ->
                                        ( { modal
                                            | state =
                                                { input = string
                                                , erc20 =
                                                    erc20
                                                        |> Ok
                                                        |> Success
                                                }
                                                    |> AllTokens
                                          }
                                            |> Modal
                                            |> Just
                                        , Cmd.none
                                        , Nothing
                                        )
                                    )
                                |> Maybe.withDefault
                                    ( { modal
                                        | state =
                                            { input = string
                                            , erc20 = Loading
                                            }
                                                |> AllTokens
                                      }
                                        |> Modal
                                        |> Just
                                    , Debug.todo "cmd"
                                    , Nothing
                                    )
                        )
                    |> Maybe.withDefault
                        ( { modal
                            | state =
                                { input = string
                                , erc20 = Error.NoResult |> Err |> Success
                                }
                                    |> AllTokens
                          }
                            |> Modal
                            |> Just
                        , Cmd.none
                        , Nothing
                        )

        ( GoToImportERC20, AllTokens { erc20 } ) ->
            ( erc20
                |> (Remote.map << Result.map)
                    (\newERC20 ->
                        { modal | state = newERC20 |> ImportingERC20 }
                            |> Modal
                    )
                |> (Remote.map << Result.withDefault) (modal |> Modal)
                |> Remote.withDefault (modal |> Modal)
                |> Just
            , Cmd.none
            , Nothing
            )

        ( ChooseToken token, AllTokens _ ) ->
            ( Nothing
            , Cmd.none
            , InputToken modal.tokenParam token |> Just
            )

        ( ImportERC20, ImportingERC20 erc20 ) ->
            ( Nothing
            , Cmd.none
            , AddERC20 modal.tokenParam erc20 |> Just
            )

        ( ClearERC20 erc20, CustomERC20s ) ->
            ( modal |> Modal |> Just
            , Cmd.none
            , RemoveERC20 erc20 |> Just
            )

        ( ClearAll, CustomERC20s ) ->
            ( modal |> Modal |> Just
            , Cmd.none
            , RemoveAll |> Just
            )

        ( Exit, _ ) ->
            ( Nothing
            , Cmd.none
            , Nothing
            )

        _ ->
            ( modal |> Modal |> Just
            , Cmd.none
            , Nothing
            )
