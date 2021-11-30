module Modal.TokenList.Main exposing
    ( Effect(..)
    , Modal
    , Msg
    , init
    , update
    , view
    )

import Blockchain.Main as Blockchain exposing (Blockchain)
import Data.Address as Address exposing (Address)
import Data.Backdrop exposing (Backdrop)
import Data.Chains as Chains exposing (Chains)
import Data.ERC20 exposing (ERC20)
import Data.Images exposing (Images)
import Data.Remote as Remote exposing (Remote(..))
import Data.Token as Token exposing (Token)
import Data.TokenParam exposing (TokenParam)
import Data.Web exposing (Web)
import Element
    exposing
        ( Element
        , alignLeft
        , alpha
        , behindContent
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , none
        , padding
        , px
        , row
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Http
import Modal.TokenList.Answer as Answer exposing (Answer)
import Modal.TokenList.Error as Error exposing (Error)
import Modal.TokenList.Query as Query
import Process
import Task
import Utility.Color as Color
import Utility.Glass as Glass
import Utility.Image as Image


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
    | QueryAgain
    | ReceiveAnswer (Result Http.Error Answer)
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
update ({ chains } as model) blockchain msg (Modal modal) =
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
                                    , get model blockchain address
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

        ( QueryAgain, AllTokens allTokens ) ->
            case
                ( allTokens.input |> Address.fromString
                , allTokens.erc20
                )
            of
                ( Just address, Failure _ ) ->
                    chains
                        |> Chains.getGivenAddress
                            (blockchain |> Blockchain.toChain)
                            address
                        |> Maybe.map
                            (\erc20 ->
                                ( { modal
                                    | state =
                                        { input = allTokens.input
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
                                    { input = allTokens.input
                                    , erc20 = Loading
                                    }
                                        |> AllTokens
                              }
                                |> Modal
                                |> Just
                            , get model blockchain address
                            , Nothing
                            )

                _ ->
                    ( { modal
                        | state =
                            { input = allTokens.input
                            , erc20 = Error.NoResult |> Err |> Success
                            }
                                |> AllTokens
                      }
                        |> Modal
                        |> Just
                    , Cmd.none
                    , Nothing
                    )

        ( ReceiveAnswer (Ok answer), AllTokens allTokens ) ->
            ( (if
                (answer.chainId == (blockchain |> Blockchain.toChain))
                    && (Just answer.address
                            == (allTokens.input |> Address.fromString)
                       )
               then
                { modal
                    | state =
                        { allTokens
                            | erc20 =
                                answer.result
                                    |> Maybe.map Ok
                                    |> Maybe.withDefault
                                        (Err Error.NoResult)
                                    |> Success
                        }
                            |> AllTokens
                }

               else
                modal
              )
                |> Modal
                |> Just
            , Cmd.none
            , Nothing
            )

        ( ReceiveAnswer (Err error), AllTokens allTokens ) ->
            ( { modal
                | state =
                    { allTokens
                        | erc20 = Failure error
                    }
                        |> AllTokens
              }
                |> Modal
                |> Just
            , Process.sleep 5000
                |> Task.perform (\_ -> QueryAgain)
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


get : { model | chains : Chains } -> Blockchain -> Address -> Cmd Msg
get model blockchain address =
    Http.get
        { url = address |> Query.toUrlString blockchain
        , expect =
            Answer.decoder model
                |> Http.expectJson ReceiveAnswer
        }


view :
    { model | backdrop : Backdrop, images : Images, chains : Chains }
    -> Blockchain
    -> Modal
    -> Element Msg
view ({ backdrop, chains } as model) blockchain modal =
    Glass.outsideModal backdrop
        Exit
        (column
            [ width <| px 335
            , height <| px 300
            , centerX
            , centerY
            , padding 20
            , Background.color Color.light100
            ]
            (chains
                |> Chains.toList (blockchain |> Blockchain.toChain)
                |> List.map (tokenButton model)
            )
        )


tokenButton : { model | images : Images } -> Token -> Element Msg
tokenButton { images } token =
    Input.button
        [ width fill
        , height <| px 56
        ]
        { onPress = ChooseToken token |> Just
        , label =
            row
                [ width fill
                , height fill
                , padding 20
                , spacing 5
                ]
                [ images
                    |> Image.viewToken
                        [ width <| px 24
                        , alignLeft
                        , centerY
                        ]
                        token
                , el
                    [ alignLeft
                    , centerY
                    , Font.size 14
                    ]
                    (token
                        |> Token.toSymbol
                        |> text
                    )
                ]
        }
