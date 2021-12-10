module Modal.TokenList.Main exposing
    ( Effect(..)
    , Modal
    , Msg
    , init
    , update
    , view
    )

import Blockchain.Main as Blockchain exposing (Blockchain)
import Blockchain.User.Main exposing (User, getBalance)
import Data.Address as Address exposing (Address)
import Data.Backdrop exposing (Backdrop)
import Data.Chain exposing (Chain)
import Data.Chains as Chains exposing (Chains)
import Data.ERC20 exposing (ERC20)
import Data.Images exposing (Images)
import Data.Remote as Remote exposing (Remote(..))
import Data.Token as Token exposing (Token)
import Data.TokenParam exposing (TokenParam)
import Data.Uint as Uint exposing (Uint)
import Data.Web as Web exposing (Web)
import Element
    exposing
        ( Element
        , alignLeft
        , alignRight
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
        , paddingEach
        , paddingXY
        , px
        , row
        , shrink
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input exposing (Placeholder)
import Http
import Modal.TokenList.Answer as Answer exposing (Answer)
import Modal.TokenList.Error as Error exposing (Error)
import Modal.TokenList.Query as Query
import Modal.TokenList.Tooltip as Tooltip exposing (Tooltip)
import Process
import Task
import Utility.Color as Color
import Utility.Glass as Glass
import Utility.Image as Image


type Modal
    = Modal
        { tokenParam : TokenParam
        , state : State
        , tooltip : Maybe Tooltip
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
    | ReceiveAnswer Chain Address (Result Http.Error Answer)
    | ChooseToken Token
    | ImportERC20
    | ClearERC20 ERC20
    | ClearAll
    | OnMouseEnter Tooltip
    | OnMouseLeave
    | Exit


init : TokenParam -> Modal
init tokenParam =
    { tokenParam = tokenParam
    , state =
        { input = ""
        , erc20 = Error.NoResult |> Err |> Success
        }
            |> AllTokens
    , tooltip = Nothing
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
                                    , get blockchain address
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
                            , get blockchain address
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

        ( ReceiveAnswer chain address result, AllTokens allTokens ) ->
            ( (if
                (chain == (blockchain |> Blockchain.toChain))
                    && (Just address
                            == (allTokens.input |> Address.fromString)
                       )
               then
                { modal
                    | state =
                        { allTokens
                            | erc20 =
                                result
                                    |> (Result.map << Maybe.map) Ok
                                    |> (Result.map << Maybe.withDefault)
                                        (Err Error.NoResult)
                                    |> Web.fromResult
                        }
                            |> AllTokens
                }

               else
                modal
              )
                |> Modal
                |> Just
            , Process.sleep 5000
                |> Task.perform (\_ -> QueryAgain)
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

        ( OnMouseEnter tooltip, _ ) ->
            ( { modal | tooltip = Just tooltip }
                |> Modal
                |> Just
            , Cmd.none
            , Nothing
            )

        ( OnMouseLeave, _ ) ->
            ( { modal | tooltip = Nothing }
                |> Modal
                |> Just
            , Cmd.none
            , Nothing
            )

        _ ->
            ( modal |> Modal |> Just
            , Cmd.none
            , Nothing
            )


get : Blockchain -> Address -> Cmd Msg
get blockchain address =
    blockchain
        |> Blockchain.toChain
        |> (\chain ->
                Http.get
                    { url = address |> Query.toUrlString blockchain
                    , expect =
                        Answer.decoder
                            |> Http.expectJson (ReceiveAnswer chain address)
                    }
           )


view :
    { model | backdrop : Backdrop, images : Images, chains : Chains }
    -> Blockchain
    -> Modal
    -> Element Msg
view ({ backdrop, images, chains } as model) blockchain modal =
    Glass.outsideModal backdrop
        Exit
        (column
            [ width <| px 350
            , height shrink
            , centerX
            , centerY
            , padding 0
            , Background.color Color.background
            , Border.color Color.transparent100
            , Border.width 1
            , Border.rounded 8
            ]
            [ modalHeader model modal
            , tokenList model blockchain
            , manageTokensBtn model
            ]
        )


modalHeader : { model | images : Images } -> Modal -> Element Msg
modalHeader { images } (Modal { state }) =
    column
        [ width fill
        , height shrink
        , spacing 16
        , paddingEach
            { top = 24
            , right = 24
            , bottom = 20
            , left = 24
            }
        , Font.color Color.light100
        , Font.bold
        , Font.size 18
        , Border.color Color.transparent100
        , Border.widthEach
            { top = 0
            , right = 0
            , bottom = 1
            , left = 0
            }
        ]
        [ row
            [ width fill ]
            [ el
                [ alignLeft
                ]
                (text "Select Token")
            , Input.button
                [ width shrink
                , alignRight
                , centerY
                ]
                { onPress = Just Exit
                , label =
                    images
                        |> Image.close
                            [ width <| px 24
                            , height <| px 24
                            ]
                }
            ]
        , Input.text
            [ width fill
            , height fill
            , padding 14
            , Font.size 16
            , Font.regular
            , Background.color Color.completelyTransparent
            , Border.width 1
            , Border.color Color.transparent100
            , Border.rounded 8
            ]
            { onChange = InputAddress
            , text =
                case state of
                    AllTokens { input } ->
                        input

                    _ ->
                        ""
            , placeholder =
                Input.placeholder
                    [ Font.color Color.transparent300
                    , Font.size 14
                    , Font.regular
                    ]
                    (text "Search a token or paste address")
                    |> Just
            , label = Input.labelHidden "Search token"
            }
        ]


tokenList :
    { model | backdrop : Backdrop, images : Images, chains : Chains }
    -> Blockchain
    -> Element Msg
tokenList ({ backdrop, chains } as model) blockchain =
    column
        [ width fill
        , paddingXY 24 6
        ]
        (chains
            |> Chains.toList (blockchain |> Blockchain.toChain)
            |> List.map (tokenButton model blockchain)
        )


tokenButton :
    { model | images : Images }
    -> Blockchain
    -> Token
    -> Element Msg
tokenButton { images } blockchain token =
    Input.button
        [ width fill
        , height shrink
        , Font.color Color.transparent500
        ]
        { onPress = ChooseToken token |> Just
        , label =
            row
                [ width fill
                , height fill
                , paddingXY 0 10
                , centerY
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
                    , paddingEach
                        { top = 0
                        , right = 0
                        , bottom = 0
                        , left = 8
                        }
                    , Font.size 16
                    , Font.bold
                    ]
                    (token
                        |> Token.toSymbol
                        |> text
                    )
                , el
                    [ height <| px 23
                    , paddingXY 5 0
                    , Font.size 22
                    , Font.color Color.transparent300
                    ]
                    (text (String.fromChar (Char.fromCode 0x2022)))
                , el
                    [ Font.size 12
                    , Font.color Color.transparent300
                    , Font.regular
                    ]
                    (token
                        |> Token.toName
                        |> text
                    )
                , case blockchain |> Blockchain.toUser of
                    Just user ->
                        tokenBalance user token

                    _ ->
                        none
                ]
        }


tokenBalance : User -> Token -> Element Msg
tokenBalance user token =
    let
        balances =
            user |> getBalance token
    in
    case balances of
        Just balance ->
            el
                [ alignRight
                , Font.regular
                , Font.size 16
                ]
                (balance |> Uint.toString |> text)

        _ ->
            none


manageTokensBtn : { model | images : Images } -> Element Msg
manageTokensBtn { images } =
    el
        [ width fill
        , height shrink
        , paddingXY 0 17
        , centerX
        , centerY
        , Font.size 14
        , Font.regular
        , Font.color Color.primary400
        , Font.center
        , Border.widthEach
            { top = 1
            , right = 0
            , bottom = 0
            , left = 0
            }
        , Border.color Color.transparent100
        ]
        (Input.button
            [ width shrink
            , height shrink
            , centerX
            , padding 0
            , Background.color Color.completelyTransparent
            , Border.rounded 8
            ]
            { onPress = Just GoToCustomERC20s
            , label =
                row
                    [ width fill
                    , height fill
                    , spacing 6
                    , centerX
                    ]
                    [ el
                        [ width shrink
                        , height shrink
                        , centerY
                        , padding 0
                        ]
                        ("Manage Token Lists" |> text)
                    , images
                        |> Image.arrow
                            [ width <| px 16
                            , height <| px 16
                            , centerY
                            ]
                    ]
            }
        )
