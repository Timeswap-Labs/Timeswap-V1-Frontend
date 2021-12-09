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
import Data.Chains as Chains exposing (Chains)
import Data.ERC20 as ERC20 exposing (ERC20)
import Data.Images exposing (Images)
import Data.Remote as Remote exposing (Remote(..))
import Data.Token as Token exposing (Token(..))
import Data.TokenParam exposing (TokenParam)
import Data.Uint as Uint exposing (Uint)
import Data.Web exposing (Web)
import Data.WebError as WebError
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
        , minimum
        , none
        , padding
        , paddingEach
        , paddingXY
        , px
        , rotate
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
import Json.Decode
import Modal.TokenList.Answer as Answer exposing (Answer)
import Modal.TokenList.Error as Error exposing (Error)
import Modal.TokenList.Query as Query
import Modal.TokenList.Tooltip as Tooltip exposing (Tooltip)
import Process
import Sort.Set as Set exposing (Set)
import String
import Task
import Utility.Color as Color
import Utility.Glass as Glass
import Utility.Image as Image
import Utility.Truncate as Truncate


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
    | ReceiveAnswer (Result Http.Error Answer)
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
            ( error
                |> WebError.fromHttpError
                |> Maybe.map
                    (\webError ->
                        { modal
                            | state =
                                { allTokens
                                    | erc20 = Failure webError
                                }
                                    |> AllTokens
                        }
                    )
                |> Maybe.withDefault modal
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
view ({ backdrop, images, chains } as model) blockchain ((Modal { state }) as modal) =
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
            , tokenList model blockchain modal
            , case state of
                AllTokens a ->
                    manageTokensBtn model

                _ ->
                    none
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
                (case state of
                    AllTokens a ->
                        text "Select Token"

                    _ ->
                        row
                            [ width fill
                            , spacing 12
                            ]
                            [ Input.button
                                [ width shrink
                                , alignRight
                                ]
                                { onPress = Just GoToAllTokens
                                , label =
                                    images
                                        |> Image.arrowDown
                                            [ width <| px 16
                                            , height <| px 15
                                            , rotate (pi / 2)
                                            ]
                                }
                            , text "Manage Tokens"
                            ]
                )
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
    -> Modal
    -> Element Msg
tokenList ({ backdrop, chains } as model) blockchain ((Modal { state }) as modal) =
    column
        [ width fill
        , height (shrink |> minimum 250)
        , paddingXY 24 6
        ]
        (case state of
            AllTokens { erc20 } ->
                case erc20 of
                    Success (Ok erc20Value) ->
                        if erc20Value |> ERC20.toAddress |> Chains.isMemberOf (blockchain |> Blockchain.toChain) chains then
                            [ tokenButton model blockchain modal (Token.ERC20 erc20Value) ]

                        else
                            [ customToken model blockchain modal (Token.ERC20 erc20Value) ]

                    _ ->
                        chains
                            |> Chains.toList (blockchain |> Blockchain.toChain)
                            |> List.map (tokenButton model blockchain modal)

            CustomERC20s ->
                chains
                    |> Chains.toCustomTokenList (blockchain |> Blockchain.toChain)
                    |> List.map (customToken model blockchain modal)

            _ ->
                [ none ]
        )


tokenButton :
    { model | images : Images }
    -> Blockchain
    -> Modal
    -> Token
    -> Element Msg
tokenButton ({ images } as model) blockchain modal token =
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
                [ tokenSymbolAndName model modal token
                , case blockchain |> Blockchain.toUser of
                    Just user ->
                        tokenBalance user token modal

                    _ ->
                        none
                ]
        }


customToken :
    { model | images : Images }
    -> Blockchain
    -> Modal
    -> Token
    -> Element Msg
customToken model blockchain ((Modal { state }) as modal) token =
    row
        [ width fill
        , height shrink
        , paddingXY 0 10
        , Font.color Color.transparent500
        ]
        [ tokenSymbolAndName model modal token
        , case state of
            ImportingERC20 erc20 ->
                row
                    [ width shrink
                    , alignRight
                    , spacing 15
                    , Font.regular
                    , Font.size 16
                    ]
                    [ Input.button
                        []
                        { onPress = Nothing
                        , label =
                            model.images
                                |> Image.minus
                                    [ width <| px 16
                                    , height <| px 16
                                    , alpha 0.64
                                    , Font.color Color.negative300
                                    ]
                        }
                    , Input.button
                        []
                        { onPress = Nothing
                        , label =
                            model.images
                                |> Image.link
                                    [ width <| px 16
                                    , height <| px 16
                                    ]
                        }
                    ]

            _ ->
                Input.button
                    [ width shrink
                    , height shrink
                    , Font.size 12
                    , Font.bold
                    , paddingXY 9 4
                    ]
                    { onPress = Nothing
                    , label =
                        el [ centerX, centerY ] (text "Import")
                    }
        ]


tokenSymbolAndName :
    { model | images : Images }
    -> Modal
    -> Token
    -> Element Msg
tokenSymbolAndName { images } (Modal { tooltip }) token =
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
            (Truncate.viewSymbol
                { onMouseEnter = OnMouseEnter
                , onMouseLeave = OnMouseLeave
                , tooltip = Tooltip.Symbol token
                , opened = tooltip
                , token = token
                }
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
            (Truncate.viewName
                { onMouseEnter = OnMouseEnter
                , onMouseLeave = OnMouseLeave
                , tooltip = Tooltip.Name token
                , opened = tooltip
                , token = token
                }
            )
        ]


tokenBalance : User -> Token -> Modal -> Element Msg
tokenBalance user token (Modal { tooltip }) =
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
                (Truncate.viewAmount
                    { onMouseEnter = OnMouseEnter
                    , onMouseLeave = OnMouseLeave
                    , tooltip = Tooltip.Amount token
                    , opened = tooltip
                    , token = token
                    , amount = balance
                    }
                )

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



-- importTokenWarning : { model | images : Images } -> Token -> Tooltip -> Element Msg
-- importTokenWarning model token tooltip =
--     column
--         [ width fill
--         , height shrink
--         , padding 24
--         , spacing 24
--         , centerX
--         ]
--         [ column
--             [ width fill
--             , height shrink
--             , padding 16
--             , spacing 12
--             , centerX
--             , Background.color Color.primary100
--             , Border.rounded 8
--             ]
--             [ images
--                 |> Image.info
--                     [ width <| px 30
--                     , height <| px 30
--                     , centerY
--                     ]
--             , text "You’re importing this token on your own risk. make sure you want to trade this token"
--             ]
--         , column
--             [ spacing 4 ]
--             [ images
--                 |> Image.viewToken
--                     [ width <| px 24
--                     , alignLeft
--                     , centerY
--                     ]
--                     token
--             , el
--                 [ alignLeft
--                 , centerY
--                 , paddingEach
--                     { top = 0
--                     , right = 0
--                     , bottom = 0
--                     , left = 8
--                     }
--                 , Font.size 16
--                 , Font.bold
--                 ]
--                 (Truncate.viewSymbol
--                     { onMouseEnter = OnMouseEnter
--                     , onMouseLeave = OnMouseLeave
--                     , tooltip = Tooltip.Symbol token
--                     , opened = tooltip
--                     , token = token
--                     }
--                 )
--             , el
--                 [ height <| px 23
--                 , paddingXY 5 0
--                 , Font.size 22
--                 , Font.color Color.transparent300
--                 ]
--                 (text (String.fromChar (Char.fromCode 0x2022)))
--             , el
--                 [ Font.size 12
--                 , Font.color Color.transparent300
--                 , Font.regular
--                 ]
--                 (Truncate.viewName
--                     { onMouseEnter = OnMouseEnter
--                     , onMouseLeave = OnMouseLeave
--                     , tooltip = Tooltip.Name token
--                     , opened = tooltip
--                     , token = token
--                     }
--                 )
--             ]
--         , Input.button [ width fill, height <| px 44, Background.color Color.primary500 ]
--             { onPress = Nothing
--             , label = text "Import"
--             }
--         ]
