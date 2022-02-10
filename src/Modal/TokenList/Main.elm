module Modal.TokenList.Main exposing
    ( Effect(..)
    , Modal
    , Msg
    , init
    , update
    , view
    )

import Blockchain.Main as Blockchain exposing (Blockchain)
import Blockchain.User.Main as User exposing (User)
import Data.Address as Address exposing (Address)
import Data.Backdrop exposing (Backdrop)
import Data.Chain exposing (Chain)
import Data.Chains as Chains exposing (Chains)
import Data.ERC20 as ERC20 exposing (ERC20)
import Data.Images exposing (Images)
import Data.Remote as Remote exposing (Remote(..))
import Data.Theme as Theme exposing (Theme)
import Data.Token as Token exposing (Token)
import Data.TokenParam exposing (TokenParam)
import Data.Web as Web exposing (Web)
import Element
    exposing
        ( Element
        , alignLeft
        , alignRight
        , alpha
        , centerX
        , centerY
        , clip
        , column
        , el
        , fill
        , height
        , htmlAttribute
        , minimum
        , mouseDown
        , mouseOver
        , newTabLink
        , none
        , padding
        , paddingEach
        , paddingXY
        , paragraph
        , px
        , row
        , scrollbarY
        , shrink
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Http
import Modal.Outside as Outside
import Modal.TokenList.Answer as Answer exposing (Answer)
import Modal.TokenList.Error as Error exposing (Error)
import Modal.TokenList.Query as Query
import Modal.TokenList.Tooltip as Tooltip exposing (Tooltip)
import Process
import Task
import Utility.BlockExplorer as BlockExplorer
import Utility.Color as Color
import Utility.Glass as Glass
import Utility.IconButton as IconButton
import Utility.Image as Image
import Utility.Loading as Loading
import Utility.ThemeColor as ThemeColor
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
        , erc20 : Maybe (Web (Result Error ERC20))
        }
    | CustomERC20s { input : String }
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
        , erc20 = Nothing
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
            ( { modal | state = CustomERC20s { input = "" } }
                |> Modal
                |> Just
            , Cmd.none
            , Nothing
            )

        ( GoToAllTokens, CustomERC20s _ ) ->
            ( { modal
                | state =
                    { input = ""
                    , erc20 = Nothing
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
                    , erc20 = Nothing
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
                    , erc20 = Nothing
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
                            if address |> Chains.isMemberOf (blockchain |> Blockchain.toChain) chains then
                                ( { modal
                                    | state =
                                        { input = string
                                        , erc20 = Nothing
                                        }
                                            |> AllTokens
                                  }
                                    |> Modal
                                    |> Just
                                , Cmd.none
                                , Nothing
                                )

                            else
                                ( { modal
                                    | state =
                                        { input = string
                                        , erc20 = Just Remote.loading
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
                                , erc20 = Nothing
                                }
                                    |> AllTokens
                          }
                            |> Modal
                            |> Just
                        , Cmd.none
                        , Nothing
                        )

        ( InputAddress string, CustomERC20s { input } ) ->
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
                            if address |> Chains.isMemberOf (blockchain |> Blockchain.toChain) chains then
                                ( { modal
                                    | state =
                                        { input = string
                                        }
                                            |> CustomERC20s
                                  }
                                    |> Modal
                                    |> Just
                                , Cmd.none
                                , Nothing
                                )

                            else
                                ( { modal
                                    | state =
                                        { input = string
                                        }
                                            |> CustomERC20s
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
                                { input = string } |> CustomERC20s
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
                ( Just address, Just (Failure _) ) ->
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
                                                |> Just
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
                                    , erc20 = Just Remote.loading
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
                            , erc20 = Nothing
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
                                    |> Just
                        }
                            |> AllTokens
                }

               else
                modal
              )
                |> Modal
                |> Just
            , case allTokens.erc20 of
                Just (Failure _) ->
                    Process.sleep 5000
                        |> Task.perform (\_ -> QueryAgain)

                _ ->
                    Cmd.none
            , Nothing
            )

        ( ChooseToken token, AllTokens _ ) ->
            ( Nothing
            , Cmd.none
            , InputToken modal.tokenParam token |> Just
            )

        ( GoToImportERC20, AllTokens { erc20 } ) ->
            case erc20 of
                Just (Success (Ok erc20Value)) ->
                    ( { modal | state = ImportingERC20 erc20Value }
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

        ( ImportERC20, ImportingERC20 erc20 ) ->
            ( Nothing
            , Cmd.none
            , AddERC20 modal.tokenParam erc20 |> Just
            )

        ( ClearERC20 erc20, CustomERC20s _ ) ->
            ( modal |> Modal |> Just
            , Cmd.none
            , RemoveERC20 erc20 |> Just
            )

        ( ClearAll, CustomERC20s _ ) ->
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
    { model | backdrop : Backdrop, images : Images, chains : Chains, theme : Theme }
    -> Blockchain
    -> Modal
    -> Element Msg
view ({ backdrop } as model) blockchain ((Modal { state, tooltip }) as modal) =
    Outside.view model
        { onClick = Exit
        , modal =
            column
                ([ width <| px 375
                 , height <| minimum 360 shrink
                 , centerX
                 , centerY
                 , model.theme |> ThemeColor.border |> Border.color
                 , Border.width 1
                 , Border.rounded 8
                 ]
                    ++ Glass.background backdrop model.theme
                )
                [ modalHeader model modal
                , case state of
                    ImportingERC20 erc20 ->
                        importTokenWarning model erc20 tooltip

                    _ ->
                        tokenList model blockchain modal
                , case state of
                    AllTokens a ->
                        manageTokensBtn model

                    _ ->
                        none
                ]
        }


modalHeader : { model | images : Images, theme : Theme } -> Modal -> Element Msg
modalHeader model (Modal { state }) =
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
        , model.theme |> ThemeColor.text |> Font.color
        , Font.bold
        , Font.size 18
        , model.theme |> ThemeColor.textboxBorder |> Border.color
        , Border.widthEach
            { top = 0
            , right = 0
            , bottom = 1
            , left = 0
            }
        ]
        [ row
            [ width fill ]
            [ row
                [ alignLeft
                , spacing 12
                ]
                (case state of
                    AllTokens a ->
                        [ text "Select Token" ]

                    ImportingERC20 a ->
                        [ IconButton.back model GoToAllTokens
                        , text "Import Token"
                        ]

                    CustomERC20s a ->
                        [ IconButton.back model GoToAllTokens
                        , text "Manage Tokens"
                        ]
                )
            , IconButton.exit model Exit
            ]
        , case state of
            ImportingERC20 a ->
                none

            _ ->
                Input.text
                    [ width fill
                    , height fill
                    , padding 14
                    , Font.size 16
                    , Font.regular
                    , Background.color Color.completelyTransparent
                    , Border.width 1
                    , model.theme |> ThemeColor.textboxBorder |> Border.color
                    , Border.rounded 8
                    ]
                    { onChange = InputAddress
                    , text =
                        case state of
                            AllTokens { input } ->
                                input

                            CustomERC20s { input } ->
                                input

                            _ ->
                                ""
                    , placeholder =
                        Input.placeholder
                            [ model.theme |> ThemeColor.textLight |> Font.color
                            , Font.size 14
                            , Font.regular
                            ]
                            (text "Search a token or paste address")
                            |> Just
                    , label = Input.labelHidden "Search token"
                    }
        ]


tokenList :
    { model | backdrop : Backdrop, images : Images, chains : Chains, theme : Theme }
    -> Blockchain
    -> Modal
    -> Element Msg
tokenList ({ chains } as model) blockchain ((Modal { state }) as modal) =
    column
        [ width fill
        , height <| minimum 278 shrink
        , paddingXY 12 10
        , spacing 0
        , clip
        , scrollbarY
        ]
        (case state of
            AllTokens { input, erc20 } ->
                case erc20 of
                    Just (Success (Ok erc20Value)) ->
                        [ customToken model blockchain modal (Token.ERC20 erc20Value) ]

                    Just (Loading timeline) ->
                        [ el [ width fill, centerX, padding 10 ] (Loading.view timeline) ]

                    _ ->
                        input
                            |> Address.fromString
                            |> Maybe.map
                                (\address ->
                                    chains
                                        |> Chains.getGivenAddress
                                            (blockchain |> Blockchain.toChain)
                                            address
                                        |> Maybe.map
                                            (\erc20Found ->
                                                [ tokenButton model blockchain modal (Token.ERC20 erc20Found) ]
                                            )
                                        |> Maybe.withDefault
                                            [ noResults model ]
                                )
                            |> Maybe.withDefault
                                (chains
                                    |> Chains.toTokenList (blockchain |> Blockchain.toChain)
                                    |> List.filter (\token -> input |> Token.containsString token)
                                    |> (\list ->
                                            if (list |> List.length) > 0 then
                                                List.map (tokenButton model blockchain modal) list

                                            else
                                                [ noResults model ]
                                       )
                                )

            CustomERC20s { input } ->
                let
                    tokens =
                        chains |> Chains.toCustomTokenList (blockchain |> Blockchain.toChain)
                in
                input
                    |> Address.fromString
                    |> Maybe.map
                        (\address ->
                            tokens
                                |> List.filter (\token -> (token |> Token.toString) == (address |> Address.toString))
                                |> (\list ->
                                        if (list |> List.length) > 0 then
                                            List.map (customToken model blockchain modal) list

                                        else
                                            [ noResults model ]
                                   )
                        )
                    |> Maybe.withDefault
                        (tokens
                            |> List.filter (\token -> input |> Token.containsString token)
                            |> (\list ->
                                    if (list |> List.length) > 0 then
                                        List.map (customToken model blockchain modal) list

                                    else
                                        [ noResults model ]
                               )
                        )

            _ ->
                [ none ]
        )


tokenButton :
    { model | images : Images, theme : Theme }
    -> Blockchain
    -> Modal
    -> Token
    -> Element Msg
tokenButton model blockchain modal token =
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
                , centerY
                , paddingXY 12 10
                , mouseDown [ model.theme |> ThemeColor.btnHoverBG |> Background.color ]
                , mouseOver [ model.theme |> ThemeColor.btnBackground |> Background.color ]
                , Border.rounded 8
                ]
                [ tokenSymbolAndName model modal token
                , case blockchain |> Blockchain.toUser of
                    Just user ->
                        tokenBalance user token modal model.theme

                    _ ->
                        none
                ]
        }


customToken :
    { model | images : Images, theme : Theme }
    -> Blockchain
    -> Modal
    -> Token
    -> Element Msg
customToken model blockchain ((Modal { state }) as modal) token =
    row
        [ width fill
        , height shrink
        , paddingXY 12 10
        , Font.color Color.transparent500
        ]
        [ tokenSymbolAndName model modal token
        , case state of
            CustomERC20s input ->
                row
                    [ width shrink
                    , alignRight
                    , spacing 15
                    , Font.regular
                    , Font.size 16
                    ]
                    [ Input.button
                        []
                        { onPress =
                            case token of
                                Token.ERC20 erc20 ->
                                    Just (ClearERC20 erc20)

                                _ ->
                                    Nothing
                        , label =
                            model.images
                                |> (case model.theme of
                                        Theme.Dark ->
                                            Image.minus

                                        Theme.Light ->
                                            Image.minusDark
                                   )
                                    [ width <| px 16
                                    , height <| px 16
                                    , alpha 0.64
                                    , Font.color Color.negative300
                                    ]
                        }
                    , newTabLink
                        [ alpha 0.64 ]
                        { url = token |> BlockExplorer.fromToken (blockchain |> Blockchain.toChain)
                        , label =
                            model.images
                                |> (case model.theme of
                                        Theme.Dark ->
                                            Image.linkWhite

                                        Theme.Light ->
                                            Image.linkDark
                                   )
                                    [ width <| px 16
                                    , height <| px 16
                                    ]
                        }
                    ]

            AllTokens a ->
                Input.button
                    [ width shrink
                    , height <| px 24
                    , Font.size 12
                    , Font.regular
                    , Font.letterSpacing 0.5
                    , paddingXY 9 4
                    , model.theme |> ThemeColor.primaryBtn |> Background.color
                    , Border.rounded 4
                    ]
                    { onPress = Just GoToImportERC20
                    , label =
                        el [ centerX, centerY ] (text "Import")
                    }

            _ ->
                none
        ]


tokenSymbolAndName :
    { model | images : Images, theme : Theme }
    -> Modal
    -> Token
    -> Element Msg
tokenSymbolAndName { images, theme } (Modal { tooltip }) token =
    row
        [ width fill
        , height fill
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
                , theme = theme
                , customStyles = []
                }
            )
        , el
            [ height <| px 23
            , paddingXY 5 0
            , Font.size 22
            , theme |> ThemeColor.textLight |> Font.color
            ]
            (Char.fromCode 0x2022
                |> String.fromChar
                |> text
            )
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
                , theme = theme
                }
            )
        ]


tokenBalance : User -> Token -> Modal -> Theme -> Element Msg
tokenBalance user token (Modal { tooltip }) theme =
    let
        balances =
            user |> User.getBalance token
    in
    case balances of
        Just (Success balance) ->
            el
                [ alignRight
                , Font.regular
                , Font.size 16
                ]
                (Truncate.viewAmount
                    { onMouseEnter = OnMouseEnter
                    , onMouseLeave = OnMouseLeave
                    , tooltip = Tooltip.Name token
                    , opened = tooltip
                    , token = token
                    , amount = balance
                    , theme = theme
                    , customStyles = []
                    }
                )

        Just (Loading timeline) ->
            el
                [ width shrink
                , height shrink
                , alignRight
                , centerY
                ]
                (Loading.view timeline)

        Just (Failure error) ->
            el
                [ alignRight
                , Font.regular
                , Font.size 16
                ]
                (text "error")

        -- |> Debug.log "implement error view"
        _ ->
            none


noResults : { model | images : Images, theme : Theme } -> Element Msg
noResults { images, theme } =
    column
        [ width fill
        , height fill
        , centerY
        , centerX
        , Font.center
        , paddingEach
            { top = 0
            , right = 0
            , bottom = 16
            , left = 0
            }
        , spacing 24
        , htmlAttribute (Html.Attributes.style "align-items" "center")
        , htmlAttribute (Html.Attributes.style "justify-content" "center")
        ]
        [ images |> Image.warningCircle [ width <| px 30, height <| px 30, Font.center ]
        , el
            [ Font.size 14
            , Font.center
            , theme |> ThemeColor.text |> Font.color
            , width fill
            ]
            ("No results found" |> text)
        ]


manageTokensBtn : { model | images : Images, theme : Theme } -> Element Msg
manageTokensBtn { images, theme } =
    el
        [ width fill
        , height <| px 52
        , spacing 0
        , centerX
        , centerY
        , Font.size 14
        , Font.regular
        , theme |> ThemeColor.actionElemLabel |> Font.color
        , Font.center
        , Border.widthEach
            { top = 1
            , right = 0
            , bottom = 0
            , left = 0
            }
        , theme |> ThemeColor.textboxBorder |> Border.color
        ]
        (Input.button
            [ width shrink
            , height shrink
            , centerX
            , centerY
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
                        ("Manage Custom Tokens" |> text)
                    , images
                        |> (case theme of
                                Theme.Dark ->
                                    Image.arrow

                                Theme.Light ->
                                    Image.arrowSecondary
                           )
                            [ width <| px 16
                            , height <| px 16
                            , centerY
                            ]
                    ]
            }
        )


importTokenWarning :
    { model | images : Images, theme : Theme }
    -> ERC20
    -> Maybe Tooltip
    -> Element Msg
importTokenWarning { images, theme } erc20 tooltip =
    column
        [ width fill
        , height shrink
        , padding 24
        , spacing 24
        , centerX
        ]
        [ column
            [ width fill
            , height shrink
            , padding 16
            , spacing 12
            , centerX
            , Font.size 14
            , theme |> ThemeColor.text |> Font.color
            , theme |> ThemeColor.sectionBackground |> Background.color
            , Border.rounded 8
            ]
            [ images
                |> Image.warningCircle
                    [ width <| px 30
                    , height <| px 30
                    , centerX
                    , centerY
                    ]
            , paragraph
                [ width shrink
                , Font.center
                ]
                [ text "You're importing this token at your own risk. Make sure you want to use this token" ]
            ]
        , column
            [ width fill, spacing 4 ]
            [ images
                |> Image.viewToken
                    [ width <| px 36
                    , height <| px 36
                    , alignLeft
                    , centerX
                    , centerY
                    ]
                    (erc20 |> Token.ERC20)
            , el
                [ alignLeft
                , centerX
                , centerY
                , Font.size 16
                , Font.bold
                ]
                (Truncate.viewSymbol
                    { onMouseEnter = OnMouseEnter
                    , onMouseLeave = OnMouseLeave
                    , tooltip = Tooltip.Symbol (erc20 |> Token.ERC20)
                    , opened = tooltip
                    , token = erc20 |> Token.ERC20
                    , theme = theme
                    , customStyles = []
                    }
                )
            , el
                [ centerX
                , Font.size 12
                , Font.color Color.transparent300
                , Font.regular
                ]
                (Truncate.viewName
                    { onMouseEnter = OnMouseEnter
                    , onMouseLeave = OnMouseLeave
                    , tooltip = Tooltip.Name (erc20 |> Token.ERC20)
                    , opened = tooltip
                    , token = erc20 |> Token.ERC20
                    , theme = theme
                    }
                )
            , el
                [ centerX
                , Font.size 12
                , theme |> ThemeColor.actionElemLabel |> Font.color
                , Font.regular
                , paddingEach
                    { top = 4
                    , right = 0
                    , bottom = 0
                    , left = 0
                    }
                ]
                (erc20
                    |> ERC20.toAddress
                    |> Address.toString
                    |> text
                )
            ]
        , Input.button
            [ width fill
            , height <| px 44
            , centerX
            , Font.center
            , padding 10
            , Font.size 16
            , Font.color Color.light100
            , theme |> ThemeColor.primaryBtn |> Background.color
            , Border.rounded 4
            ]
            { onPress = Just ImportERC20
            , label = text "Import"
            }
        ]
