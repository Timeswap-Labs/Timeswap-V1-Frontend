module Modal.MaturityList.Main exposing
    ( Effect(..)
    , Modal
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Blockchain.Main as Blockchain exposing (Blockchain)
import Browser.Events
import Data.Backdrop exposing (Backdrop)
import Data.Chain exposing (Chain)
import Data.ChosenZone exposing (ChosenZone)
import Data.Device as Device exposing (Device)
import Data.Images exposing (Images)
import Data.Maturity as Maturity exposing (Maturity)
import Data.Offset exposing (Offset)
import Data.Pair as Pair exposing (Pair)
import Data.Pool exposing (Pool)
import Data.PriceFeed as PriceFeed exposing (PriceFeed)
import Data.Remote as Remote exposing (Remote(..))
import Data.Theme as Theme exposing (Theme)
import Data.Token as Token
import Data.Web as Web exposing (Web)
import Element
    exposing
        ( Element
        , alignRight
        , below
        , centerX
        , centerY
        , clipY
        , column
        , el
        , fill
        , fillPortion
        , height
        , inFront
        , maximum
        , minimum
        , mouseDown
        , mouseOver
        , moveDown
        , moveLeft
        , none
        , paddingEach
        , paddingXY
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
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Http
import Json.Decode as Decode exposing (Decoder)
import Modal.MaturityList.Answer as Answer exposing (Answer)
import Modal.MaturityList.Pools as Pools exposing (Pools)
import Modal.MaturityList.Query as Query
import Modal.MaturityList.Sorting as Sorting exposing (Sorting)
import Modal.MaturityList.Tooltip as Tooltip exposing (Tooltip)
import Modal.Outside as Outside
import Process
import Sort.Dict as Dict
import Task
import Time exposing (Posix)
import Utility.Calculate as Calculate
import Utility.Color as Color
import Utility.Duration as Duration
import Utility.Glass as Glass
import Utility.IconButton as IconButton
import Utility.Id as Id
import Utility.Image as Image
import Utility.Loading as Loading
import Utility.PairImage as PairImage
import Utility.ThemeColor as ThemeColor
import Utility.Tooltip as Tooltip


type Modal
    = Modal
        { pair : Pair
        , sorting : Sorting
        , dropdown : Dropdown
        , pools : Web Pools
        , tooltip : Maybe Tooltip
        }


type Dropdown
    = Open
    | Close


type Msg
    = GoToSortMaturity
    | GoToSortLiquidity
    | SelectMaturity Maturity
    | QueryAgain
    | ReceiveAnswer Chain Pair (Result Http.Error Answer)
    | OnMouseEnter Tooltip
    | OnMouseLeave
    | OpenDropdown
    | CloseDropdown
    | Tick Posix
    | Exit


type Effect
    = InputPool Pool


init :
    Blockchain
    -> Pair
    -> ( Modal, Cmd Msg )
init blockchain pair =
    ( { pair = pair
      , sorting = Sorting.Liquidity
      , dropdown = Close
      , pools = Remote.loading
      , tooltip = Nothing
      }
        |> Modal
    , get blockchain pair
    )


update :
    { model | time : Posix }
    -> Blockchain
    -> Msg
    -> Modal
    -> ( Maybe Modal, Cmd Msg, Maybe Effect )
update { time } blockchain msg (Modal modal) =
    case msg of
        GoToSortMaturity ->
            ( { modal
                | sorting = Sorting.Maturity
                , dropdown = Close
              }
                |> Modal
                |> Just
            , Cmd.none
            , Nothing
            )

        GoToSortLiquidity ->
            ( { modal
                | sorting = Sorting.Liquidity
                , dropdown = Close
              }
                |> Modal
                |> Just
            , Cmd.none
            , Nothing
            )

        SelectMaturity maturity ->
            case modal.pools of
                Success pools ->
                    if maturity |> Dict.memberOf pools then
                        ( Nothing
                        , Cmd.none
                        , InputPool
                            { pair = modal.pair
                            , maturity = maturity
                            }
                            |> Just
                        )

                    else
                        ( modal |> Modal |> Just
                        , Cmd.none
                        , Nothing
                        )

                _ ->
                    ( modal |> Modal |> Just
                    , Cmd.none
                    , Nothing
                    )

        QueryAgain ->
            ( modal |> Modal |> Just
            , get blockchain modal.pair
            , Nothing
            )

        ReceiveAnswer chain pair result ->
            if
                (chain == (blockchain |> Blockchain.toChain))
                    && (pair == modal.pair)
            then
                ( { modal
                    | pools =
                        result
                            |> Result.map
                                (\answer ->
                                    answer
                                        |> Dict.keepIf (\maturity _ -> maturity |> Maturity.isActive time)
                                )
                            |> Web.fromResult
                  }
                    |> Modal
                    |> Just
                , case result |> Web.fromResult of
                    Failure _ ->
                        Process.sleep 5000
                            |> Task.perform (\_ -> QueryAgain)

                    _ ->
                        Cmd.none
                , Nothing
                )

            else
                ( modal |> Modal |> Just
                , Cmd.none
                , Nothing
                )

        OnMouseEnter tooltip ->
            ( { modal | tooltip = Just tooltip }
                |> Modal
                |> Just
            , Cmd.none
            , Nothing
            )

        OnMouseLeave ->
            ( { modal | tooltip = Nothing }
                |> Modal
                |> Just
            , Cmd.none
            , Nothing
            )

        OpenDropdown ->
            ( { modal | dropdown = Open }
                |> Modal
                |> Just
            , Cmd.none
            , Nothing
            )

        CloseDropdown ->
            ( { modal | dropdown = Close }
                |> Modal
                |> Just
            , Cmd.none
            , Nothing
            )

        Tick posix ->
            ( { modal | pools = modal.pools |> Remote.update posix }
                |> Modal
                |> Just
            , Cmd.none
            , Nothing
            )

        Exit ->
            ( Nothing
            , Cmd.none
            , Nothing
            )


subscriptions : Modal -> Sub Msg
subscriptions ((Modal { pools }) as modal) =
    [ onClickOutsideDropdown modal
    , Remote.subscriptions Tick pools
    ]
        |> Sub.batch


onClickOutsideDropdown : Modal -> Sub Msg
onClickOutsideDropdown (Modal { dropdown }) =
    case dropdown of
        Open ->
            decoderOutsideDropdown
                |> Decode.at [ "target", "id" ]
                |> Browser.Events.onClick

        Close ->
            Sub.none


decoderOutsideDropdown : Decoder Msg
decoderOutsideDropdown =
    Decode.string
        |> Decode.andThen
            (\string ->
                if string /= "sort-dropdown" then
                    Decode.succeed CloseDropdown

                else
                    Decode.fail "Its the sort dropdown"
            )


get :
    Blockchain
    -> Pair
    -> Cmd Msg
get blockchain pair =
    blockchain
        |> Blockchain.toChain
        |> (\chain ->
                Http.get
                    { url = pair |> Query.toUrlString chain
                    , expect =
                        Answer.decoder
                            |> Http.expectJson (ReceiveAnswer chain pair)
                    }
           )


view :
    { model
        | time : Posix
        , offset : Offset
        , chosenZone : ChosenZone
        , backdrop : Backdrop
        , images : Images
        , device : Device
        , priceFeed : PriceFeed
        , theme : Theme
    }
    -> Modal
    -> Element Msg
view ({ backdrop, device, priceFeed, theme } as model) ((Modal { pair }) as modal) =
    Outside.view model
        { onClick = Exit
        , modal =
            column
                ([ if device |> Device.isPhoneOrTablet then
                    width <| minimum 375 shrink

                   else
                    width <| minimum 622 shrink
                 , height <| maximum 468 shrink
                 , spacing 16
                 , centerX
                 , centerY
                 , Border.rounded 8
                 , theme |> ThemeColor.border |> Border.color
                 , Border.width 1
                 ]
                    ++ Glass.background backdrop theme
                )
                [ column
                    [ width fill
                    , spacing 20
                    , paddingEach
                        { top = 24
                        , right = 24
                        , bottom = 0
                        , left = 24
                        }
                    ]
                    [ row
                        [ width fill
                        , height shrink
                        ]
                        [ el
                            [ width shrink
                            , height shrink
                            , centerY
                            , Font.size 18
                            , Font.bold
                            , paddingXY 0 3
                            , theme |> ThemeColor.text |> Font.color
                            ]
                            (text "Choose Maturity")
                        , IconButton.exit model Exit
                        ]
                    , row
                        [ width fill
                        , height shrink
                        , centerY
                        , spacing 8
                        ]
                        [ pairWithPoolCount model modal
                        , sortBy model modal
                        , sortInfo model modal
                        ]
                    ]
                , row
                    [ width fill
                    , height <| px 34
                    , centerY
                    , paddingXY 44 0
                    , spacing 16
                    , theme |> ThemeColor.tableHeaderBG |> Background.color
                    ]
                    [ el
                        [ centerX
                        , width <| fillPortion 2
                        , Font.size 12
                        , Font.bold
                        , Font.letterSpacing 0.08
                        , theme |> ThemeColor.placeholder |> Font.color
                        , Font.center
                        , moveLeft 16
                        ]
                        (text "MATURITY TIME")
                    , el
                        [ centerX
                        , width <| fillPortion 1
                        , Font.size 12
                        , Font.bold
                        , Font.letterSpacing 0.08
                        , theme |> ThemeColor.placeholder |> Font.color
                        , Font.center
                        ]
                        (text "MAX APR")
                    , el
                        [ centerX
                        , width <| fillPortion 1
                        , Font.size 12
                        , Font.bold
                        , Font.letterSpacing 0.08
                        , theme |> ThemeColor.placeholder |> Font.color
                        , Font.center
                        ]
                        (case priceFeed of
                            PriceFeed.Ignore ->
                                [ "CDP - "
                                , pair |> Pair.toCollateral |> Token.toSymbol
                                , " PER "
                                , pair |> Pair.toAsset |> Token.toSymbol
                                ]
                                    |> String.concat
                                    |> text

                            PriceFeed.Utilize ->
                                text "MIN CDP"
                        )
                    ]
                , maturityList model modal
                ]
        }


pairWithPoolCount :
    { model
        | time : Posix
        , images : Images
        , theme : Theme
    }
    -> Modal
    -> Element Msg
pairWithPoolCount { images, theme } (Modal { pair, pools }) =
    let
        asset =
            pair |> Pair.toAsset

        collateral =
            pair |> Pair.toCollateral
    in
    row
        [ width fill
        , height shrink
        , spacing 8
        ]
        [ images
            |> PairImage.view
                { pair = pair
                , length = 32
                }
        , el
            [ width shrink
            , height shrink
            , centerY
            , Font.size 16
            , paddingXY 0 3
            , theme |> ThemeColor.text |> Font.color
            ]
            (collateral
                |> Token.toSymbol
                |> String.append "/"
                |> String.append (asset |> Token.toSymbol)
                |> text
            )
        , el
            [ width shrink
            , height shrink
            , centerY
            , Font.size 14
            , paddingXY 0 3
            , theme |> ThemeColor.textLight |> Font.color
            ]
            (case pools of
                Success poolsDict ->
                    [ "("
                    , poolsDict
                        |> Dict.toList
                        |> List.length
                        |> String.fromInt
                    , if (poolsDict |> Dict.toList |> List.length) == 1 then
                        " Pool)"

                      else
                        " Pools)"
                    ]
                        |> String.concat
                        |> text

                _ ->
                    none
            )
        ]


sortBy :
    { model | images : Images, theme : Theme }
    -> Modal
    -> Element Msg
sortBy { images, theme } (Modal { sorting, dropdown }) =
    row
        [ alignRight
        , spacing 14
        , centerY
        ]
        [ el
            [ theme |> ThemeColor.actionElemLabel |> Font.color
            , Font.size 14
            ]
            (text "Sort by")
        , Input.button
            [ Region.description "sort by"
            , width <| px 130
            , height <| px 38
            , theme |> ThemeColor.btnBackground |> Background.color
            , Border.width 1
            , theme |> ThemeColor.border |> Border.color
            , Border.rounded 8
            , (if dropdown == Open then
                [ Sorting.Liquidity
                , Sorting.Maturity
                ]
                    |> sortOptionsEl theme

               else
                none
              )
                |> below
            ]
            { onPress =
                if dropdown == Open then
                    Nothing

                else
                    Just OpenDropdown
            , label =
                row
                    [ width fill
                    , height fill
                    , paddingXY 12 0
                    , spacing 6
                    , theme |> ThemeColor.text |> Font.color
                    , Font.size 14
                    ]
                    [ sorting |> Sorting.toString |> text
                    , images
                        |> (case theme of
                                Theme.Dark ->
                                    Image.discloser

                                Theme.Light ->
                                    Image.arrowDownDark
                           )
                            [ width <| px 11
                            , height <| px 7
                            , alignRight
                            , centerY
                            ]
                    ]
            }
        ]


sortInfo : { model | images : Images, theme : Theme } -> Modal -> Element Msg
sortInfo { images, theme } (Modal { tooltip }) =
    images
        |> (case theme of
                Theme.Dark ->
                    Image.info

                Theme.Light ->
                    Image.infoDark
           )
            [ width <| px 20
            , height <| px 20
            , Events.onMouseEnter (OnMouseEnter Tooltip.SortInfo)
            , Events.onMouseLeave OnMouseLeave
            , (if tooltip == Just Tooltip.SortInfo then
                el
                    [ Font.size 14
                    , theme |> ThemeColor.textLight |> Font.color
                    ]
                    ("Liquidity-sorting is based on the size of the product of X, Y, Z pools" |> text)
                    |> Tooltip.belowAlignRight theme

               else
                none
              )
                |> below
            ]


sortOptionsEl : Theme -> List Sorting -> Element Msg
sortOptionsEl theme sortList =
    column
        [ width <| px 130
        , height shrink
        , moveDown 10
        , theme |> ThemeColor.dropdownBG |> Background.color
        , Border.rounded 4
        , Border.width 1
        , theme |> ThemeColor.border |> Border.color
        ]
        (sortList
            |> List.map
                (\sortOption ->
                    Input.button
                        [ width fill
                        , height shrink
                        , paddingXY 12 10
                        , theme |> ThemeColor.text |> Font.color
                        , Font.size 14
                        , mouseOver [ theme |> ThemeColor.btnBackground |> Background.color ]
                        , el
                            [ width fill
                            , height fill
                            , Id.is "sort-dropdown"
                            ]
                            none
                            |> inFront
                        ]
                        { onPress =
                            case sortOption of
                                Sorting.Liquidity ->
                                    Just GoToSortLiquidity

                                Sorting.Maturity ->
                                    Just GoToSortMaturity
                        , label =
                            sortOption
                                |> Sorting.toString
                                |> text
                        }
                )
        )


maturityList :
    { model
        | images : Images
        , time : Posix
        , offset : Offset
        , chosenZone : ChosenZone
        , priceFeed : PriceFeed
        , theme : Theme
    }
    -> Modal
    -> Element Msg
maturityList { images, time, offset, chosenZone, priceFeed, theme } (Modal { pair, pools, tooltip, sorting }) =
    column
        [ width fill
        , height <| minimum 295 shrink
        , paddingEach
            { top = 0
            , right = 24
            , bottom = 16
            , left = 24
            }
        , spacing 12
        , clipY
        , scrollbarY
        ]
        (case pools of
            Loading timeline ->
                [ el [ centerX, centerY ] (Loading.view timeline theme) ]

            Failure _ ->
                [ el [ centerX, centerY, Font.color Color.negative400 ] (text "Error") ]

            Success poolsDict ->
                if Dict.size poolsDict > 0 then
                    poolsDict
                        |> Dict.toList
                        |> List.sortWith
                            (case sorting of
                                Sorting.Maturity ->
                                    Pools.compareMaturity

                                Sorting.Liquidity ->
                                    Pools.compareRank
                            )
                        |> List.map
                            (\( maturity, summary ) ->
                                Input.button [ width fill ]
                                    { onPress = Just (SelectMaturity maturity)
                                    , label =
                                        row
                                            [ width fill
                                            , height <| px 62
                                            , paddingXY 20 12
                                            , spacing 16
                                            , centerY
                                            , theme |> ThemeColor.sectionBackground |> Background.color
                                            , Border.rounded 8
                                            , mouseDown [ theme |> ThemeColor.btnPressBG |> Background.color ]
                                            , mouseOver [ theme |> ThemeColor.border |> Background.color ]
                                            ]
                                            [ row [ width <| fillPortion 2, spacing 16 ]
                                                [ images
                                                    |> (case theme of
                                                            Theme.Dark ->
                                                                Image.hourglassPrimary

                                                            Theme.Light ->
                                                                Image.hourglassDark
                                                       )
                                                        [ width <| px 16
                                                        , height <| px 22
                                                        ]
                                                , column
                                                    []
                                                    [ Duration.viewMaturity
                                                        { onMouseEnter = OnMouseEnter
                                                        , onMouseLeave = OnMouseLeave
                                                        , tooltip = Tooltip.Maturity maturity
                                                        , opened = tooltip
                                                        , time = time
                                                        , offset = offset
                                                        , chosenZone = chosenZone
                                                        , maturity = maturity
                                                        , theme = theme
                                                        }
                                                    ]
                                                ]
                                            , el
                                                [ width <| fillPortion 1
                                                , centerX
                                                , centerY
                                                , paddingXY 0 3
                                                , Font.size 14
                                                , Font.bold
                                                , Font.center
                                                , Font.color Color.positive400
                                                ]
                                                (Calculate.apr summary.apr)
                                            , el
                                                [ width <| fillPortion 1
                                                , centerX
                                                , centerY
                                                , paddingXY 0 3
                                                , Font.size 14
                                                , Font.bold
                                                , Font.center
                                                , theme |> ThemeColor.text |> Font.color
                                                ]
                                                (Calculate.cdp
                                                    { onMouseEnter = OnMouseEnter
                                                    , onMouseLeave = OnMouseLeave
                                                    , cdpTooltip = Tooltip.CDP maturity
                                                    , opened = tooltip
                                                    , pair = pair
                                                    , cdp = summary.cdp
                                                    , theme = theme
                                                    }
                                                    priceFeed
                                                    (theme |> ThemeColor.text)
                                                    14
                                                )
                                            ]
                                    }
                            )

                else
                    [ el
                        [ width fill
                        , centerX
                        , Font.center
                        , Font.size 16
                        , paddingXY 10 20
                        , theme |> ThemeColor.text |> Font.color
                        ]
                        (text "No pools available")
                    ]
        )
