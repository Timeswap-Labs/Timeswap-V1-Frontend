module Modal.MaturityList.Main exposing
    ( Effect(..)
    , Modal
    , Msg
    , init
    , update
    , view
    )

import Blockchain.Main as Blockchain exposing (Blockchain)
import Data.Backdrop exposing (Backdrop)
import Data.Chain exposing (Chain)
import Data.Chains exposing (Chains)
import Data.ChosenZone exposing (ChosenZone)
import Data.Device as Device exposing (Device)
import Data.Images exposing (Images)
import Data.Maturity exposing (Maturity)
import Data.Offset exposing (Offset)
import Data.Pair as Pair exposing (Pair)
import Data.Pool exposing (Pool)
import Data.PriceFeed exposing (PriceFeed)
import Data.Remote exposing (Remote(..))
import Data.Token as Token
import Data.Web as Web exposing (Web)
import Element
    exposing
        ( Element
        , alignRight
        , below
        , centerX
        , centerY
        , column
        , el
        , fill
        , fillPortion
        , height
        , maximum
        , minimum
        , mouseDown
        , mouseOver
        , moveDown
        , moveLeft
        , moveRight
        , none
        , paddingEach
        , paddingXY
        , px
        , row
        , shrink
        , spacing
        , table
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html.Attributes
import Http
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
import Utility.Image as Image


type Modal
    = Modal
        { pair : Pair
        , sorting : Sorting
        , sortDropdown : Maybe ()
        , pools : Web Pools
        , tooltip : Maybe Tooltip
        }


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
      , sortDropdown = Nothing
      , pools = Success Pools.dummy
      , tooltip = Nothing
      }
        |> Modal
      -- , get blockchain pair
    , Cmd.none
    )


update :
    Blockchain
    -> Msg
    -> Modal
    -> ( Maybe Modal, Cmd Msg, Maybe Effect )
update blockchain msg (Modal modal) =
    case ( msg, modal.sorting ) of
        ( GoToSortMaturity, Sorting.Liquidity ) ->
            ( { modal
                | sorting = Sorting.Maturity
                , sortDropdown = Nothing
              }
                |> Modal
                |> Just
            , Cmd.none
            , Nothing
            )

        ( GoToSortLiquidity, Sorting.Maturity ) ->
            ( { modal
                | sorting = Sorting.Liquidity
                , sortDropdown = Nothing
              }
                |> Modal
                |> Just
            , Cmd.none
            , Nothing
            )

        ( SelectMaturity maturity, _ ) ->
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

        ( QueryAgain, _ ) ->
            ( modal |> Modal |> Just
            , get blockchain modal.pair
            , Nothing
            )

        ( ReceiveAnswer chain pair result, _ ) ->
            if
                (chain == (blockchain |> Blockchain.toChain))
                    && (pair == modal.pair)
            then
                ( { modal | pools = result |> Web.fromResult }
                    |> Modal
                    |> Just
                , Process.sleep 5000
                    |> Task.perform (\_ -> QueryAgain)
                , Nothing
                )

            else
                ( modal |> Modal |> Just
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

        ( OpenDropdown, _ ) ->
            ( { modal | sortDropdown = Just () }
                |> Modal
                |> Just
            , Cmd.none
            , Nothing
            )

        ( CloseDropdown, _ ) ->
            ( { modal | sortDropdown = Nothing }
                |> Modal
                |> Just
            , Cmd.none
            , Nothing
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
    }
    -> Modal
    -> Element Msg
view ({ time, offset, chosenZone, device } as model) (Modal modal) =
    Outside.view model
        { onClick = Exit
        , modal =
            column
                [ if device |> Device.isPhoneOrTablet then
                    width <| minimum 375 shrink

                  else
                    width <| minimum 622 shrink
                , height <| maximum 468 shrink
                , spacing 16
                , centerX
                , centerY
                , Background.color Color.background
                , Border.rounded 8
                , Border.color Color.transparent100
                , Border.width 1
                ]
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
                            , paddingXY 0 3
                            , Font.color Color.light100
                            ]
                            (text "Choose Maturity")
                        , IconButton.exit model Exit
                        ]
                    , row
                        [ width fill
                        , height shrink
                        , centerY
                        ]
                        [ pairWithPoolCount model (modal |> Modal)
                        , sortBy model (modal |> Modal)
                        ]
                    ]
                , row
                    [ width fill
                    , height <| px 34
                    , centerY
                    , paddingXY 24 0
                    , Background.color Color.list
                    ]
                    [ el
                        [ centerX
                        , width <| fillPortion 2
                        , Font.size 12
                        , Font.bold
                        , Font.letterSpacing 0.08
                        , Font.color Color.transparent200
                        , Font.center
                        ]
                        (text "MATURITY TIME")
                    , el
                        [ centerX
                        , width <| fillPortion 1
                        , Font.size 12
                        , Font.bold
                        , Font.letterSpacing 0.08
                        , Font.color Color.transparent200
                        , Font.center
                        ]
                        (text "APR")
                    , el
                        [ centerX
                        , width <| fillPortion 1
                        , Font.size 12
                        , Font.bold
                        , Font.letterSpacing 0.08
                        , Font.color Color.transparent200
                        , Font.center
                        ]
                        (text "CDP")
                    ]
                , maturityList model (modal |> Modal)
                ]
        }


pairWithPoolCount :
    { model
        | time : Posix
        , images : Images
    }
    -> Modal
    -> Element Msg
pairWithPoolCount { images } (Modal { pair, pools }) =
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
        [ row [ width <| px 52 ]
            [ images
                |> Image.viewToken
                    [ width <| px 32
                    , height <| px 32
                    , moveRight 16
                    ]
                    collateral
            , images
                |> Image.viewToken
                    [ width <| px 32
                    , height <| px 32
                    , moveLeft 32
                    ]
                    asset
            ]
        , el
            [ width shrink
            , height shrink
            , centerY
            , Font.size 16
            , paddingXY 0 3
            , Font.color Color.light100
            ]
            ((collateral |> Token.toSymbol)
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
            , Font.color Color.transparent300
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
    { model
        | images : Images
    }
    -> Modal
    -> Element Msg
sortBy { images } (Modal { sorting, sortDropdown }) =
    row
        [ alignRight
        , spacing 14
        , centerY
        , Font.color Color.primary400
        , Font.size 14
        ]
        [ text "Sort by"
        , Input.button
            [ Region.description
                "sort by"
            , width <| px 130
            , height <| px 38
            , Background.color Color.primary100
            , Border.width 1
            , Border.color Color.transparent100
            , Border.rounded 8
            , (if sortDropdown == Just () then
                [ Sorting.Liquidity
                , Sorting.Maturity
                ]
                    |> sortOptionsEl

               else
                none
              )
                |> below
            ]
            { onPress =
                if sortDropdown == Just () then
                    Nothing

                else
                    Just OpenDropdown

            -- if sortDropdown == Just () then
            --     Just CloseDropdown
            -- else
            --     Just OpenDropdown
            , label =
                row
                    [ width fill
                    , height fill
                    , paddingXY 12 0
                    , spacing 6
                    , Font.color Color.light100
                    ]
                    [ sorting |> Sorting.toString |> text
                    , images
                        |> Image.discloser
                            [ width <| px 11
                            , height <| px 7
                            , alignRight
                            , centerY
                            ]
                    ]
            }
        ]


sortOptionsEl : List Sorting -> Element Msg
sortOptionsEl sortList =
    column
        [ width <| px 130
        , height shrink
        , moveDown 10
        , Background.color Color.dark300
        , Border.rounded 4
        , Border.width 1
        , Border.color Color.transparent100
        ]
        (sortList
            |> List.map
                (\sortOption ->
                    Input.button
                        [ width fill
                        , height shrink
                        , paddingXY 12 10
                        , Font.color Color.light100
                        , mouseOver [ Background.color Color.primary100 ]
                        ]
                        { onPress =
                            case sortOption of
                                Sorting.Liquidity ->
                                    Just GoToSortLiquidity

                                Sorting.Maturity ->
                                    Just GoToSortMaturity
                        , label = sortOption |> Sorting.toString |> text
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
    }
    -> Modal
    -> Element Msg
maturityList { images, time, offset, chosenZone, priceFeed } (Modal { pair, pools, tooltip, sorting }) =
    column
        [ width fill
        , height shrink
        , paddingEach
            { top = 0
            , right = 24
            , bottom = 16
            , left = 24
            }
        , spacing 12
        ]
        (case pools of
            Loading ->
                [ el [ centerX, Font.color Color.light100 ] (text "Loading...") ]

            Failure err ->
                [ el [ centerX, Font.color Color.negative400 ] (text "Error") ]

            Success poolsDict ->
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
                                        , Background.color Color.primary100
                                        , Border.rounded 8
                                        , mouseDown [ Background.color Color.primary300 ]
                                        , mouseOver [ Background.color Color.primary200 ]
                                        ]
                                        [ row [ width <| fillPortion 2, spacing 16 ]
                                            [ images
                                                |> Image.hourglassPrimary
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
                                            , Font.color Color.light100
                                            ]
                                            (Calculate.cdp
                                                { onMouseEnter = OnMouseEnter
                                                , onMouseLeave = OnMouseLeave
                                                , cdpTooltip = Tooltip.CDP
                                                , opened = tooltip
                                                , pair = pair
                                                , cdp = summary.cdp
                                                }
                                                priceFeed
                                                Color.light100
                                                14
                                            )
                                        ]
                                }
                        )
        )
