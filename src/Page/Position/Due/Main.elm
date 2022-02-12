module Page.Position.Due.Main exposing
    ( Effect(..)
    , Msg
    , Position
    , init
    , update
    , view
    )

import Blockchain.User.Due as Due
import Blockchain.User.Main as User exposing (User)
import Blockchain.User.TokenId as TokenId exposing (TokenId)
import Data.Backdrop exposing (Backdrop)
import Data.ChosenZone exposing (ChosenZone)
import Data.Device exposing (Device(..))
import Data.Images exposing (Images)
import Data.Maturity as Maturity
import Data.Offset exposing (Offset)
import Data.Pair as Pair
import Data.Pool exposing (Pool)
import Data.Remote as Remote exposing (Remote(..))
import Data.Theme as Theme exposing (Theme)
import Data.TokenParam as TokenParam
import Data.Uint as Uint
import Element
    exposing
        ( Element
        , alignRight
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , none
        , padding
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
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Page.Position.Due.Tooltip as Tooltip exposing (Tooltip)
import Sort.Dict as Dict
import Sort.Set as Set exposing (Set)
import Time exposing (Posix)
import Utility.Color as Color
import Utility.Duration as Duration
import Utility.Glass as Glass
import Utility.Image as Image
import Utility.PairImage as PairImage
import Utility.ThemeColor as ThemeColor
import Utility.Truncate as Truncate


type Position
    = Position
        { pool : Pool
        , checks : Set TokenId
        , tooltip : Maybe Tooltip
        }


type Msg
    = ClickBorrowMore
    | ClickProceed
    | ClickReturn
    | Check TokenId Bool
    | OnMouseEnter Tooltip
    | OnMouseLeave


type Effect
    = InputPool Pool
    | OpenPayTransaction Pool (Set TokenId)


init : Pool -> Position
init pool =
    { pool = pool
    , checks = Set.empty TokenId.sorter
    , tooltip = Nothing
    }
        |> Position


update :
    User
    -> Msg
    -> Position
    -> ( Maybe Position, Maybe Effect )
update user msg (Position position) =
    case msg of
        ClickBorrowMore ->
            ( Nothing
            , InputPool position.pool
                |> Just
            )

        ClickProceed ->
            ( { position | checks = Set.empty TokenId.sorter }
                |> Position
                |> Just
            , position
                |> getChecks user
                |> OpenPayTransaction position.pool
                |> Just
            )

        ClickReturn ->
            ( Nothing
            , Nothing
            )

        Check tokenId bool ->
            ( { position
                | checks =
                    if bool then
                        position.checks
                            |> Set.insert tokenId

                    else
                        position.checks
                            |> Set.remove tokenId
              }
                |> Position
                |> Just
            , Nothing
            )

        OnMouseEnter tooltip ->
            ( { position | tooltip = Just tooltip }
                |> Position
                |> Just
            , Nothing
            )

        OnMouseLeave ->
            ( { position | tooltip = Nothing }
                |> Position
                |> Just
            , Nothing
            )


getChecks :
    User
    ->
        { position
            | pool : Pool
            , checks : Set TokenId
        }
    -> Set TokenId
getChecks user { pool, checks } =
    checks
        |> Set.keepIf
            (\tokenId ->
                user
                    |> User.getDues
                    |> Remote.map (Dict.get pool)
                    |> (Remote.map << Maybe.map)
                        (\dict ->
                            dict
                                |> Dict.get tokenId
                                |> Maybe.map
                                    (\{ collateral } ->
                                        collateral
                                            |> Uint.isZero
                                            |> not
                                    )
                                |> Maybe.withDefault False
                        )
                    |> (Remote.map << Maybe.withDefault) False
                    |> Remote.withDefault False
            )


view :
    { model
        | time : Posix
        , offset : Offset
        , chosenZone : ChosenZone
        , device : Device
        , backdrop : Backdrop
        , theme : Theme
        , images : Images
    }
    -> User
    -> Position
    -> Element Msg
view ({ device, backdrop, theme } as model) user (Position position) =
    column
        [ width shrink
        , height shrink
        , spacing 20
        ]
        [ returnButton model
        , column
            ([ Region.description "Borrow positions"
             , (case device of
                    Desktop ->
                        758

                    _ ->
                        375
               )
                |> px
                |> width
             , height shrink
             , (case device of
                    Desktop ->
                        24

                    _ ->
                        16
               )
                |> padding
             , spacing 30
             , Border.rounded 8
             , Border.width 1
             , theme |> ThemeColor.border |> Border.color
             ]
                ++ Glass.background backdrop theme
            )
            [ header model position
            , viewDue model user position
            ]
        ]


returnButton : { model | images : Images, theme : Theme } -> Element Msg
returnButton { images, theme } =
    Input.button
        [ width shrink
        , height shrink
        ]
        { onPress = Just ClickReturn
        , label =
            row
                [ width shrink
                , height shrink
                , spacing 12
                ]
                [ images
                    |> (case theme of
                            Theme.Dark ->
                                Image.arrowLeft

                            Theme.Light ->
                                Image.arrowLeftDark
                       )
                        [ width <| px 16
                        , height <| px 16
                        , centerY
                        ]
                , el
                    [ width shrink
                    , height shrink
                    , Font.size 16
                    , Font.bold
                    , paddingXY 0 2
                    , theme |> ThemeColor.text |> Font.color
                    , centerY
                    ]
                    (text "Back to borrow")
                ]
        }


header :
    { model
        | time : Posix
        , offset : Offset
        , chosenZone : ChosenZone
        , theme : Theme
        , images : Images
    }
    -> { position | pool : Pool, checks : Set TokenId, tooltip : Maybe Tooltip }
    -> Element Msg
header { time, offset, chosenZone, theme, images } { pool, checks, tooltip } =
    row
        [ width fill
        , height shrink
        , spacing 12
        ]
        [ el
            [ width shrink
            , height shrink
            , centerY
            ]
            (images
                |> PairImage.view
                    { pair = pool.pair
                    , length = 32
                    }
            )
        , el
            [ width shrink
            , height shrink
            , centerY
            ]
            (Truncate.viewPairSymbol
                { onMouseEnter = OnMouseEnter
                , onMouseLeave = OnMouseLeave
                , tooltip = Tooltip.Pair
                , opened = tooltip
                , pair = pool.pair
                , fontSize = 16
                , fontPadding = 2
                , theme = theme
                }
            )
        , el
            [ width shrink
            , height shrink
            , alignRight
            , centerY
            ]
            (Duration.viewMaturity
                { onMouseEnter = OnMouseEnter
                , onMouseLeave = OnMouseLeave
                , tooltip = Tooltip.Maturity
                , opened = tooltip
                , time = time
                , offset = offset
                , chosenZone = chosenZone
                , maturity = pool.maturity
                , theme = theme
                }
            )
        , if pool.maturity |> Maturity.isActive time then
            borrowMoreButton theme

          else
            borrowMoreDisabled
        , if (pool.maturity |> Maturity.isActive time) && (checks |> Set.isEmpty |> not) then
            repayButton theme

          else
            repayDisabled theme
        ]


borrowMoreButton : Theme -> Element Msg
borrowMoreButton theme =
    Input.button
        [ width shrink
        , height <| px 44
        , paddingXY 12 5
        , Border.rounded 4
        , Border.width 1
        , theme |> ThemeColor.btnHoverBG |> Border.color
        ]
        { onPress = Just ClickBorrowMore
        , label =
            el
                [ width shrink
                , height shrink
                , centerX
                , centerY
                , Font.size 16
                , Font.bold
                , theme |> ThemeColor.text |> Font.color
                ]
                (text "Borrow More")
        }


borrowMoreDisabled : Element msg
borrowMoreDisabled =
    el
        [ width shrink
        , height <| px 44
        , paddingXY 12 5
        , Border.rounded 4
        , Border.width 1
        , Border.color Color.transparent200
        ]
        (el
            [ width shrink
            , height shrink
            , centerX
            , centerY
            , Font.size 16
            , Font.color Color.transparent200
            , Font.bold
            ]
            (text "Borrow More")
        )


repayButton : Theme -> Element Msg
repayButton theme =
    Input.button
        [ width <| px 153
        , height <| px 44
        , Border.rounded 4
        , theme |> ThemeColor.primaryBtn |> Background.color
        ]
        { onPress = Just ClickProceed
        , label =
            el
                [ width shrink
                , height shrink
                , centerX
                , centerY
                , Font.size 16
                , Font.color Color.light100
                , Font.bold
                ]
                (text "Proceed to Repay")
        }


repayDisabled : Theme -> Element msg
repayDisabled theme =
    el
        [ width <| px 153
        , height <| px 44
        , Border.rounded 4
        , theme |> ThemeColor.btnBackground |> Background.color
        ]
        (el
            [ width shrink
            , height shrink
            , centerX
            , centerY
            , Font.size 16
            , Font.color Color.transparent300
            , Font.bold
            ]
            (text "Proceed to Repay")
        )


viewDue :
    { model | images : Images, theme : Theme }
    -> User
    ->
        { pool : Pool
        , checks : Set TokenId
        , tooltip : Maybe Tooltip
        }
    -> Element Msg
viewDue { images, theme } user { pool, checks, tooltip } =
    column
        [ width fill
        , spacing 12
        ]
        (user
            |> User.getDues
            |> Remote.map (Dict.get pool)
            |> (Remote.map << Maybe.map)
                (\dict ->
                    dict
                        |> Due.dropZero
                        |> Dict.toList
                        |> List.map
                            (\( tokenId, dues ) ->
                                row
                                    [ width fill
                                    , height <| px 82
                                    , theme |> ThemeColor.positionBG |> Background.color
                                    , Border.rounded 8
                                    , paddingXY 24 16
                                    , spacing 48
                                    ]
                                    [ Input.checkbox
                                        [ width <| px 24
                                        , height <| px 24
                                        , Border.rounded 4
                                        ]
                                        { onChange = Check tokenId
                                        , icon = Input.defaultCheckbox
                                        , checked = tokenId |> Set.memberOf checks
                                        , label =
                                            Input.labelHidden "Due checkbox"
                                        }
                                    , column
                                        [ width shrink
                                        , height shrink
                                        , spacing 8
                                        , centerY
                                        ]
                                        [ el
                                            [ width shrink
                                            , height shrink
                                            , Font.size 14
                                            , paddingXY 0 3
                                            , theme |> ThemeColor.textLight |> Font.color
                                            ]
                                            (text "Asset to Repay")
                                        , row
                                            [ width shrink
                                            , height <| px 24
                                            , spacing 12
                                            ]
                                            [ row
                                                [ width shrink
                                                , height shrink
                                                , spacing 6
                                                ]
                                                [ images
                                                    |> Image.viewToken
                                                        [ width <| px 24
                                                        , height <| px 24
                                                        , centerY
                                                        ]
                                                        (pool.pair |> Pair.toAsset)
                                                , Truncate.viewSymbol
                                                    { onMouseEnter = OnMouseEnter
                                                    , onMouseLeave = OnMouseLeave
                                                    , tooltip = Tooltip.Symbol tokenId TokenParam.Asset
                                                    , opened = tooltip
                                                    , token = pool.pair |> Pair.toAsset
                                                    , theme = theme
                                                    , customStyles = []
                                                    }
                                                ]
                                            , Truncate.viewAmount
                                                { onMouseEnter = OnMouseEnter
                                                , onMouseLeave = OnMouseLeave
                                                , tooltip = Tooltip.Amount tokenId TokenParam.Asset
                                                , opened = tooltip
                                                , token = pool.pair |> Pair.toAsset
                                                , amount = dues.debt
                                                , theme = theme
                                                , customStyles = []
                                                }
                                            ]
                                        ]
                                    , el
                                        [ width <| px 1
                                        , height fill
                                        , theme |> ThemeColor.textDisabled |> Background.color
                                        ]
                                        none
                                    , column
                                        [ width shrink
                                        , height shrink
                                        , spacing 8
                                        ]
                                        [ el
                                            [ width shrink
                                            , height shrink
                                            , Font.size 14
                                            , paddingXY 0 3
                                            , theme |> ThemeColor.textLight |> Font.color
                                            ]
                                            (text "Collateral to unlock")
                                        , row
                                            [ width shrink
                                            , height <| px 24
                                            , spacing 12
                                            ]
                                            [ row
                                                [ width shrink
                                                , height shrink
                                                , spacing 6
                                                ]
                                                [ images
                                                    |> Image.viewToken
                                                        [ width <| px 24
                                                        , height <| px 24
                                                        , centerY
                                                        ]
                                                        (pool.pair |> Pair.toCollateral)
                                                , Truncate.viewSymbol
                                                    { onMouseEnter = OnMouseEnter
                                                    , onMouseLeave = OnMouseLeave
                                                    , tooltip = Tooltip.Symbol tokenId TokenParam.Collateral
                                                    , opened = tooltip
                                                    , token = pool.pair |> Pair.toCollateral
                                                    , theme = theme
                                                    , customStyles = []
                                                    }
                                                ]
                                            , Truncate.viewAmount
                                                { onMouseEnter = OnMouseEnter
                                                , onMouseLeave = OnMouseLeave
                                                , tooltip = Tooltip.Amount tokenId TokenParam.Collateral
                                                , opened = tooltip
                                                , token = pool.pair |> Pair.toCollateral
                                                , amount = dues.collateral
                                                , theme = theme
                                                , customStyles = []
                                                }
                                            ]
                                        ]
                                    ]
                            )
                )
            |> (Remote.map << Maybe.withDefault) []
            |> Remote.withDefault []
        )
