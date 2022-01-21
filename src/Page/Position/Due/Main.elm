module Page.Position.Due.Main exposing
    ( Effect(..)
    , Msg
    , Position
    , init
    , update
    , view
    )

import Blockchain.User.Main as User exposing (User)
import Blockchain.User.TokenId as TokenId exposing (TokenId)
import Data.Backdrop exposing (Backdrop)
import Data.ChosenZone exposing (ChosenZone)
import Data.Device exposing (Device(..))
import Data.Images exposing (Images)
import Data.Offset exposing (Offset)
import Data.Pool exposing (Pool)
import Data.Remote as Remote exposing (Remote(..))
import Data.Theme exposing (Theme)
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
        , rotate
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
import Utility.Truncate as Truncate


type Position
    = Position
        { pool : Pool
        , checks : Set TokenId
        , tooltip : Maybe Tooltip
        }


type Msg
    = ClickBorrowMore
    | ClickProcceed
    | ClickReturn
    | Check TokenId Bool
    | OnMouseEnter Tooltip
    | OnMouseLeave


type Effect
    = InputPool Pool
    | OpenPay Pool (Set TokenId)


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

        ClickProcceed ->
            ( position
                |> Position
                |> Just
            , position
                |> getChecks user
                |> OpenPay position.pool
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
            ([ Region.description "lend positions"
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
             , Border.color Color.transparent100
             ]
                ++ Glass.background backdrop theme
            )
            [ header model position ]
        ]


returnButton : { model | images : Images } -> Element Msg
returnButton { images } =
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
                    |> Image.arrowLeft
                        [ width <| px 16
                        , height <| px 16
                        , centerY
                        ]
                , el
                    [ width shrink
                    , height shrink
                    , Font.size 16
                    , paddingXY 0 2
                    , Font.color Color.transparent500
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
    -> { position | pool : Pool, tooltip : Maybe Tooltip }
    -> Element Msg
header { time, offset, chosenZone, theme, images } { pool, tooltip } =
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
        ]
