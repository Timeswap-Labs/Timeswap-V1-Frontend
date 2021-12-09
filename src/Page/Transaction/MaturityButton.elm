module Page.Transaction.MaturityButton exposing (disabled, view)

import Data.ChosenZone exposing (ChosenZone)
import Data.Images exposing (Images)
import Data.Maturity exposing (Maturity)
import Data.Offset exposing (Offset)
import Element
    exposing
        ( Element
        , alignLeft
        , alignRight
        , centerX
        , centerY
        , el
        , fill
        , height
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
import Time exposing (Posix)
import Utility.Color as Color
import Utility.Duration as Duration
import Utility.Image as Image


view :
    { model
        | time : Posix
        , offset : Offset
        , chosenZone : ChosenZone
        , images : Images
    }
    ->
        { onPress : msg
        , onMouseEnter : tooltip -> msg
        , onMouseLeave : msg
        , tooltip : tooltip
        , opened : Maybe tooltip
        , maturity : Maybe Maturity
        }
    -> Element msg
view { time, offset, chosenZone, images } param =
    param.maturity
        |> Maybe.map
            (\maturity ->
                Input.button
                    [ Region.description "maturity button"
                    , width fill
                    , height <| px 44
                    , Background.color Color.primary100
                    , Border.width 1
                    , Border.color Color.transparent100
                    , Border.rounded 8
                    ]
                    { onPress = param.onPress |> Just
                    , label =
                        row
                            [ width fill
                            , height fill
                            , paddingXY 12 0
                            , spacing 6
                            ]
                            [ Duration.viewMaturity
                                { onMouseEnter = param.onMouseEnter
                                , onMouseLeave = param.onMouseLeave
                                , tooltip = param.tooltip
                                , opened = param.opened
                                , time = time
                                , offset = offset
                                , chosenZone = chosenZone
                                , maturity = maturity
                                }
                            , images
                                |> Image.discloser
                                    [ width <| px 9
                                    , alignRight
                                    , centerY
                                    ]
                            ]
                    }
            )
        |> Maybe.withDefault
            (Input.button
                [ Region.description "maturity button"
                , width fill
                , height <| px 44
                , Background.color Color.primary500
                , Border.rounded 8
                ]
                { onPress = param.onPress |> Just
                , label =
                    row
                        [ width fill
                        , height fill
                        , paddingXY 12 0
                        , spacing 6
                        ]
                        [ el
                            [ width <| px 24
                            , height <| px 24
                            ]
                            (images
                                |> Image.hourglass
                                    [ height <| px 24
                                    , centerX
                                    ]
                            )
                        , el
                            [ width shrink
                            , height shrink
                            , alignLeft
                            , centerY
                            , Font.size 14
                            , Font.color Color.transparent400
                            ]
                            (text "Select Maturity")
                        , images
                            |> Image.discloser
                                [ width <| px 9
                                , alignRight
                                , centerY
                                ]
                        ]
                }
            )


disabled : Element Never
disabled =
    el
        [ width fill
        , height <| px 44
        , paddingXY 12 0
        , spacing 6
        , Background.color Color.primary100
        , Border.rounded 8
        ]
        (el
            [ width shrink
            , height shrink
            , alignLeft
            , centerY
            , Font.size 14
            , Font.color Color.transparent100
            ]
            (text "Select Pair First")
        )
