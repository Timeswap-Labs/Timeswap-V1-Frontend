module Page.Transaction.Output exposing
    ( disabledCollateral
    , disabledLiquidity
    , empty
    , liquidity
    , view
    , viewCollateral
    )

import Data.Images exposing (Images)
import Data.Pair exposing (Pair)
import Data.Remote exposing (Remote(..))
import Data.Theme exposing (Theme)
import Data.Token exposing (Token)
import Data.Uint exposing (Uint)
import Element
    exposing
        ( Element
        , centerX
        , centerY
        , clip
        , el
        , fill
        , height
        , moveLeft
        , none
        , paddingXY
        , px
        , row
        , shrink
        , spacing
        , text
        , width
        )
import Element.Font as Font
import Element.Region as Region
import Utility.Color as Color
import Utility.Fade as Fade
import Utility.Image as Image
import Utility.Loading as Loading
import Utility.PairImage as PairImage
import Utility.ThemeColor as ThemeColor
import Utility.Truncate as Truncate


view :
    { model | images : Images, theme : Theme }
    ->
        { onMouseEnter : tooltip -> msg
        , onMouseLeave : msg
        , tooltip : tooltip
        , opened : Maybe tooltip
        , token : Token
        , output : Remote error Uint
        , description : String
        }
    -> Element msg
view { images, theme } param =
    row
        [ Region.description param.description
        , width fill
        , height <| px 24
        , spacing 12
        , clip
        ]
        [ row
            [ width <| px 95
            , height shrink
            , spacing 6
            , centerY
            ]
            [ images
                |> Image.viewToken
                    [ width <| px 24
                    , height <| px 24
                    ]
                    param.token
            , Truncate.viewSymbol
                { onMouseEnter = param.onMouseEnter
                , onMouseLeave = param.onMouseLeave
                , tooltip = param.tooltip
                , opened = param.opened
                , token = param.token
                , theme = theme
                , customStyles = []
                }
            ]
        , el
            [ width shrink
            , height shrink
            , centerY
            ]
            (case param.output of
                Success output ->
                    output
                        |> Fade.view theme param.token

                Loading timeline ->
                    el
                        [ width shrink
                        , height shrink
                        , centerX
                        , centerY
                        ]
                        (Loading.view timeline theme)

                _ ->
                    none
            )
        ]


viewCollateral :
    { model | images : Images, theme : Theme }
    ->
        { onMouseEnter : tooltip -> msg
        , onMouseLeave : msg
        , tooltip : tooltip
        , opened : Maybe tooltip
        , token : Token
        , input : String
        , description : String
        }
    -> Element msg
viewCollateral { images, theme } param =
    row
        [ Region.description param.description
        , width fill
        , height <| px 24
        , spacing 12
        , clip
        ]
        [ row
            [ width <| px 95
            , height shrink
            , spacing 6
            , centerY
            ]
            [ images
                |> Image.viewToken
                    [ width <| px 24
                    , height <| px 24
                    ]
                    param.token
            , Truncate.viewSymbol
                { onMouseEnter = param.onMouseEnter
                , onMouseLeave = param.onMouseLeave
                , tooltip = param.tooltip
                , opened = param.opened
                , token = param.token
                , theme = theme
                , customStyles = []
                }
            ]
        , el
            [ width shrink
            , height shrink
            , centerY
            , theme |> ThemeColor.text |> Font.color
            , Font.size 16
            ]
            (text param.input)
        ]


disabledCollateral :
    { model | images : Images, theme : Theme }
    ->
        { token : Token
        , input : String
        , description : String
        }
    -> Element Never
disabledCollateral { images, theme } param =
    row
        [ Region.description param.description
        , width fill
        , height <| px 24
        , spacing 12
        , clip
        ]
        [ row
            [ width <| px 95
            , height shrink
            , spacing 6
            , centerY
            ]
            [ images
                |> Image.viewToken
                    [ width <| px 24
                    , height <| px 24
                    ]
                    param.token
            , Truncate.disabledSymbol param.token theme
            ]
        , el
            [ width shrink
            , height shrink
            , centerY
            ]
            (text param.input)
        ]


empty :
    { model | images : Images, theme : Theme }
    ->
        { token : Token
        , description : String
        }
    -> Element Never
empty { images, theme } param =
    el
        [ Region.description param.description
        , width fill
        , height <| px 24
        , clip
        ]
        (row
            [ width <| px 95
            , height shrink
            , spacing 6
            , centerY
            ]
            [ images
                |> Image.viewToken
                    [ width <| px 24
                    , height <| px 24
                    ]
                    param.token
            , Truncate.disabledSymbol param.token theme
            ]
        )


liquidity :
    { model | images : Images, theme : Theme }
    ->
        { pair : Pair
        , output : Remote error Uint
        , description : String
        }
    -> Element msg
liquidity { images, theme } param =
    el
        [ Region.description param.description
        , width fill
        , height <| px 24
        , spacing 12
        , clip
        ]
        (row
            [ width fill
            , height shrink
            , spacing 6
            , centerY
            ]
            [ row
                [ width <| px 80
                , height <| px 24
                , spacing 18
                ]
                [ images
                    |> PairImage.view
                        { pair = param.pair
                        , length = 24
                        }
                , el
                    [ width shrink
                    , height shrink
                    , centerY
                    , Font.size 16
                    , paddingXY 0 4
                    , Font.color Color.light100
                    , moveLeft 12
                    ]
                    (text "LIQ")
                ]
            , el
                [ width fill
                , height shrink
                , centerY
                ]
                (case param.output of
                    Success output ->
                        output
                            |> Fade.viewLP

                    Loading timeline ->
                        el
                            [ width shrink
                            , height shrink
                            , centerX
                            , centerY
                            ]
                            (Loading.view timeline theme)

                    _ ->
                        none
                )
            ]
        )


disabledLiquidity :
    { model | images : Images }
    ->
        { pair : Pair
        , description : String
        }
    -> Element Never
disabledLiquidity { images } param =
    el
        [ Region.description param.description
        , width fill
        , height <| px 24
        , spacing 12
        , clip
        ]
        (row
            [ width fill
            , height shrink
            , spacing 6
            , centerY
            ]
            [ images
                |> PairImage.view
                    { pair = param.pair
                    , length = 24
                    }
            ]
        )
