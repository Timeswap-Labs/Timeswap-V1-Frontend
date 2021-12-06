module Page.Transaction.Output exposing (disabled, view)

import Data.Images exposing (Images)
import Data.Remote exposing (Remote(..))
import Data.Token exposing (Token)
import Data.Uint exposing (Uint)
import Element
    exposing
        ( Element
        , centerY
        , clip
        , el
        , fill
        , height
        , none
        , paddingXY
        , px
        , row
        , shrink
        , spacing
        , width
        )
import Element.Region as Region
import Utility.Fade as Fade
import Utility.Image as Image
import Utility.Truncate as Truncate


view :
    { model | images : Images }
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
view { images } param =
    row
        [ Region.description param.description
        , width fill
        , height <| px 24
        , spacing 12
        , clip
        ]
        [ row
            [ width <| px 80
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
                        |> Fade.view param.token

                Loading ->
                    none |> Debug.log "loading animation"

                _ ->
                    none
            )
        ]


disabled :
    { model | images : Images }
    ->
        { token : Token
        , description : String
        }
    -> Element Never
disabled { images } param =
    el
        [ Region.description param.description
        , width fill
        , height <| px 24
        , spacing 12
        , clip
        ]
        (row
            [ width <| px 80
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
            , Truncate.disabledSymbol param.token
            ]
        )
