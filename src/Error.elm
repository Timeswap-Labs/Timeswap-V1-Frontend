module Error exposing (view)

import Data.Images exposing (Images)
import Data.Remote exposing (Remote(..))
import Element
    exposing
        ( Element
        , alignRight
        , centerX
        , centerY
        , el
        , fill
        , height
        , none
        , paddingXY
        , px
        , row
        , shrink
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import User
import Utility.Color as Color
import Utility.Image as Image
import Utility.Typography as Typography


view :
    { msgs | closeError : msg }
    ->
        { model
            | images : Images
            , user : Remote User.Error user
        }
    -> Element msg
view msgs { images, user } =
    case user of
        Failure error ->
            row
                [ width fill
                , height <| px 44
                , paddingXY 20 0
                , spacing 6
                , Background.color Color.negative400
                , Font.family Typography.supreme
                ]
                [ Image.option images
                    [ width <| px 20
                    , centerX
                    , centerY
                    , Font.center
                    ]
                , el
                    [ centerX
                    , centerY
                    , Font.size 16
                    , Font.regular
                    , Font.color Color.transparent500
                    , Font.center
                    ]
                    (error
                        |> User.errorToMessage
                        |> text
                    )
                , Input.button
                    [ width shrink
                    , height shrink
                    , alignRight
                    , centerY
                    ]
                    { onPress = Just msgs.closeError
                    , label = Image.close images [ width <| px 24 ]
                    }
                ]

        _ ->
            none
