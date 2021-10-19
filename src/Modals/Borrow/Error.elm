module Modals.Borrow.Error exposing
    ( Error(..)
    , decoder
    , insufficientCollateral
    , view
    )

import Element
    exposing
        ( Element
        , centerX
        , centerY
        , el
        , fill
        , height
        , paddingEach
        , px
        , shrink
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Json.Decode as Decode exposing (Decoder)
import Utility.Color as Color


type Error
    = PrincipalOverflow
    | DebtUnderflow
    | DebtOverflow
    | CollateralUnderflow
    | CollateralOverflow
    | Invalid


decoder : Decoder Error
decoder =
    Decode.int
        |> Decode.map
            (\code ->
                case code of
                    1 ->
                        PrincipalOverflow

                    2 ->
                        DebtUnderflow

                    3 ->
                        DebtOverflow

                    4 ->
                        CollateralUnderflow

                    5 ->
                        CollateralOverflow

                    _ ->
                        Invalid
            )


toString : Error -> String
toString error =
    case error of
        PrincipalOverflow ->
            "Lend amount too much"

        DebtUnderflow ->
            "Debt receive too little"

        DebtOverflow ->
            "Debt receive too much"

        CollateralUnderflow ->
            "Collateral lock too little"

        CollateralOverflow ->
            "Collateral lock too much"

        Invalid ->
            "Invalid Transaction"


view : Error -> Element msg
view error =
    el
        [ width fill
        , height <| px 44
        , paddingEach
            { top = 0
            , right = 16
            , bottom = 0
            , left = 10
            }
        , centerX
        , centerY
        , Background.color Color.negative100
        , Border.rounded 4
        , Font.size 16
        , Font.color Color.light500
        ]
        (el
            [ width shrink
            , height shrink
            , centerX
            , centerY
            , Font.bold
            , Font.size 16
            , Font.color Color.negative500
            ]
            (error
                |> toString
                |> text
            )
        )


insufficientCollateral : Element msg
insufficientCollateral =
    el
        [ width fill
        , height <| px 44
        , paddingEach
            { top = 0
            , right = 16
            , bottom = 0
            , left = 10
            }
        , centerX
        , centerY
        , Background.color Color.negative100
        , Border.rounded 4
        , Font.size 16
        , Font.color Color.light500
        ]
        (el
            [ width shrink
            , height shrink
            , centerX
            , centerY
            , Font.bold
            , Font.size 16
            , Font.color Color.negative500
            ]
            (text "Insufficient collateral")
        )
