module Modals.Lend.Error exposing
    ( Error(..)
    , decoder
    , insufficientAsset
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
    | BondUnderflow
    | BondOverflow
    | InsuranceUnderflow
    | InsuranceOverflow
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
                        BondUnderflow

                    3 ->
                        BondOverflow

                    4 ->
                        BondOverflow

                    5 ->
                        InsuranceOverflow

                    _ ->
                        Invalid
            )


toString : Error -> String
toString error =
    case error of
        PrincipalOverflow ->
            "Lend amount too much"

        BondUnderflow ->
            "Bond receive too little"

        BondOverflow ->
            "Bond receive too much"

        InsuranceUnderflow ->
            "Insurance receive too little"

        InsuranceOverflow ->
            "Insurance receive too much"

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


insufficientAsset : Element msg
insufficientAsset =
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
            (text "Insufficient asset")
        )
