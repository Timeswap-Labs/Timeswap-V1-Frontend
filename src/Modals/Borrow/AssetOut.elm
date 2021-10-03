module Modals.Borrow.AssetOut exposing (view)

import Data.Balances exposing (Balances)
import Data.Pair as Pair exposing (Pair)
import Data.Remote exposing (Remote(..))
import Data.Token as Token
import Data.TokenImages exposing (TokenImages)
import Element
    exposing
        ( Element
        , alignLeft
        , centerY
        , column
        , el
        , fill
        , height
        , moveDown
        , paddingEach
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
import Modals.Borrow.DuesOut as DuesOut exposing (DuesOut)
import Utility.Color as Color
import Utility.TokenImage as TokenImage


view :
    { msgs | inputAssetOut : String -> msg }
    ->
        { model
            | tokenImages : TokenImages
            , user : Remote userError { user | balances : Remote () Balances }
        }
    ->
        { modal
            | pool : { pool | pair : Pair }
            , assetOut : String
            , duesOut : DuesOut
        }
    -> Element msg
view msgs model modal =
    column
        [ width fill
        , height shrink
        , paddingXY 20 16
        , spacing 16
        , Border.width 1
        , Border.solid
        , Border.color Color.transparent100
        , Border.rounded 4
        ]
        [ title
        , assetOutTextbox msgs model modal
        ]


title : Element msg
title =
    el
        [ width fill
        , height shrink
        , spacing 4
        ]
        (el
            [ alignLeft
            , paddingXY 0 4
            , Font.bold
            , Font.size 16
            , Font.color Color.transparent500
            ]
            (text "Amount to borrow")
        )


assetOutTextbox :
    { msgs | inputAssetOut : String -> msg }
    ->
        { model
            | tokenImages : TokenImages
            , user : Remote userError { user | balances : Remote () Balances }
        }
    ->
        { modal
            | pool : { pool | pair : Pair }
            , assetOut : String
            , duesOut : DuesOut
        }
    -> Element msg
assetOutTextbox msgs model modal =
    row
        [ width fill
        , height <| px 44
        ]
        [ logo model modal
        , amount msgs model modal
        ]


logo :
    { model
        | tokenImages : TokenImages
        , user : Remote userError { user | balances : Remote () Balances }
    }
    ->
        { modal
            | pool : { pool | pair : Pair }
            , assetOut : String
            , duesOut : DuesOut
        }
    -> Element msg
logo ({ tokenImages } as model) ({ pool } as modal) =
    row
        [ width shrink
        , height fill
        , paddingXY 12 0
        , spacing 6
        , Background.color Color.primary100
        , Border.widthEach
            { top = 1
            , right = 0
            , bottom = 1
            , left = 1
            }
        , Border.solid
        , (if DuesOut.isCorrect model modal then
            Color.transparent100

           else
            Color.negative500
          )
            |> Border.color
        , Border.roundEach
            { topLeft = 4
            , topRight = 0
            , bottomRight = 0
            , bottomLeft = 4
            }
        ]
        [ pool.pair
            |> Pair.toAsset
            |> TokenImage.icon tokenImages
                [ width <| px 24
                , alignLeft
                , centerY
                ]
        , el
            [ alignLeft
            , centerY
            , Font.regular
            , Font.size 16
            , Font.color Color.light100
            ]
            (pool.pair
                |> Pair.toAsset
                |> Token.toSymbol
                |> text
            )
        ]


amount :
    { msgs | inputAssetOut : String -> msg }
    -> { model | user : Remote userError { user | balances : Remote () Balances } }
    ->
        { modal
            | pool : { pool | pair : Pair }
            , assetOut : String
            , duesOut : DuesOut
        }
    -> Element msg
amount msgs model modal =
    el
        [ width fill
        , height fill
        , paddingEach
            { top = 0
            , right = 12
            , bottom = 0
            , left = 0
            }
        , spacing 8
        , Border.widthEach
            { top = 1
            , right = 1
            , bottom = 1
            , left = 0
            }
        , Border.solid
        , (if DuesOut.isCorrect model modal then
            Color.transparent100

           else
            Color.negative500
          )
            |> Border.color
        , Border.roundEach
            { topLeft = 0
            , topRight = 4
            , bottomRight = 4
            , bottomLeft = 0
            }
        ]
        (assetOutInput msgs model modal)


assetOutInput :
    { msgs | inputAssetOut : String -> msg }
    -> { model | user : Remote userError { user | balances : Remote () Balances } }
    ->
        { modal
            | pool : { pool | pair : Pair }
            , assetOut : String
            , duesOut : DuesOut
        }
    -> Element msg
assetOutInput msgs model ({ assetOut } as modal) =
    Input.text
        [ width fill
        , height shrink
        , paddingXY 12 4
        , alignLeft
        , centerY
        , moveDown 1
        , Background.color Color.none
        , Border.color Color.none
        , Border.width 0
        , Font.regular
        , Font.size 16
        , (if DuesOut.isCorrect model modal then
            Color.transparent500

           else
            Color.negative500
          )
            |> Font.color
        ]
        { onChange = msgs.inputAssetOut
        , text = assetOut
        , placeholder =
            Input.placeholder
                [ Font.color Color.transparent100 ]
                (text "0.0")
                |> Just
        , label = Input.labelHidden "Input Amount"
        }
