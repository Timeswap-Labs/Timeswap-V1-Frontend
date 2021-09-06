module Modals.Lend.AssetIn exposing (view)

import Data.Balances as Balances exposing (Balances)
import Data.Pair as Pair exposing (Pair)
import Data.Remote exposing (Remote(..))
import Data.Token as Token
import Data.TokenImages exposing (TokenImages)
import Element
    exposing
        ( Element
        , alignLeft
        , alignRight
        , below
        , centerY
        , column
        , el
        , fill
        , height
        , none
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
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Modals.Lend.ClaimsOut as ClaimsOut exposing (ClaimsOut)
import Modals.Lend.Tooltip as Tooltip exposing (Tooltip)
import Utility.Color as Color
import Utility.Loading as Loading
import Utility.TokenImage as TokenImage
import Utility.Truncate as Truncate


view :
    { msgs
        | inputAssetIn : String -> msg
        , inputMax : msg
        , onMouseEnter : Tooltip -> msg
        , onMouseLeave : msg
    }
    ->
        { model
            | tokenImages : TokenImages
            , user : Maybe { user | balances : Remote Balances }
        }
    ->
        { modal
            | pool : { pool | pair : Pair }
            , assetIn : String
            , claimsOut : ClaimsOut
            , tooltip : Maybe Tooltip
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
        [ title msgs model modal
        , assetInTextbox msgs model modal
        ]


title :
    { msgs
        | onMouseEnter : Tooltip -> msg
        , onMouseLeave : msg
    }
    -> { model | user : Maybe { user | balances : Remote Balances } }
    -> { modal | pool : { pool | pair : Pair }, tooltip : Maybe Tooltip }
    -> Element msg
title msgs { user } { pool, tooltip } =
    row
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
            (text "Amount to lend")
            :: (user
                    |> Maybe.map
                        (\{ balances } ->
                            case balances of
                                Loading ->
                                    [ el
                                        [ height <| px 20
                                        , alignRight
                                        , centerY
                                        ]
                                        Loading.view
                                    ]

                                Failure ->
                                    []

                                Success successBalances ->
                                    [ successBalances
                                        |> Balances.get (pool.pair |> Pair.toAsset)
                                        |> Truncate.amount
                                        |> (\{ full, truncated } ->
                                                truncated
                                                    |> Maybe.map
                                                        (\short ->
                                                            row
                                                                [ height <| px 20
                                                                , alignRight
                                                                , centerY
                                                                , Font.size 14
                                                                , Font.color Color.transparent300
                                                                ]
                                                                [ el
                                                                    [ paddingXY 0 3
                                                                    , Font.regular
                                                                    ]
                                                                    (text "Your Balance: ")
                                                                , pool.pair
                                                                    |> Pair.toAsset
                                                                    |> Token.toSymbol
                                                                    |> (\symbol ->
                                                                            el
                                                                                ([ paddingXY 0 3
                                                                                 , Font.regular
                                                                                 , Border.widthEach
                                                                                    { top = 0
                                                                                    , right = 0
                                                                                    , bottom = 1
                                                                                    , left = 0
                                                                                    }
                                                                                 , Border.dashed
                                                                                 , Border.color Color.transparent300
                                                                                 , Events.onMouseEnter (msgs.onMouseEnter Tooltip.AssetBalance)
                                                                                 , Events.onMouseLeave msgs.onMouseLeave
                                                                                 ]
                                                                                    ++ (tooltip
                                                                                            |> Maybe.map
                                                                                                (\tooltipJust ->
                                                                                                    case tooltipJust of
                                                                                                        Tooltip.AssetBalance ->
                                                                                                            [ [ full
                                                                                                              , symbol
                                                                                                              ]
                                                                                                                |> String.join " "
                                                                                                                |> Tooltip.assetBalance
                                                                                                                |> below
                                                                                                            ]
                                                                                                )
                                                                                            |> Maybe.withDefault []
                                                                                       )
                                                                                )
                                                                                ([ short
                                                                                 , symbol
                                                                                 ]
                                                                                    |> String.join " "
                                                                                    |> text
                                                                                )
                                                                       )
                                                                ]
                                                        )
                                                    |> Maybe.withDefault
                                                        (el
                                                            [ height <| px 20
                                                            , alignRight
                                                            , centerY
                                                            , paddingXY 0 3
                                                            , Font.regular
                                                            , Font.size 14
                                                            , Font.color Color.transparent300
                                                            ]
                                                            ([ "Your Balance:"
                                                             , full
                                                             , pool.pair
                                                                |> Pair.toAsset
                                                                |> Token.toSymbol
                                                             ]
                                                                |> String.join " "
                                                                |> text
                                                            )
                                                        )
                                           )
                                    ]
                        )
                    |> Maybe.withDefault []
               )
        )


assetInTextbox :
    { msgs | inputAssetIn : String -> msg, inputMax : msg }
    ->
        { model
            | tokenImages : TokenImages
            , user : Maybe { user | balances : Remote Balances }
        }
    ->
        { modal
            | pool : { pool | pair : Pair }
            , assetIn : String
            , claimsOut : ClaimsOut
        }
    -> Element msg
assetInTextbox msgs model modal =
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
        , user : Maybe { user | balances : Remote Balances }
    }
    ->
        { modal
            | pool : { pool | pair : Pair }
            , assetIn : String
            , claimsOut : ClaimsOut
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
        , (if ClaimsOut.isCorrect model modal then
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
    { msgs | inputAssetIn : String -> msg, inputMax : msg }
    -> { model | user : Maybe { user | balances : Remote Balances } }
    ->
        { modal
            | pool : { pool | pair : Pair }
            , assetIn : String
            , claimsOut : ClaimsOut
        }
    -> Element msg
amount msgs ({ user } as model) modal =
    row
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
        , (if ClaimsOut.isCorrect model modal then
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
        [ assetInInput msgs model modal
        , user
            |> Maybe.map
                (\{ balances } ->
                    case balances of
                        Success _ ->
                            maxButton msgs

                        _ ->
                            none
                )
            |> Maybe.withDefault none
        ]


assetInInput :
    { msgs | inputAssetIn : String -> msg }
    -> { model | user : Maybe { user | balances : Remote Balances } }
    ->
        { modal
            | pool : { pool | pair : Pair }
            , assetIn : String
            , claimsOut : ClaimsOut
        }
    -> Element msg
assetInInput msgs model ({ assetIn } as modal) =
    Input.text
        [ width fill
        , height shrink
        , paddingXY 12 4
        , alignLeft
        , centerY
        , Background.color Color.none
        , Border.color Color.none
        , Font.regular
        , Font.size 16
        , (if ClaimsOut.isCorrect model modal then
            Color.transparent500

           else
            Color.negative500
          )
            |> Font.color
        ]
        { onChange = msgs.inputAssetIn
        , text = assetIn
        , placeholder =
            Input.placeholder
                [ Font.color Color.transparent100 ]
                (text "0.0")
                |> Just
        , label = Input.labelHidden "Input Amount"
        }


maxButton : { msgs | inputMax : msg } -> Element msg
maxButton msgs =
    Input.button
        [ width shrink
        , height shrink
        , centerY
        , Font.regular
        , Font.size 16
        , Font.color Color.primary500
        ]
        { onPress = Just msgs.inputMax
        , label = text "MAX"
        }
