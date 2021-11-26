module Utility.Truncate exposing (amount, balance, symbol)


type alias Truncated =
    { full : String
    , truncated : Maybe String
    }


balance :
    { amount : String
    , symbol : String
    }
    -> Truncated
balance full =
    case
        ( full.amount
            |> amount
            |> .truncated
        , full.symbol
            |> symbol
            |> .truncated
        )
    of
        ( Just truncatedAmount, Just truncatedSymbol ) ->
            { full =
                [ full.amount
                , full.symbol
                ]
                    |> String.join " "
            , truncated =
                [ truncatedAmount
                , truncatedSymbol
                ]
                    |> String.join " "
                    |> Just
            }

        ( Just truncatedAmount, Nothing ) ->
            { full =
                [ full.amount
                , full.symbol
                ]
                    |> String.join " "
            , truncated =
                [ truncatedAmount
                , full.symbol
                ]
                    |> String.join " "
                    |> Just
            }

        ( Nothing, Just truncatedSymbol ) ->
            { full =
                [ full.amount
                , full.symbol
                ]
                    |> String.join " "
            , truncated =
                [ full.amount
                , truncatedSymbol
                ]
                    |> String.join " "
                    |> Just
            }

        _ ->
            { full =
                [ full.amount
                , full.symbol
                ]
                    |> String.join " "
            , truncated = Nothing
            }


amount : String -> Truncated
amount full =
    full
        |> String.split "."
        |> (\list ->
                case list of
                    whole :: fraction :: _ ->
                        if (whole |> String.length) > 15 then
                            [ [ whole |> String.dropRight 15
                              , whole
                                    |> String.dropRight 13
                                    |> String.right 2
                              ]
                                |> String.join "."
                            , "Q"
                            ]
                                |> String.concat
                                |> Just
                                |> Truncated full

                        else if (whole |> String.length) > 12 then
                            [ [ whole |> String.dropRight 12
                              , whole
                                    |> String.dropRight 10
                                    |> String.right 2
                              ]
                                |> String.join "."
                            , "T"
                            ]
                                |> String.concat
                                |> Just
                                |> Truncated full

                        else if (whole |> String.length) > 9 then
                            [ [ whole |> String.dropRight 9
                              , whole
                                    |> String.dropRight 7
                                    |> String.right 2
                              ]
                                |> String.join "."
                            , "B"
                            ]
                                |> String.concat
                                |> Just
                                |> Truncated full

                        else if (whole |> String.length) > 6 then
                            [ [ whole |> String.dropRight 6
                              , whole
                                    |> String.dropRight 4
                                    |> String.right 2
                              ]
                                |> String.join "."
                            , "M"
                            ]
                                |> String.concat
                                |> Just
                                |> Truncated full

                        else if (whole |> String.length) > 3 then
                            [ whole |> String.dropRight 3
                            , whole |> String.right 3
                            ]
                                |> String.join ","
                                |> Just
                                |> Truncated full

                        else if (fraction |> String.length) <= 3 then
                            Truncated full Nothing

                        else
                            [ whole
                            , fraction |> String.left 3
                            ]
                                |> String.join "."
                                |> Just
                                |> Truncated full

                    whole :: _ ->
                        if (whole |> String.length) > 15 then
                            [ [ whole |> String.dropRight 15
                              , whole
                                    |> String.dropRight 13
                                    |> String.right 2
                              ]
                                |> String.join "."
                            , "Q"
                            ]
                                |> String.concat
                                |> Just
                                |> Truncated full

                        else if (whole |> String.length) > 12 then
                            [ [ whole |> String.dropRight 12
                              , whole
                                    |> String.dropRight 10
                                    |> String.right 2
                              ]
                                |> String.join "."
                            , "T"
                            ]
                                |> String.concat
                                |> Just
                                |> Truncated full

                        else if (whole |> String.length) > 9 then
                            [ [ whole |> String.dropRight 9
                              , whole
                                    |> String.dropRight 7
                                    |> String.right 2
                              ]
                                |> String.join "."
                            , "B"
                            ]
                                |> String.concat
                                |> Just
                                |> Truncated full

                        else if (whole |> String.length) > 6 then
                            [ [ whole |> String.dropRight 6
                              , whole
                                    |> String.dropRight 4
                                    |> String.right 2
                              ]
                                |> String.join "."
                            , "M"
                            ]
                                |> String.concat
                                |> Just
                                |> Truncated full

                        else if (whole |> String.length) > 3 then
                            [ whole |> String.dropRight 3
                            , whole |> String.right 3
                            ]
                                |> String.join ","
                                |> (\formattedFull -> Truncated formattedFull Nothing)

                        else
                            Truncated full Nothing

                    _ ->
                        Truncated full Nothing
           )


symbol : String -> Truncated
symbol string =
    { full = string
    , truncated =
        if (string |> String.length) >= 5 then
            string
                |> String.left 5
                |> Just

        else
            Nothing
    }
