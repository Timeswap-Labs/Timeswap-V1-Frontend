module Utility.Fade exposing (cut)


cut : String -> ( String, String )
cut full =
    full
        |> String.split "."
        |> (\list ->
                case list of
                    whole :: fraction :: _ ->
                        ( [ whole
                          , fraction |> String.left 2
                          ]
                            |> String.join "."
                        , fraction |> String.dropLeft 2
                        )

                    whole :: _ ->
                        ( whole
                        , ""
                        )

                    _ ->
                        ( full
                        , ""
                        )
           )
