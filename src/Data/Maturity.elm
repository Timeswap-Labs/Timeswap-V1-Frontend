module Data.Maturity exposing (Maturity, fromFragment, toFragment)


type Maturity
    = Maturity String


fromFragment : String -> Maybe Maturity
fromFragment string =
    string
        |> String.split "="
        |> (\list ->
                case list of
                    "maturity" :: number :: _ ->
                        if number |> isUint then
                            number
                                |> Maturity
                                |> Just

                        else
                            Nothing

                    _ ->
                        Nothing
           )


toFragment : Maturity -> String
toFragment (Maturity string) =
    "maturity=" ++ string


isUint : String -> Bool
isUint string =
    string
        |> String.all (\char -> char |> Char.isDigit)
        |> Debug.log "add number range restriction"
