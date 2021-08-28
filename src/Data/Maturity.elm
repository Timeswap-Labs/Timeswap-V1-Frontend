module Data.Maturity exposing (Maturity, fromFragment, toFragment, toPosix, toString)

import Time exposing (Posix)


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


toString : Maturity -> String
toString (Maturity string) =
    string


isUint : String -> Bool
isUint string =
    string
        |> String.all (\char -> char |> Char.isDigit)
        |> Debug.log "add number range restriction"


toPosix : Maturity -> Maybe Posix
toPosix (Maturity string) =
    string
        |> String.toInt
        |> Maybe.map ((*) 1000)
        |> Maybe.map Time.millisToPosix
