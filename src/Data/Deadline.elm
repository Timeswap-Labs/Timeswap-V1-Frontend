module Data.Deadline exposing
    ( Deadline
    , Option(..)
    , fromOption
    , fromString
    , init
    , isCorrect
    , toOption
    , toString
    )


type Deadline
    = Deadline Int


type Option
    = Short
    | Medium
    | Long


init : Deadline
init =
    Deadline 1200


toOption : Deadline -> Maybe Option
toOption (Deadline seconds) =
    if seconds == 600 then
        Just Short

    else if seconds == 1200 then
        Just Medium

    else if seconds == 1800 then
        Just Long

    else
        Nothing


toString : Deadline -> Maybe String
toString (Deadline seconds) =
    if seconds == 600 || seconds == 1200 || seconds == 1800 then
        Nothing

    else
        seconds
            // 60
            |> String.fromInt
            |> Just


isCorrect : String -> Bool
isCorrect string =
    string
        |> String.toInt
        |> Maybe.map
            (\minutes ->
                if minutes > 0 && minutes <= 180 then
                    True

                else
                    False
            )
        |> Maybe.withDefault False


fromOption : Option -> Deadline
fromOption option =
    case option of
        Short ->
            Deadline 600

        Medium ->
            Deadline 1200

        Long ->
            Deadline 1800


fromString : String -> Deadline
fromString string =
    string
        |> String.toInt
        |> Maybe.andThen
            (\minutes ->
                if minutes > 0 && minutes <= 180 then
                    minutes
                        |> (*) 60
                        |> Deadline
                        |> Just

                else
                    Nothing
            )
        |> Maybe.withDefault (Deadline 1200)
