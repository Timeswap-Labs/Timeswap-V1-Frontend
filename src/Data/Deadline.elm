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
    Deadline 20


toOption : Deadline -> Maybe Option
toOption (Deadline minutes) =
    if minutes == 10 then
        Just Short

    else if minutes == 20 then
        Just Medium

    else if minutes == 30 then
        Just Long

    else
        Nothing


toString : Deadline -> Maybe String
toString (Deadline minutes) =
    if minutes == 10 || minutes == 20 || minutes == 30 then
        Nothing

    else
        minutes
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
            Deadline 10

        Medium ->
            Deadline 20

        Long ->
            Deadline 30


fromString : String -> Deadline
fromString string =
    string
        |> String.toInt
        |> Maybe.andThen
            (\minutes ->
                if minutes > 0 && minutes <= 180 then
                    minutes
                        |> Deadline
                        |> Just

                else
                    Nothing
            )
        |> Maybe.withDefault (Deadline 20)
