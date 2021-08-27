module Data.Deadline exposing
    ( Deadline
    , Option(..)
    , Output(..)
    , init
    , input
    , toOutput
    )


type Deadline
    = Deadline_ Int
    | Short_
    | Medium_
    | Long_


type Output
    = Deadline String
    | Short
    | Medium
    | Long


type Option
    = ShortOption
    | MediumOption
    | LongOption


init : Option -> Deadline
init option =
    case option of
        ShortOption ->
            Short_

        MediumOption ->
            Medium_

        LongOption ->
            Long_


input : String -> Maybe Deadline
input string =
    string
        |> String.toInt
        --|> Maybe.andThen ((<) 0 |> Utility.require)
        --|> Maybe.andThen ((>=) 180 |> Utility.require)
        |> Maybe.map Deadline_


toOutput : Deadline -> Output
toOutput deadline =
    case deadline of
        Deadline_ minute ->
            Deadline <| String.fromInt minute

        Short_ ->
            Short

        Medium_ ->
            Medium

        Long_ ->
            Long
