module Data.Or exposing (Or(..), map)


type Or a b
    = Either a
    | Or b


map : (a -> a1) -> (b -> b1) -> Or a b -> Or a1 b1
map functorEither functorOr or =
    case or of
        Either a ->
            a |> functorEither |> Either

        Or b ->
            b |> functorOr |> Or
