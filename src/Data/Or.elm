module Data.Or exposing (Or(..), mapEither)


type Or a b
    = Either a
    | Or b


mapEither : (a -> c) -> Or a b -> Or c b
mapEither functor or =
    case or of
        Either a ->
            functor a |> Either

        Or b ->
            Or b
