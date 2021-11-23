module Data.TokenParam exposing (TokenParam(..), fromFragment)


type TokenParam
    = Asset
    | Collateral


fromFragment : String -> Maybe TokenParam
fromFragment fragment =
    case fragment of
        "asset" ->
            Just Asset

        "collateral" ->
            Just Collateral

        _ ->
            Nothing
