module Services.Swap.GameToken exposing (GameToken(..), decoder, encode, toERC20)

import Data.ERC20 as ERC20 exposing (ERC20)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type GameToken
    = Shiba
    | Doge
    | Token3


encode : GameToken -> Value
encode gameToken =
    (case gameToken of
        Shiba ->
            "shiba"

        Doge ->
            "doge"

        Token3 ->
            "token3"
    )
        |> Encode.string


decoder : Decoder GameToken
decoder =
    Decode.string
        |> Decode.andThen
            (\tokenStr ->
                case tokenStr of
                    "shiba" ->
                        Shiba |> Decode.succeed

                    "doge" ->
                        Doge |> Decode.succeed

                    "token3" ->
                        Token3 |> Decode.succeed

                    _ ->
                        Decode.fail "Not a game token"
            )


toERC20 : GameToken -> ERC20
toERC20 gameToken =
    case gameToken of
        Shiba ->
            ERC20.shiba

        Doge ->
            ERC20.doge

        Token3 ->
            ERC20.shiba
