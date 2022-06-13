module Data.Images exposing (Flags, Images, init)

import Sort
import Sort.Dict as Dict exposing (Dict)


type alias Images =
    { images : Dict String String
    , pngimages : Dict String String
    , gifs : Dict String String
    , tokens : Dict String String
    , chains : Dict String String
    , wallets : Dict String String
    }


type alias Flags =
    List ( String, String )


init :
    { images : Flags
    , pngimages : Flags
    , gifs : Flags
    , tokenImages : Flags
    , chainImages : Flags
    , walletImages : Flags
    }
    -> Images
init { images, pngimages, gifs, tokenImages, chainImages, walletImages } =
    { images = images |> Dict.fromList Sort.alphabetical
    , pngimages = pngimages |> Dict.fromList Sort.alphabetical
    , gifs = gifs |> Dict.fromList Sort.alphabetical
    , tokens = tokenImages |> Dict.fromList Sort.alphabetical
    , chains = chainImages |> Dict.fromList Sort.alphabetical
    , wallets = walletImages |> Dict.fromList Sort.alphabetical
    }
