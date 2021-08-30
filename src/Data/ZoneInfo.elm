module Data.ZoneInfo exposing (ZoneInfo)

import Time exposing (Zone, ZoneName)


type alias ZoneInfo =
    { zone : Zone
    , zoneName : ZoneName
    }
