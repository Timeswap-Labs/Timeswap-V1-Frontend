module Pages.AllMarket.Main exposing (Msg, Page, init, update)


type Page
    = Page


init : Page
init =
    Page


type Msg
    = Msg


update : Msg -> Page -> ( Page, Cmd Msg )
update msg page =
    case msg of
        Msg ->
            ( Page, Cmd.none )
