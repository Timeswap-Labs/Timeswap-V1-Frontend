module Pages.BorrowDashboard.Main exposing (Msg, Page, init, update)


type Page
    = Page


init : Page
init =
    Page


type Msg
    = Msg


update : Msg -> Page -> ( Page, Cmd Msg )
update msg model =
    case msg of
        Msg ->
            ( model, Cmd.none )
