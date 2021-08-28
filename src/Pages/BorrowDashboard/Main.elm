module Pages.BorrowDashboard.Main exposing (Msg, Page, init, toUrl, update)


type Page
    = Page


init : Page
init =
    Page


toUrl : String
toUrl =
    "#dashboard?transaction=borrow"


type Msg
    = Msg


update : Msg -> Page -> ( Page, Cmd Msg )
update msg model =
    case msg of
        Msg ->
            ( model, Cmd.none )
