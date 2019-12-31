module Main exposing (..)
import Browser exposing (..)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Url exposing (Url)
import Time
import String exposing (..)




-- MAIN


main : Program (Int) Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }

type Msg
    = LinkClicked UrlRequest
    | UrlChanged Url



-- MODEL


type alias Model =
    { day : String
    , key : Nav.Key
    , url : Url.Url
    }

init : (Int) -> Url -> Nav.Key -> (Model, Cmd Msg)
init dateNow url navKey =
    ( Model ( dateToString ( Time.millisToPosix dateNow ) Time.utc ) navKey url
    , Cmd.none
    )




-- UPDATE


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )
                Browser.External href ->
                    ( model, Nav.load href )
        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none




-- VIEW


view : Model -> Document msg
view model =
    Document "TITLE"
        [ div []
            [ div [ class "navbar-fixed" ]
                [ nav []
                    [ div [ class "nav-wrapper blue" ]
                        [ a [ href "#", class "brand-logo" ] [ text "Bluebook"]
                        , ul [ id "nav-mobile", class "right" ]
                            [ li [] [ a [ href "Today"] [ text "Today" ]]
                            , li [] [ a [ href "Calendar" ] [ text "Calendar", i [ class "material-icons right" ] [ text "today" ] ] ]
                            ]
                        ]
                    ]
                ]
            ]
            , div [ class "content-wrapper" ]
                [div [ class "section" ]
                    [ div [ class "container" ]
                        [ div [ class "card" ]
                            [ div [ class "card-content center" ]
                                (helpMe model)
                            ]
                        ]
                    ]
                ]
        ]

helpMe : Model -> List (Html msg)
helpMe model =
    case model.url.path of
        "/elm-bluebook/Calendar" ->
            [h3 [ ] [ text "Calendar"]
            ]
        "/elm-bluebook/Today" ->
            [ h3 [ ] [ text model.day ]
            , ul [ class "collapsible" ]
                [ li [ ]
                    [ div [ class "collapsible-header" ] [ text (model.url.path) ]
                    , div [ class "collapsible-body" ] [ text "First Body" ]
                    ]
                , li [ ]
                    [ div [ class "collapsible-header" ] [ text "Second Header" ]
                    , div [ class "collapsible-body" ] [ text "Second Body" ]
                    ]
                ]
            ]
        _ -> [ div [] [text model.url.path ] ]

dateToString : Time.Posix -> Time.Zone -> String
dateToString zone posix =
    weekdayToString zone posix ++ ", " ++ monthToString zone posix ++ " " ++ dayToString zone posix

monthToString : Time.Posix -> Time.Zone -> String
monthToString zone posix =
    case Time.toMonth posix zone of
        Time.Jan -> "January"
        Time.Feb -> "February"
        Time.Mar -> "March"
        Time.Apr -> "April"
        Time.May -> "May"
        Time.Jun -> "June"
        Time.Jul -> "July"
        Time.Aug -> "August"
        Time.Sep -> "September"
        Time.Oct -> "October"
        Time.Nov -> "November"
        Time.Dec -> "December"

dayToString : Time.Posix -> Time.Zone -> String
dayToString zone posix =
    String.fromInt (Time.toDay posix zone)

weekdayToString : Time.Posix -> Time.Zone -> String
weekdayToString zone posix =
    case Time.toWeekday posix zone of
        Time.Mon -> "Monday"
        Time.Tue -> "Tuesday"
        Time.Wed -> "Wednesday"
        Time.Thu -> "Thursday"
        Time.Fri -> "Friday"
        Time.Sat -> "Saturday"
        Time.Sun -> "Sunday"

type alias Bluebook int =
    { title : String
    , body : List (Html int)
    }