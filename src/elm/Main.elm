module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as DecodePipe


-- APP


main : Program Never Model Msg
main =
    Html.program { init = init, view = view, update = update, subscriptions = subscriptions }


type alias WidgetListItem =
    { title : String
    , url : String
    , image_url : Maybe String
    }


type AccountType
    = Strava
    | GitHub
    | Medium
    | LastFM
    | Twitter
    | Instagram


type WidgetType
    = GitHubRepos
    | MedimPosts
    | MediumRecommended
    | LastFMAlbumChart
    | LastFMArtistChart
    | LastFMTrackChart
    | TwitterPosts
    | TwitterSaves
    | InstagramPosts
    | LastFMgetRecentTracks
    | LastFMgetWeeklyTrackChart
    | LastFMgetWeeklyAlbumChart
    | LastFMgetWeeklyArtistChart


type alias Widget =
    { name : WidgetType
    , active : Bool
    , data : Result Http.Error (List WidgetListItem)
    }


type Auth
    = Token (Maybe String)
    | Username String


type alias Account =
    { name : AccountType
    , active : Bool
    , auth : Auth
    , url : Maybe String
    , widgets : List Widget
    }



-- MODEL


type alias Model =
    { accounts : List Account }


model : Model
model =
    { accounts =
        [ { name = GitHub
          , active = True
          , auth = Username "mutebg"
          , url = Nothing
          , widgets =
                [ { name = GitHubRepos
                  , active = True
                  , data = Ok []
                  }
                ]
          }
        , { name = Instagram
          , active = True
          , auth = Username "mutebg"
          , url = Nothing
          , widgets =
                [ { name = InstagramPosts
                  , active = True
                  , data = Ok []
                  }
                ]
          }
        , { name = Twitter
          , active = True
          , auth = Username "mutebg"
          , url = Nothing
          , widgets =
                [ { name = TwitterPosts
                  , active = True
                  , data = Ok []
                  }
                , { name = TwitterSaves
                  , active = True
                  , data = Ok []
                  }
                ]
          }
        , { name = Medium
          , active = True
          , auth = Username "mutebg"
          , url = Nothing
          , widgets =
                [ { name = MedimPosts
                  , active = True
                  , data = Ok []
                  }
                , { name = MediumRecommended
                  , active = True
                  , data = Ok []
                  }
                ]
          }
        , { name = LastFM
          , active = True
          , auth = Username "mutebg"
          , url = Nothing
          , widgets =
                [ { name = LastFMgetRecentTracks
                  , active = True
                  , data = Ok []
                  }
                , { name = LastFMgetWeeklyTrackChart
                  , active = True
                  , data = Ok []
                  }
                , { name = LastFMgetWeeklyAlbumChart
                  , active = True
                  , data = Ok []
                  }
                , { name = LastFMgetWeeklyArtistChart
                  , active = True
                  , data = Ok []
                  }
                ]
          }
        , { name = Strava
          , active = True
          , auth = Token Nothing
          , url = Just "http://strava.com"
          , widgets = []
          }
        ]
    }


init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )



-- UPDATE


type Msg
    = NoOp
    | ToggleAccount AccountType
    | ChangeUserName AccountType String
    | ToggleWidget AccountType WidgetType
    | LoadWidgetData AccountType
    | SetWidgetData AccountType WidgetType (Result Http.Error (List WidgetListItem))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleAccount accountName ->
            ( { model | accounts = findAndUpdateAccount model.accounts accountName toogleAccount }, Cmd.none )

        ChangeUserName accountName input ->
            let
                localChangeInput =
                    changeUserName input
            in
                ( { model | accounts = findAndUpdateAccount model.accounts accountName localChangeInput }, Cmd.none )

        ToggleWidget accountName widgetName ->
            let
                localUpdateWidgets =
                    toggleWidget widgetName
            in
                ( { model | accounts = findAndUpdateAccount model.accounts accountName localUpdateWidgets }, Cmd.none )

        LoadWidgetData accountName ->
            let
                account =
                    model.accounts
                        |> List.filter (\acc -> acc.name == accountName)
                        |> List.head

                msgs =
                    case account of
                        Just acc ->
                            acc.widgets
                                |> List.filter (\a -> a.active)
                                |> List.map (\a -> makeRequest acc.auth accountName a.name)

                        _ ->
                            [ Cmd.none ]
            in
                ( model, Cmd.batch msgs )

        SetWidgetData accountName widgetName list ->
            let
                localUpdateWidgets =
                    updateWidgetData list widgetName
            in
                ( { model | accounts = findAndUpdateAccount model.accounts accountName localUpdateWidgets }, Cmd.none )

        _ ->
            ( model, Cmd.none )


findAndUpdateAccount : List Account -> AccountType -> (Account -> Account) -> List Account
findAndUpdateAccount list accountName fn =
    List.map
        (\acc ->
            if acc.name == accountName then
                fn acc
            else
                acc
        )
        list


toogleAccount : Account -> Account
toogleAccount account =
    { account | active = not account.active }


changeUserName : String -> Account -> Account
changeUserName input account =
    { account | auth = Username input }


toggleWidget : WidgetType -> Account -> Account
toggleWidget widgetName account =
    let
        newWidgets =
            List.map
                (\widget ->
                    if widget.name == widgetName then
                        { widget | active = not widget.active }
                    else
                        widget
                )
                account.widgets
    in
        { account | widgets = newWidgets }


updateWidgetData : Result Http.Error (List WidgetListItem) -> WidgetType -> Account -> Account
updateWidgetData data widgetName account =
    let
        newWidgets =
            List.map
                (\widget ->
                    if widget.name == widgetName then
                        { widget | data = data }
                    else
                        widget
                )
                account.widgets
    in
        { account | widgets = newWidgets }



-- SUBSCRIBTION


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.none ]



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewAccounts model.accounts
        ]


viewAccounts : List Account -> Html Msg
viewAccounts accoutns =
    div []
        [ div [ class "account-list" ]
            (List.map viewAccount accoutns)
        , div
            [ class "widget-list" ]
            (accoutns
                |> List.filter (\a -> a.active)
                |> List.concatMap (\a -> List.filter (\b -> b.active) a.widgets)
                |> List.map viewWidget
            )
        ]


viewAccount : Account -> Html Msg
viewAccount { name, active, auth, widgets } =
    let
        onInputChange =
            ChangeUserName name
    in
        div []
            [ h2 [] [ text <| toString name ]
            , toggleButton (ToggleAccount name) active
            , case auth of
                Username name ->
                    input [ value name, onInput onInputChange ] []

                _ ->
                    text "No Implemented yet"
            , ul [] (viewAccountWidgetsList name widgets)
            , button [ onClick (LoadWidgetData name) ] [ text "Load data" ]
            , hr [] []
            ]


viewAccountWidgetsList : AccountType -> List Widget -> List (Html Msg)
viewAccountWidgetsList accountName widgets =
    List.map
        (\{ name, active } ->
            li []
                [ span []
                    [ text <| toString name ]
                , toggleButton
                    (ToggleWidget accountName name)
                    active
                ]
        )
        widgets


toggleButton : Msg -> Bool -> Html Msg
toggleButton msg active =
    button [ onClick msg ]
        [ text <|
            if active then
                "Deactivate"
            else
                "Activate"
        ]


viewWidget : Widget -> Html Msg
viewWidget { name, data } =
    div [ class "widget" ]
        [ h3 [ class "widget__title" ] [ text <| toString name ]
        , case data of
            Ok list ->
                ul []
                    (list
                        |> List.map
                            (\item ->
                                li []
                                    [ a [ href item.url ] [ text item.title ]
                                    ]
                            )
                    )

            _ ->
                div [ class "alert alert--error" ] [ text "ERROR" ]
        ]



--


makeRequest : Auth -> AccountType -> WidgetType -> Cmd Msg
makeRequest auth accountName widget =
    let
        id =
            case auth of
                Username name ->
                    name

                Token (Just token) ->
                    token

                _ ->
                    ""

        msg =
            SetWidgetData accountName widget

        baseUrl =
            "http://localhost:5002/personal-dashboard-ebee0/us-central1/api/"

        ( url, decoder ) =
            case widget of
                GitHubRepos ->
                    ( baseUrl ++ "github/" ++ id ++ "/repos", standartDecoder )

                InstagramPosts ->
                    ( baseUrl ++ "instagram/" ++ id ++ "/recent", standartDecoder )

                TwitterPosts ->
                    ( baseUrl ++ "twitter/" ++ id ++ "/list", standartDecoder )

                TwitterSaves ->
                    ( baseUrl ++ "twitter/" ++ id ++ "/favorites", standartDecoder )

                MedimPosts ->
                    ( baseUrl ++ "medium/" ++ id ++ "/latest", standartDecoder )

                MediumRecommended ->
                    ( baseUrl ++ "medium/" ++ id ++ "/has-recommended", standartDecoder )

                LastFMgetRecentTracks ->
                    ( baseUrl ++ "lastfm/" ++ id ++ "/getRecentTracks", standartDecoder )

                LastFMgetWeeklyTrackChart ->
                    ( baseUrl ++ "lastfm/" ++ id ++ "/getWeeklyTrackChart", standartDecoder )

                LastFMgetWeeklyAlbumChart ->
                    ( baseUrl ++ "lastfm/" ++ id ++ "/getWeeklyAlbumChart", standartDecoder )

                LastFMgetWeeklyArtistChart ->
                    ( baseUrl ++ "lastfm/" ++ id ++ "/getWeeklyArtistChart", standartDecoder )

                _ ->
                    ( "http://google.com", standartDecoder )

        req =
            Http.request
                { method = "GET"
                , body = Http.emptyBody
                , url = url
                , expect = Http.expectJson decoder
                , headers = []
                , timeout = Nothing
                , withCredentials = False
                }
    in
        Http.send msg req


standartDecoder : Decode.Decoder (List WidgetListItem)
standartDecoder =
    Decode.list standarItemDecoder


standarItemDecoder : Decode.Decoder WidgetListItem
standarItemDecoder =
    DecodePipe.decode WidgetListItem
        |> DecodePipe.required "title" Decode.string
        |> DecodePipe.required "url" Decode.string
        |> DecodePipe.optional "image_url" (Decode.nullable Decode.string) Nothing
