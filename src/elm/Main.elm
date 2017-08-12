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


type WidgetView
    = GalleryView
    | ListView


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
    | SetlistFMattended


type alias Widget =
    { name : WidgetType
    , active : Bool
    , view : WidgetView
    , data : Result Http.Error (List WidgetListItem)
    }


type AccountType
    = Strava
    | GitHub
    | Medium
    | LastFM
    | Twitter
    | Instagram
    | SetlistFM


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


type alias UI =
    { showSettings : Bool
    }


type alias Model =
    { accounts : List Account
    , name : String
    , ui : UI
    }



-- MODEL


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
                  , view = ListView
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
                  , view = GalleryView
                  , data = Ok []
                  }
                ]
          }
        , { name = SetlistFM
          , active = True
          , auth = Username "fb:1243029572"
          , url = Nothing
          , widgets =
                [ { name = SetlistFMattended
                  , active = True
                  , view = ListView
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
                  , view = ListView
                  , data = Ok []
                  }
                , { name = TwitterSaves
                  , active = True
                  , view = ListView
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
                  , view = ListView
                  , data = Ok []
                  }
                , { name = MediumRecommended
                  , active = True
                  , view = ListView
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
                  , view = ListView
                  , data = Ok []
                  }
                , { name = LastFMgetWeeklyTrackChart
                  , active = True
                  , view = ListView
                  , data = Ok []
                  }
                , { name = LastFMgetWeeklyAlbumChart
                  , active = True
                  , view = ListView
                  , data = Ok []
                  }
                , { name = LastFMgetWeeklyArtistChart
                  , active = True
                  , view = ListView
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
    , name = "Stoyan Delev"
    , ui =
        { showSettings = False
        }
    }


init : ( Model, Cmd Msg )
init =
    ( model, loadAllWidgetData model )



-- UPDATE


type Msg
    = NoOp
    | ToggleAccount AccountType
    | ChangeUserName AccountType String
    | ToggleWidget AccountType WidgetType
    | LoadAllWidgetData
    | LoadAccountData AccountType
    | LoadWidgetData WidgetType
    | SetWidgetData AccountType WidgetType (Result Http.Error (List WidgetListItem))
    | ToogleSettings


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

        LoadAllWidgetData ->
            let
                msgs =
                    loadAllWidgetData model
            in
                ( model, msgs )

        LoadAccountData accountName ->
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

        LoadWidgetData widgetName ->
            let
                account =
                    model.accounts
                        |> List.filter
                            (\acc ->
                                List.length (List.filter (\w -> w.name == widgetName) acc.widgets) > 0
                            )
                        |> List.head

                msg =
                    case account of
                        Just acc ->
                            makeRequest acc.auth acc.name widgetName

                        _ ->
                            Cmd.none
            in
                ( model, msg )

        SetWidgetData accountName widgetName list ->
            let
                localUpdateWidgets =
                    updateWidgetData list widgetName
            in
                ( { model | accounts = findAndUpdateAccount model.accounts accountName localUpdateWidgets }, Cmd.none )

        ToogleSettings ->
            let
                current =
                    model.ui

                ui =
                    { current | showSettings = not current.showSettings }
            in
                ( { model | ui = ui }, Cmd.none )

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


loadAllWidgetData : Model -> Cmd Msg
loadAllWidgetData model =
    let
        accounts =
            model.accounts
                |> List.filter (\acc -> acc.active)
                |> List.map (\acc -> { acc | widgets = List.filter (\b -> b.active) acc.widgets })

        msgs =
            accounts
                |> List.concatMap
                    (\acc ->
                        List.map (\widget -> makeRequest acc.auth acc.name widget.name) acc.widgets
                    )
    in
        Cmd.batch msgs



-- SUBSCRIBTION


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.none ]



-- VIEW


view : Model -> Html Msg
view { accounts, name, ui } =
    let
        showSettingsClass =
            "main"
                ++ (if ui.showSettings then
                        " main--open"
                    else
                        ""
                   )
    in
        div [ class showSettingsClass ]
            [ div [ class "dashboard lp" ]
                [ viewHeader name ui
                , viewDashboard accounts
                ]
            , div
                [ class "settings lp" ]
                [ viewSettingsList accounts
                ]
            ]


viewHeader : String -> UI -> Html Msg
viewHeader name ui =
    div [ class "header" ]
        [ h1 [] [ text name ]
        , div [ class "header__actions" ]
            [ button [ onClick LoadAllWidgetData ] [ text "Reaload all" ]
            , button [ onClick ToogleSettings ]
                [ text
                    (if ui.showSettings then
                        "CLOSE Settings"
                     else
                        "OPEN Settings"
                    )
                ]
            ]
        ]


viewDashboard : List Account -> Html Msg
viewDashboard accounts =
    div
        [ class "widget-list" ]
        (accounts
            |> List.filter (\a -> a.active)
            |> List.concatMap (\a -> List.filter (\b -> b.active) a.widgets)
            |> List.map viewWidget
        )


viewSettingsList : List Account -> Html Msg
viewSettingsList accounts =
    div [ class "account-list" ]
        (List.map viewAccount accounts)


viewAccount : Account -> Html Msg
viewAccount { name, active, auth, widgets } =
    let
        onInputChange =
            ChangeUserName name

        activeClass =
            if active then
                " account-box--active"
            else
                ""
    in
        div [ class ("account-box" ++ activeClass) ]
            [ div [ class "account-box__header" ]
                [ h2 [ class "account-box__title" ] [ text <| toString name ]
                , toggleButton (ToggleAccount name) active
                ]
            , div [ class "account-box__content" ]
                [ case auth of
                    Username name ->
                        input [ class "account-box__input", value name, onInput onInputChange ] []

                    _ ->
                        text "No Implemented yet"
                , ul [ class "account-box__list" ] (viewAccountWidgetsList name widgets)
                , button [ onClick (LoadAccountData name) ] [ text "Load data" ]
                ]
            ]


viewAccountWidgetsList : AccountType -> List Widget -> List (Html Msg)
viewAccountWidgetsList accountName widgets =
    List.map
        (\{ name, active } ->
            li [ class "account-box__list__item" ]
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
    let
        ( label, className ) =
            if active then
                ( "Deactivate", "toggle--active" )
            else
                ( "Activate", "" )
    in
        span [ class <| "toggle " ++ className, onClick msg ]
            [ text label
            , span [ class "toggle__symbol" ] [ text "" ]
            ]


viewWidget : Widget -> Html Msg
viewWidget { name, data, view } =
    div [ class "widget" ]
        [ div [ class "widget__content" ]
            [ div [ class "widget__title" ]
                [ text <| toString name
                , div [ class "widget__actions" ]
                    [ button [ onClick (LoadWidgetData name) ] [ text "reload" ]
                    ]
                ]
            , case data of
                Ok list ->
                    case view of
                        GalleryView ->
                            viewWidgetGallery list

                        _ ->
                            viewWidgetList list

                _ ->
                    div [ class "alert alert--error" ] [ text "ERROR" ]
            ]
        ]


viewWidgetList : List WidgetListItem -> Html msg
viewWidgetList list =
    ul [ class "widget__list" ]
        (list
            |> List.map
                (\item ->
                    li [ class "widget__list__item" ]
                        [ a [ href item.url ] [ text item.title ]
                        ]
                )
        )


viewWidgetGallery : List WidgetListItem -> Html msg
viewWidgetGallery list =
    div [ class "widget__gallery" ]
        (list
            |> List.map
                (\item ->
                    a [ class "widget__gallery__item", href item.url ]
                        [ (case item.image_url of
                            Just url ->
                                img [ src url, class "widget__gallery__img", height 224 ] []

                            _ ->
                                text ""
                          )
                        ]
                )
        )



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

                SetlistFMattended ->
                    ( baseUrl ++ "setlistfm/" ++ id, standartDecoder )

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
