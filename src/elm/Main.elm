module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required)


-- APP


main : Program Never Model Msg
main =
    Html.program { init = init, view = view, update = update, subscriptions = subscriptions }


type alias WidgetListItem =
    { title : String
    , url : String
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
    | InstagramPosts


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
        , { name = Twitter
          , active = True
          , auth = Username "mutebg"
          , url = Nothing
          , widgets =
                [ { name = TwitterPosts
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
                  , active = False
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
                    ( baseUrl ++ "github/" ++ id ++ "/repos", mediumDecoder )

                TwitterPosts ->
                    ( baseUrl ++ "twitter/" ++ id, mediumDecoder )

                MedimPosts ->
                    ( baseUrl ++ "medium/" ++ id ++ "/latest", mediumDecoder )

                MediumRecommended ->
                    ( baseUrl ++ "medium/" ++ id ++ "/has-recommended", mediumDecoder )

                _ ->
                    ( "http://google.com", mediumDecoder )

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


mediumDecoder : Decode.Decoder (List WidgetListItem)
mediumDecoder =
    Decode.list mediumItemDecoder


mediumItemDecoder : Decode.Decoder WidgetListItem
mediumItemDecoder =
    decode WidgetListItem
        |> Json.Decode.Pipeline.required "title" Decode.string
        |> Json.Decode.Pipeline.required "url" Decode.string
