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
    , data : Result String (List WidgetListItem)
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
            -- SEND MULTIPLY MESSAGES -> MAP ALL ACTIVE WIDGETS
            -- let
            --
            --   reqAccount = makeRequest accountName
            --
            -- in
            ( model, Cmd.none )

        SetWidgetData accountName widgetName (Ok list) ->
            ( model, Cmd.none )

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
    div [ class "account-list" ] (List.map viewAccount accoutns)


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

        ( url, decoder, msg ) =
            case widget of
                _ ->
                    let
                        msg =
                            SetWidgetData accountName widget
                    in
                        ( "https://medium.com/@" ++ id ++ "/latest?format=json", mediumDecoder, msg )

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
        |> Json.Decode.Pipeline.required "uniqueSlug" Decode.string
