module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


-- APP


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = model, view = view, update = update }


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
                  }
                , { name = MediumRecommended
                  , active = False
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



-- UPDATE


type Msg
    = NoOp
    | ToggleAccount AccountType
    | ChangeUserName AccountType String
    | ToggleWidget AccountType WidgetType


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleAccount accountName ->
            { model | accounts = findAndUpdateAccount model.accounts accountName toogleAccount }

        ChangeUserName accountName input ->
            let
                localChangeInput =
                    changeUserName input
            in
                { model | accounts = findAndUpdateAccount model.accounts accountName localChangeInput }

        ToggleWidget accountName widgetName ->
            let
                localUpdateWidgets =
                    toggleWidget widgetName
            in
                { model | accounts = findAndUpdateAccount model.accounts accountName localUpdateWidgets }

        _ ->
            model


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
