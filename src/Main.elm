module Main exposing (..)

import Dom exposing (focus)
import Html exposing (Attribute, Html, a, button, div, h1, h3, i, input, li, span, text, ul)
import Html.Attributes exposing (class, disabled, hidden, id, placeholder, value)
import Html.Events exposing (keyCode, on, onClick, onInput)
import Http
import Json.Decode exposing (Decoder, andThen, fail, int, list, string, succeed)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode
import Navigation
import OAuth
import OAuth.Implicit
import Task


main =
    Navigation.program
        (always Nop)
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { todo : List String
    , completed : List String
    , eventualTasks : List String
    , currentStandupTask : String
    , error : Maybe String
    , token : Maybe OAuth.Token
    , oauth :
        { clientId : String
        , redirectUri : String
        }
    , id : String -- TODO: Make this a maybe
    }


initialModel : Navigation.Location -> Model
initialModel location =
    { todo = []
    , completed = []
    , eventualTasks = []
    , currentStandupTask = ""
    , error = Nothing
    , token = Nothing
    , oauth =
        { clientId = ""
        , redirectUri = location.origin ++ location.pathname
        }
    , id = ""
    }


type alias DriveFileIdentifier =
    { id : String }



--     {
--  "kind": "drive#file",
--  "id": "1NZLCpnaaKCMIb0OrKElEOcuuCKbG4zUggy2PNgJnYys",
--  "name": "CSCareerQuestions Salary Sharing Survey (Responses)",
--  "mimeType": "application/vnd.google-apps.spreadsheet"
-- },
-- TODO: Rename, this is horrible lol


type alias DriveSearch =
    { files : List DriveFileIdentifier
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        model =
            initialModel location
    in
    case OAuth.Implicit.parse location of
        Ok { token } ->
            let
                req =
                    Http.request
                        { method = "GET"
                        , body = Http.emptyBody
                        , headers = OAuth.use token []
                        , withCredentials = False
                        , url = filesEndpoint ++ "?spaces=appDataFolder&name = 'standup.json'"
                        , expect = Http.expectJson driveSearchDecoder
                        , timeout = Nothing
                        }
            in
            { model | token = Just token }
                ! [ Navigation.modifyUrl model.oauth.redirectUri
                  , Http.send GetDriveSearch req
                  , focusOnTaskInput
                  ]

        Err OAuth.Empty ->
            model ! []

        Err (OAuth.OAuthErr err) ->
            { model | error = Just <| OAuth.showErrCode err.error }
                ! [ Navigation.modifyUrl model.oauth.redirectUri ]

        Err _ ->
            { model | error = Just "parsing error" } ! []


authorizationEndpoint : String
authorizationEndpoint =
    "https://accounts.google.com/o/oauth2/v2/auth"


filesEndpoint : String
filesEndpoint =
    "https://www.googleapis.com/drive/v3/files"



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "box container" ]
        [ viewHeader model
        , h1 [] [ text ("ID" ++ model.id) ]
        , viewError model
        , viewCompletedTasks model
        , viewTodoTasks model
        , viewEventualTasks model
        , input
            [ id taskInputId
            , class "input"
            , onEnter Add
            , onInput ChangeStandupTask
            , value model.currentStandupTask
            , placeholder "What are you planning on doing?"
            ]
            []
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    case model.token of
        Just tokenOAuth ->
            case tokenOAuth of
                OAuth.Bearer token ->
                    div [] [ text token ]

        Nothing ->
            viewLoginButton


viewLoginButton : Html Msg
viewLoginButton =
    button [ onClick Authorize ] [ text "Authorize" ]


viewError : Model -> Html Msg
viewError model =
    case model.error of
        Just error ->
            div [ class "notification is-danger" ]
                [ button [ onClick ClearError, class "delete" ] []
                , text error
                ]

        Nothing ->
            text ""


viewTodoTasks : Model -> Html Msg
viewTodoTasks model =
    let
        todoTasks =
            case model.todo of
                [] ->
                    [ li [] [ text "No pending tasks" ] ]

                _ ->
                    List.map viewTodoTask model.todo
    in
    div [ class "content" ]
        [ h3 [ class "title is-4" ] [ text "What am I planning on doing?" ]
        , ul [] todoTasks
        ]


viewTodoTask : String -> Html Msg
viewTodoTask task =
    li [ class "task" ]
        [ text task
        , button [ class "button is-small", onClick (WaitOnTask task) ]
            [ viewIcon "fas fa-clock" ]
        , button [ class "button is-small", onClick (Complete task) ]
            [ viewIcon "fas fa-check" ]
        ]


viewCompletedTasks : Model -> Html Msg
viewCompletedTasks model =
    let
        completedTasks =
            case model.completed of
                [] ->
                    [ li [] [ text "No completed tasks" ] ]

                _ ->
                    List.map viewCompletedTask model.completed
    in
    div [ class "content" ]
        [ h3 [ class "title is-4" ] [ text "What have I completed?" ]
        , ul [] completedTasks
        ]


viewCompletedTask : String -> Html Msg
viewCompletedTask task =
    li [ class "task" ]
        [ text task
        , button [ class "button is-small", onClick (Delete task) ]
            [ viewIcon "fas fa-trash-alt" ]
        ]


viewEventualTasks : Model -> Html Msg
viewEventualTasks model =
    let
        eventualTasks =
            case model.eventualTasks of
                [] ->
                    [ li [] [ text "Nothing to do in the future" ] ]

                _ ->
                    List.map viewEventualTask model.eventualTasks
    in
    div [ class "content" ]
        [ h3 [ class "title is-4" ] [ text "What do I need to do in the near future?" ]
        , ul [] eventualTasks
        ]


viewEventualTask : String -> Html Msg
viewEventualTask task =
    li [ class "task" ]
        [ text task
        , button [ class "button is-small", onClick (BumpTaskToTodo task) ]
            [ viewIcon "fas fa-angle-double-up" ]
        ]


viewIcon : String -> Html Msg
viewIcon fontAwesomeIcon =
    span [ class "icon" ] [ i [ class fontAwesomeIcon ] [] ]


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                succeed msg
            else
                fail "not ENTER"
    in
    on "keydown" (andThen isEnter keyCode)



-- UPDATE


type Msg
    = Nop
    | ChangeStandupTask String
    | Add
    | Complete String
    | Delete String
    | WaitOnTask String
    | BumpTaskToTodo String
    | FocusResult (Result Dom.Error ())
    | ClearError
    | Authorize
    | GetDriveSearch (Result Http.Error DriveSearch)
    | StandupDataCreated (Result Http.Error DriveFileIdentifier)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Nop ->
            model ! []

        Add ->
            addNewTask model.currentStandupTask model ! []

        Complete task ->
            completeTask task model ! [ focusOnTaskInput ]

        Delete task ->
            { model | completed = removeTask task model.completed } ! [ focusOnTaskInput ]

        ChangeStandupTask newInput ->
            { model | currentStandupTask = newInput } ! []

        WaitOnTask task ->
            waitOnTask task model ! [ focusOnTaskInput ]

        BumpTaskToTodo task ->
            bumpTaskToTodo task model ! []

        FocusResult result ->
            case result of
                Err (Dom.NotFound id) ->
                    { model | error = Just ("Could not find dom id: " ++ id) } ! []

                Ok () ->
                    { model | error = Nothing } ! []

        ClearError ->
            { model | error = Nothing } ! []

        Authorize ->
            model
                ! [ OAuth.Implicit.authorize
                        { clientId = model.oauth.clientId
                        , redirectUri = model.oauth.redirectUri
                        , responseType = OAuth.Token
                        , scope =
                            [ "https://www.googleapis.com/auth/drive.appdata"
                            , "https://www.googleapis.com/auth/drive.readonly"
                            , "https://www.googleapis.com/auth/drive.metadata.readonly"
                            , "https://www.googleapis.com/auth/drive.file"
                            , "https://www.googleapis.com/auth/drive"
                            ]
                        , state = Nothing
                        , url = authorizationEndpoint
                        }
                  ]

        GetDriveSearch result ->
            case result of
                Err err ->
                    handleHttpError model err ! []

                Ok driveSearch ->
                    let
                        first =
                            List.head driveSearch.files

                        -- |> Maybe.withDefault
                        --     { id = "asdfasdfa"
                        --     }
                    in
                    case first of
                        Just actualFirst ->
                            { model | id = actualFirst.id } ! []

                        Nothing ->
                            let
                                _ =
                                    Debug.log "wtf" driveSearch.files
                            in
                            createStandupData model

        StandupDataCreated result ->
            case result of
                Err err ->
                    handleHttpError model err ! []

                Ok driveFile ->
                    { model | id = driveFile.id } ! []


handleHttpError : Model -> Http.Error -> Model
handleHttpError model errorHttp =
    case errorHttp of
        Http.Timeout ->
            { model | error = Just "Timeout" }

        Http.NetworkError ->
            { model | error = Just "NetworkError " }

        Http.BadPayload wat a ->
            { model | error = Just ("unexpected" ++ wat) }

        Http.BadStatus str ->
            { model | error = Just ("bad: " ++ str.body) }

        Http.BadUrl a ->
            { model | error = Just ("bad url: " ++ a) }


focusOnTaskInput : Cmd Msg
focusOnTaskInput =
    Task.attempt FocusResult (focus taskInputId)


taskInputId : String
taskInputId =
    "taskInput"


addNewTask : String -> Model -> Model
addNewTask newTask model =
    case model.currentStandupTask of
        "" ->
            model

        _ ->
            case List.any (\task -> task == model.currentStandupTask) model.todo of
                True ->
                    { model | error = Just "Task has already been added!" }

                False ->
                    { model | todo = model.currentStandupTask :: model.todo, currentStandupTask = "" }


completeTask : String -> Model -> Model
completeTask completedTask model =
    { model
        | todo = removeTask completedTask model.todo
        , completed = completedTask :: model.completed
    }


waitOnTask : String -> Model -> Model
waitOnTask task model =
    { model
        | todo = removeTask task model.todo
        , eventualTasks = task :: model.eventualTasks
    }


bumpTaskToTodo : String -> Model -> Model
bumpTaskToTodo task model =
    { model
        | todo = task :: model.todo
        , eventualTasks = removeTask task model.eventualTasks
    }


removeTask : String -> List String -> List String
removeTask taskToRemove tasks =
    List.filter (\task -> task /= taskToRemove) tasks


createStandupData : Model -> ( Model, Cmd Msg )
createStandupData model =
    case model.token of
        Just token ->
            let
                req =
                    Http.request
                        { method = "POST"
                        , body = { name = "standup.json", parents = [] } |> newFileEncoder |> Http.jsonBody
                        , headers = OAuth.use token []
                        , withCredentials = False
                        , url = filesEndpoint
                        , expect = Http.expectJson driveFileIdentifierDecoder
                        , timeout = Nothing
                        }
            in
            model ! [ Http.send StandupDataCreated req ]

        Nothing ->
            model ! []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- SERIALIZATION


type alias NewFile =
    { name : String
    , parents : List String
    }


newFileEncoder newFile =
    Json.Encode.object
        [ ( "name", Json.Encode.string newFile.name )
        , ( "parents", Json.Encode.list (List.map Json.Encode.string newFile.parents) )
        ]


driveFileEncoder fileIdentifier =
    Json.Encode.object
        [ ( "id", Json.Encode.string fileIdentifier.id ) ]


driveFileIdentifierDecoder : Decoder DriveFileIdentifier
driveFileIdentifierDecoder =
    decode DriveFileIdentifier
        |> required "id" string


driveSearchDecoder : Decoder DriveSearch
driveSearchDecoder =
    decode DriveSearch
        |> required "files" (list driveFileIdentifierDecoder)
