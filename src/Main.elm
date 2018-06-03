module Main exposing (..)

import Dom exposing (focus)
import Html exposing (Attribute, Html, a, button, div, h3, i, input, li, span, text, ul)
import Html.Attributes exposing (class, disabled, hidden, id, placeholder, value)
import Html.Events exposing (keyCode, on, onClick, onInput)
import Json.Decode
import Task


main =
    Html.program
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
    }


initialModel : Model
initialModel =
    { todo = []
    , completed = []
    , eventualTasks = []
    , currentStandupTask = ""
    , error = Nothing
    }


init : ( Model, Cmd Msg )
init =
    initialModel ! [ focusOnTaskInput ]



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "box container" ]
        [ viewError model
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
                Json.Decode.succeed msg
            else
                Json.Decode.fail "not ENTER"
    in
    on "keydown" (Json.Decode.andThen isEnter keyCode)



-- UPDATE


type Msg
    = ChangeStandupTask String
    | Add
    | Complete String
    | Delete String
    | WaitOnTask String
    | BumpTaskToTodo String
    | FocusResult (Result Dom.Error ())
    | ClearError


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
