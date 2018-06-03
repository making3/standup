module Main exposing (..)

import Dom exposing (focus)
import Html exposing (Attribute, Html, a, button, div, h3, i, input, li, span, text, ul)
import Html.Attributes exposing (class, disabled, hidden, id, value)
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
    , currentStandupTask : String
    , error : Maybe String
    }


initialModel : Model
initialModel =
    { todo = []
    , completed = []
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
        , input [ id taskInputId, onEnter Add, onInput ChangeStandupTask, value model.currentStandupTask ] []
        ]


viewError : Model -> Html Msg
viewError model =
    case model.error of
        Just error ->
            div [] [ text error ]

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
        [ h3 [ class "title is-4" ] [ text "TODO" ]
        , ul [] todoTasks
        ]


viewTodoTask : String -> Html Msg
viewTodoTask task =
    li [ class "task" ]
        [ text task
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
        [ h3 [ class "title is-4" ] [ text "Completed" ]
        , ul [] completedTasks
        ]


viewCompletedTask : String -> Html Msg
viewCompletedTask task =
    li [ class "task" ]
        [ text task
        , button [ class "button is-small", onClick (Delete task) ]
            [ viewIcon "fas fa-trash-alt" ]
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
    | FocusResult (Result Dom.Error ())


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

        FocusResult result ->
            case result of
                Err (Dom.NotFound id) ->
                    { model | error = Just ("Could not find dom id: " ++ id) } ! []

                Ok () ->
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


removeTask : String -> List String -> List String
removeTask taskToRemove tasks =
    List.filter (\task -> task /= taskToRemove) tasks



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
