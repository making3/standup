module Main exposing (..)

import Dom exposing (focus)
import Html exposing (Attribute, Html, button, div, h6, input, span, text)
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
    div []
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
    div []
        [ h6 [] [ text "TODO" ]
        , div [] (List.map viewTodoTask model.todo)
        ]


viewTodoTask : String -> Html Msg
viewTodoTask task =
    div []
        [ text task
        , button [ onClick (Complete task) ] [ text "Complete" ]
        ]


viewCompletedTasks : Model -> Html Msg
viewCompletedTasks model =
    div []
        [ h6 [] [ text "Completed" ]
        , div [] (List.map viewCompletedTask model.completed)
        ]


viewCompletedTask : String -> Html Msg
viewCompletedTask task =
    div []
        [ text task
        , button [ onClick (Delete task) ] [ text "Remove" ]
        ]


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
