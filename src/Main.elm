module Main exposing (..)

import Html exposing (Html, button, div, h6, input, span, text)
import Html.Attributes exposing (class, value)
import Html.Events exposing (onClick, onInput)


main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { todo : List String
    , completed : List String
    , currentStandupTask : String
    }


model : Model
model =
    { todo = []
    , completed = []
    , currentStandupTask = ""
    }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewCompletedTasks model
        , viewTodoTasks model
        , input [ onInput ChangeStandupTask, value model.currentStandupTask ] []
        , button [ onClick Add ] [ text "Add" ]
        ]


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



-- UPDATE


type Msg
    = ChangeStandupTask String
    | Add
    | Complete String
    | Delete String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Add ->
            addNewTask model.currentStandupTask model

        Complete task ->
            completeTask task model

        Delete task ->
            { model | completed = removeTask task model.completed }

        ChangeStandupTask newInput ->
            { model | currentStandupTask = newInput }


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
