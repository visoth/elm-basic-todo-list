module Todo exposing (Model, Msg, init, update, view)

import Bulma.Columns exposing (..)
import Bulma.Components exposing (..)
import Bulma.Elements exposing (..)
import Bulma.Form exposing (..)
import Bulma.Layout exposing (..)
import Bulma.Modifiers exposing (..)
import Bulma.Modifiers.Typography exposing (textCentered)
import Html exposing (Attribute, Html, div, main_, text)
import Html.Attributes exposing (placeholder, src, value)
import Html.Events exposing (keyCode, on, onClick, onInput)
import Json.Decode as Json



---- MODEL ----


defaultTasks : List Task
defaultTasks =
    [ { id = 1, name = "Daily scrum meeting", state = Done }
    , { id = 2, name = "Check my mails", state = InProgress }
    , { id = 3, name = "Eat croissants", state = NoStarted }
    , { id = 4, name = "Start a new user story", state = NoStarted }
    ]


init : Model
init =
    { currentId = List.length defaultTasks
    , tasks = defaultTasks
    , newTaskName = Nothing
    }


type State
    = Done
    | InProgress
    | NoStarted


type alias Task =
    { id : Int
    , name : String
    , state : State
    }


type alias Model =
    { tasks : List Task
    , currentId : Int
    , newTaskName : Maybe String
    }


type Msg
    = Create
    | UpdateState Int State
    | UpdateNewTaskName String
    | KeyDown Int
    | Delete Int



---- UPDATE ----


newId : Model -> Model
newId model =
    { model | currentId = model.currentId + 1 }


appendTask : String -> Model -> Model
appendTask newName model =
    { model | tasks = { id = model.currentId, name = newName, state = NoStarted } :: model.tasks }


cannotCreate : Model -> Bool
cannotCreate model =
    case model.newTaskName of
        Just "" ->
            True

        Nothing ->
            True

        Just newName ->
            False


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Create ->
            case model.newTaskName of
                Nothing ->
                    ( model, Cmd.none )

                Just "" ->
                    ( model, Cmd.none )

                Just newName ->
                    ( model
                        |> newId
                        |> appendTask newName
                        |> (\state -> { state | newTaskName = Nothing })
                    , Cmd.none
                    )

        UpdateNewTaskName name ->
            ( { model | newTaskName = Just name }, Cmd.none )

        KeyDown key ->
            case key of
                13 ->
                    update Create model

                _ ->
                    ( model, Cmd.none )

        UpdateState id state ->
            let
                updateState task =
                    if task.id == id then
                        { task | state = state }

                    else
                        task
            in
            ( { model
                | tasks =
                    List.map updateState model.tasks
              }
            , Cmd.none
            )

        Delete id ->
            ( { model | tasks = List.filter (\task -> task.id /= id) model.tasks }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ hero
            { bold = False
            , size = Standard
            , color = Default
            }
            []
            [ heroBody []
                [ container []
                    [ title H1 [] [ text "My tasks" ]
                    , case List.length model.tasks of
                        0 ->
                            noMoreTask

                        _ ->
                            taskTable model
                    , taskForm model
                    ]
                ]
            ]
        ]


noMoreTask =
    title H3
        []
        [ text "No more tasks to do, well done !" ]


taskTable model =
    table
        { bordered = True
        , striped = True
        , narrow = True
        , hoverable = True
        , fullWidth = True
        }
        []
        [ tableHead []
            [ tableRow False
                []
                [ tableCellHead [] [ text "ID" ]
                , tableCellHead [] [ text "Name" ]
                , tableCellHead [] [ text "Status" ]
                ]
            ]
        , tableBody [] (List.map formatRow model.tasks)
        , tableFoot [] []
        ]


formatRow : Task -> Html Msg
formatRow task =
    tableRow False
        []
        [ tableCell [] [ text (String.fromInt task.id) ]
        , tableCell [] [ text task.name ]
        , tableCell []
            [ formatState task ]
        ]


formatState : Task -> Html Msg
formatState task =
    case task.state of
        Done ->
            button { buttonModifiers | color = Primary }
                [ onClick (Delete task.id) ]
                [ text "Done !" ]

        InProgress ->
            button { buttonModifiers | color = Warning }
                [ onClick (UpdateState task.id Done) ]
                [ text "InProgress" ]

        NoStarted ->
            button { buttonModifiers | color = Light }
                [ onClick (UpdateState task.id InProgress) ]
                [ text "NoStarted" ]


taskForm : Model -> Field Msg
taskForm model =
    let
        name =
            case model.newTaskName of
                Just a ->
                    a

                Nothing ->
                    ""
    in
    fields Centered
        []
        [ controlInput
            inputModifier
            []
            [ onInput UpdateNewTaskName
            , onKeyDown KeyDown
            , placeholder "put a new task"
            , value name
            ]
            []
        , control
            controlModifier
            []
            [ button { buttonModifiers | disabled = cannotCreate model }
                [ onClick Create ]
                [ text "Create it !" ]
            ]
        ]


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (Json.map tagger keyCode)



-- Default form style


controlModifier : ControlModifiers msg
controlModifier =
    { loading = Nothing
    , expanded = False
    , iconLeft = Nothing
    , iconRight = Nothing
    }


inputModifier : ControlInputModifiers msg
inputModifier =
    { size = Standard
    , state = Active
    , color = Default
    , expanded = False
    , rounded = False
    , readonly = False
    , disabled = False
    , iconLeft = Nothing
    , iconRight = Nothing
    }


buttonModifiers : ButtonModifiers msg
buttonModifiers =
    { disabled = False
    , outlined = False
    , inverted = False
    , rounded = False
    , static = False
    , size = Standard
    , state = Blur
    , color = Primary
    , iconLeft = Nothing
    , iconRight = Nothing
    }
