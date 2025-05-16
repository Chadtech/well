port module Main exposing (main)

import AddTask
import Array exposing (Array)
import Assignee exposing (Assignee)
import Browser exposing (Document)
import Browser.Events
import Css
import Css.Global
import Ext.UniqueList as UniqueList exposing (UniqueList)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Ev
import Json.Decode as JD exposing (Decoder)
import Status exposing (Status)
import Style as S
import Task_ exposing (Task)
import View.Button as Button
import View.Dialog as DialogView
import View.Dropdown as Dropdown
import View.Textarea as Textarea


{-| This is a port that leaves the Elm run time, to determine which
status column the user released their mouse click over. The (Int, Int) are
the x and y coordinates of the mouse click.
-}
port findColumnStatus : ( Int, Int ) -> Cmd msg


{-| This is a port from JavaScript to Elm, notifying the Elm run time what status
the user released their mouse click over.
-}
port columnStatus : (String -> msg) -> Sub msg



--------------------------------------------------------
-- TYPES --
--------------------------------------------------------


type alias Model =
    { jsonTextField : String

    -- These are the statuses and assignees available in the app
    -- They are a stored in a UniqueList to prevent duplicate elements.
    , statuses : UniqueList Status
    , assignees : UniqueList Assignee

    -- These are the tasks that will appear in the status columns.
    -- Since we have to sometimes get or set individual tasks, they
    -- are stored as an Array, which has an api designed for setting
    -- and getting elements at a specific index.
    , tasks : Array Task
    , dialog : Dialog
    , mouseDrag : MouseDrag
    , mousePos : Maybe Pos
    }


{-| The Dialog type represents all the possible dialog boxes that could
open in this app. It is modeled as a custom type to reflect the fact that
at any given time there can only be one dialog on the screen at once.
-}
type Dialog
    = Dialog__None
    | Dialog__Json JsonDialogModel
    | Dialog__AddTask AddTask.Model
    | Dialog__Error { error : String }


type alias JsonDialogModel =
    { jsonParseError : Maybe JD.Error

    -- Depending on how the json dialog was initialized, we may or
    -- may not want to render a "go back" button, because there
    -- might not have been anywhere the user came from, to go back to.
    , renderGoBackButton : Bool
    }


{-| The MouseDrag type represents the state of the mouse drag operation.
-}
type MouseDrag
    = MouseDrag__Ready
    | MouseDrag__Dragging TaskPositioning
    | MouseDrag__WaitingForStatusColumnInfo { taskIndex : Int }


{-| As an individual task is being dragged to a new status column, we need
to store information about:

1.  The index of the task with respect to the `Array` stored in the main `Model`.
2.  The x and y coordinates of the click on the task, relative to the task itself
    in the ui, so that the mouse position on the task will not change when the users
    starts dragging it.
3.  The width of the task, so that it can be rendered with the same width it had in
    the column, as it is being dragged.

-}
type alias TaskPositioning =
    { taskIndex : Int
    , taskX : Int
    , taskY : Int
    , taskWidth : Float
    }


{-| This is the main Msg type of this application. I like to think of `Msg` in Elm
as a list of all the things that can possibly happen. I therefore also try my best
to name them as past tense descriptions of what happened
-}
type Msg
    = ClickedUseJson
    | ChangedJsonText String
    | MousedDownOnTask TaskPositioning
    | MouseMoved Pos
    | MousedUp Pos
    | ColumnStatus Status
    | ClickedJsonView
    | ClickedCloseDialog
    | ClickedBackdrop
    | ClickedAddTask
    | GotAddTaskMsg AddTask.Msg
    | ClickedGoBackToJson


type alias Pos =
    { x : Int, y : Int }



--------------------------------------------------------
-- INIT --
--------------------------------------------------------


{-| Flags are the initializing values of the Elm app, kind of like a
config or a payload of data the Elm app needs to start.
-}
type alias Flags =
    { statuses : UniqueList Status
    , assignees : UniqueList Assignee
    , tasks : List Task
    , jsonText : String
    }


init : Result JD.Error Flags -> ( Model, Cmd msg )
init flagsDecodeResult =
    let
        model : Model
        model =
            case flagsDecodeResult of
                Ok flags ->
                    { jsonTextField = flags.jsonText
                    , statuses = flags.statuses
                    , assignees = flags.assignees
                    , tasks = Array.fromList flags.tasks
                    , dialog = Dialog__None
                    , mouseDrag = MouseDrag__Ready
                    , mousePos = Nothing
                    }

                Err err ->
                    { jsonTextField = ""
                    , statuses = UniqueList.empty
                    , assignees = UniqueList.empty
                    , tasks = Array.empty
                    , dialog =
                        Dialog__Json
                            { jsonParseError = Just err
                            , renderGoBackButton = False
                            }
                    , mouseDrag = MouseDrag__Ready
                    , mousePos = Nothing
                    }
    in
    ( model, Cmd.none )


initJsonText : String
initJsonText =
    """[
    {
        "title": "Express Myself",
        "description": "Set the building on fire.",
        "status": "To Do",
        "assignee": "Lyla Harper"
    },
    {
        "title": "Catch Up Work - Saturday",
        "description": "Gonna need you to come into work on Saturday",
        "status": "In Progress",
        "assignee": "Hayes Aguirre"
    },
    {
        "title": "Catch Up Work - Sunday",
        "description": "Gonna need you to com into work on Sunday too.",
        "status": "In Progress",
        "assignee": "Ariah Koch"
    },
    {
        "title": "TPS Reports",
        "description": "Did you get the memo?",
        "status": "Done",
        "assignee": "Salvador Vega"
    },
    {
        "title": "Buy some more \\"Flare\\"",
        "description": "Apparently, 13 is not the minimum number of Flare.",
        "status": "Done",
        "assignee": "Dakota Calhoun"
    },
    {
        "title": "Move desk into storage room B",
        "description": "See if you can take care of some of the rat problem while you're down here.",
        "status": "Done",
        "assignee": "Gary Crane"
    }
]"""



--------------------------------------------------------
-- HELPERS --
--------------------------------------------------------


{-| This function tries to parse a json string into the `Flags`, which
are the initializing values of this Elm app.
-}
parseJsonText : String -> Result JD.Error Flags
parseJsonText jsonText =
    let
        taskDecoder : Decoder Task
        taskDecoder =
            JD.map4 Task
                (JD.field "status" Status.decoder)
                (JD.field "title" JD.string)
                (JD.field "description" JD.string)
                (JD.field "assignee" Assignee.decoder)

        flagsDecoder : Decoder Flags
        flagsDecoder =
            let
                fromTasks : List Task -> Flags
                fromTasks tasks =
                    { statuses =
                        List.map (\task -> task.status) tasks
                            |> UniqueList.fromList
                    , assignees =
                        List.map (\task -> task.assignee) tasks
                            |> UniqueList.fromList
                    , tasks = tasks
                    , jsonText = jsonText
                    }
            in
            JD.list taskDecoder
                |> JD.map fromTasks
    in
    JD.decodeString flagsDecoder jsonText


taskBeingDragged : MouseDrag -> Maybe TaskPositioning
taskBeingDragged mouseDrag =
    case mouseDrag of
        MouseDrag__Ready ->
            Nothing

        MouseDrag__Dragging rec ->
            Just rec

        MouseDrag__WaitingForStatusColumnInfo _ ->
            Nothing


setMouseDrag : MouseDrag -> Model -> Model
setMouseDrag mouseDrag model =
    { model
        | mouseDrag = mouseDrag
    }


mapTask : Int -> (Task -> Task) -> Model -> Model
mapTask taskIndex f model =
    { model
        | tasks =
            case
                Array.get taskIndex model.tasks
                    |> Maybe.map f
            of
                Just task ->
                    model.tasks
                        |> Array.set taskIndex task

                Nothing ->
                    model.tasks
    }


setDialog : Dialog -> Model -> Model
setDialog dialog model =
    { model
        | dialog = dialog
    }


closeDialog : Model -> Model
closeDialog model =
    setDialog Dialog__None model



--------------------------------------------------------
-- UPDATE --
--------------------------------------------------------


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedUseJson ->
            case parseJsonText model.jsonTextField of
                Ok flags ->
                    -- If we can parse the json text into `Flags`, simply
                    -- restart the whole app with the new flags
                    init (Ok flags)

                Err error ->
                    -- If we cannot parse the json text into `Flags`, then
                    -- store the error so it can be rendered
                    ( model
                        |> setDialog
                            (Dialog__Json
                                { jsonParseError = Just error
                                , renderGoBackButton = True
                                }
                            )
                    , Cmd.none
                    )

        ChangedJsonText string ->
            ( { model | jsonTextField = string }
            , Cmd.none
            )

        MousedDownOnTask rec ->
            ( model
                |> setMouseDrag
                    (MouseDrag__Dragging rec)
            , Cmd.none
            )

        MouseMoved rec ->
            ( { model | mousePos = Just rec }
            , Cmd.none
            )

        MousedUp pos ->
            case taskBeingDragged model.mouseDrag of
                Just taskPositioning ->
                    ( model
                        |> setMouseDrag
                            (MouseDrag__WaitingForStatusColumnInfo
                                { taskIndex = taskPositioning.taskIndex }
                            )
                    , findColumnStatus ( pos.x, pos.y )
                    )

                Nothing ->
                    ( model
                        |> setMouseDrag MouseDrag__Ready
                    , Cmd.none
                    )

        ColumnStatus status ->
            case model.mouseDrag of
                MouseDrag__WaitingForStatusColumnInfo rec ->
                    ( mapTask
                        rec.taskIndex
                        (Task_.setStatus status)
                        model
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ClickedJsonView ->
            ( model
                |> setDialog
                    (Dialog__Json
                        { jsonParseError = Nothing
                        , renderGoBackButton = True
                        }
                    )
            , Cmd.none
            )

        ClickedCloseDialog ->
            ( model
                |> closeDialog
            , Cmd.none
            )

        ClickedBackdrop ->
            ( model
                |> closeDialog
            , Cmd.none
            )

        ClickedAddTask ->
            case ( UniqueList.first model.assignees, UniqueList.first model.statuses ) of
                -- The AddTask dialog uses the first assignee and first
                -- status as initial values.
                ( Just assignee, Just status ) ->
                    ( model
                        |> setDialog
                            (Dialog__AddTask <|
                                AddTask.init assignee status
                            )
                    , Cmd.none
                    )

                _ ->
                    ( model
                        |> setDialog
                            (Dialog__Error
                                { error =
                                    "Somehow there were either no assignees or statuses available to initialize the AddTask dialog with."
                                }
                            )
                    , Cmd.none
                    )

        GotAddTaskMsg subMsg ->
            case model.dialog of
                Dialog__AddTask subModel ->
                    let
                        ( newSubModel, out ) =
                            AddTask.update subMsg subModel
                    in
                    -- Here we handle the AddTask's "OutMsg", which is a way
                    -- for the AddTask dialog to communicate back to the main app.
                    case out of
                        AddTask.NoOut ->
                            ( model
                                |> setDialog (Dialog__AddTask newSubModel)
                            , Cmd.none
                            )

                        AddTask.NewTask task ->
                            ( { model
                                | tasks = Array.push task model.tasks
                              }
                                |> closeDialog
                            , Cmd.none
                            )

                        AddTask.Close ->
                            ( model
                                |> closeDialog
                            , Cmd.none
                            )

                _ ->
                    ( model, Cmd.none )

        ClickedGoBackToJson ->
            ( model
                |> setDialog
                    (Dialog__Json
                        { jsonParseError = Nothing
                        , renderGoBackButton = True
                        }
                    )
            , Cmd.none
            )



--------------------------------------------------------
-- VIEW --


{-| Some notes about the styling

1.  The CSS styling is written in Elm, using the elm-css library, which basically
    lets us write css styling in our code. The end result is writing CSS, but in Elm syntax.
    So, the CSS "width: 40px;" would be in Elm "Css.width (Css.px 40)". The styling is also
    written directly on the element that is being styled, so we don't really use classes
    with Elm-Css. There are classes in the rendered html, but they are generated at run time
    and are not human readable.

2.  There is an additional layer of abstraction for this apps styling, which is the `Style`
    module, imported as "S". Style.elm is inspired by the CSS framework "tailwind" (<https://tailwindcss.com/>).
    I think of Tailwind as taking just the good parts of CSS, and then representing them as
    really tiny tags- not even key-values. For example, the tailwind equivalent of "padding: 1rem;"
    is "p4". "p" stands for "padding" and "4" is the size of the padding, which is 1rem (p1 would
    be 1/4th of a rem). pt4 would be "padding-top: 1rem;". g1 is "gap: 0.25rem;" to list a few more
    examples.

-}



--------------------------------------------------------


document : Model -> Document Msg
document model =
    { title = "Todo List"
    , body = List.map Html.toUnstyled <| view model
    }


{-| Styling that is applied to the whole page like a stylesheet. These styles
are exceptional, as most styling in this app is done by applying it directly
to the html.
-}
globalStyles : Html Msg
globalStyles =
    let
        bg : Css.Style
        bg =
            S.bgNightwood1
    in
    Css.Global.global
        [ Css.Global.everything
            [ S.userSelectNone ]
        , Css.Global.body
            [ S.m0
            , S.col
            , S.g4
            , bg
            , S.relative
            , Css.height <| Css.vh 100
            , S.textGray4
            , Css.fontFamilies
                [ "'JetBrains Mono'"
                , "inter"
                , "monospace"
                , "sans-serif"
                ]
            ]
        , Css.Global.selector ".task:hover"
            [ Css.Global.children
                [ Css.Global.class "task-title"
                    [ S.textYellow5 ]
                ]
            ]
        , Dropdown.globalStyles
        ]


view : Model -> List (Html Msg)
view model =
    let
        draggedTask : Maybe ( TaskPositioning, Task )
        draggedTask =
            model.mouseDrag
                |> taskBeingDragged
                |> Maybe.andThen
                    (\taskPositioning ->
                        Array.get taskPositioning.taskIndex model.tasks
                            |> Maybe.map (Tuple.pair taskPositioning)
                    )
    in
    [ globalStyles
    , controlsView
    , statusColumnsView model
    , draggedTaskView model.mousePos draggedTask
    , dialogBackdrop model.dialog
    , dialogView model
    ]


dialogBackdrop : Dialog -> Html Msg
dialogBackdrop dialog =
    case dialog of
        Dialog__None ->
            Html.text ""

        _ ->
            Html.div
                [ Attr.css
                    [ S.absolute
                    , S.bgNightwood1
                    , Css.property "opacity" "0.5"
                    , S.wFull
                    , S.hFull
                    ]
                , Ev.onClick ClickedBackdrop
                ]
                []


dialogView : Model -> Html Msg
dialogView model =
    let
        -- Sorry about the confusing names "DialogView.Dialog" and "Dialog".
        -- The "Dialog" type is defined here in `Main.elm` and represents all
        -- the different kinds of dialogs that can be opened in the app.
        --
        -- The "DialogView.Dialog" type is defined in `View/Dialog.elm` and
        -- is like a view component for rendering Dialog-looking things.
        dialog : DialogView.Dialog Msg
        dialog =
            case model.dialog of
                Dialog__None ->
                    DialogView.none

                Dialog__Json subModel ->
                    let
                        bg : Css.Style
                        bg =
                            if subModel.jsonParseError == Nothing then
                                S.bgGray1

                            else
                                S.bgRed0
                    in
                    DialogView.fromBody
                        [ S.col
                        , S.outdent
                        , bg
                        , S.g4
                        , S.w196
                        , S.h128
                        ]
                        (jsonView subModel model.jsonTextField)

                Dialog__AddTask subModel ->
                    AddTask.dialog
                        (UniqueList.toList model.assignees)
                        (UniqueList.toList model.statuses)
                        subModel
                        |> DialogView.map GotAddTaskMsg

                Dialog__Error record ->
                    DialogView.fromBody
                        [ S.col
                        , S.outdent
                        , S.bgRed0
                        , S.g4
                        , S.w196
                        , S.h128
                        ]
                        [ Html.text "Error parsing JSON"
                        , Textarea.readOnly record.error
                            |> Textarea.toHtml
                        , Html.div
                            [ Attr.css
                                [ S.row
                                , S.g4
                                , S.justifyEnd
                                ]
                            ]
                            [ Button.secondary "Close" ClickedCloseDialog
                                |> Button.toHtml
                            ]
                        ]
    in
    DialogView.toHtml dialog


draggedTaskView : Maybe Pos -> Maybe ( TaskPositioning, Task ) -> Html Msg
draggedTaskView maybeMousePos maybeDraggedTask =
    case ( maybeMousePos, maybeDraggedTask ) of
        ( Just mousePos, Just ( { taskIndex, taskX, taskY, taskWidth }, task ) ) ->
            Html.div
                [ Attr.css
                    [ S.p2
                    , S.absolute
                    , Css.top <| Css.px <| toFloat (mousePos.y - taskY)
                    , Css.left <| Css.px <| toFloat (mousePos.x - taskX)
                    , Css.width <| Css.calc (Css.px taskWidth) Css.minus (Css.rem 1)
                    , S.bgNightwood2
                    ]
                ]
                (taskBodyView task)

        _ ->
            Html.text ""


statusColumnsView : Model -> Html Msg
statusColumnsView model =
    let
        statuses : List Status
        statuses =
            model.statuses
                |> UniqueList.toList

        body : List (Html Msg)
        body =
            if List.isEmpty statuses then
                [ Html.div
                    [ Attr.css
                        [ S.row
                        , S.g4
                        , S.justifyCenter
                        , S.itemsCenter
                        , S.flex1
                        ]
                    ]
                    [ Html.text "No tasks loaded. Please load some tasks, and they will display here."
                    ]
                ]

            else
                List.map
                    (statusColumn
                        (taskBeingDragged model.mouseDrag
                            |> Maybe.map .taskIndex
                        )
                        (Array.toIndexedList model.tasks)
                    )
                    statuses
    in
    Html.div
        [ Attr.css
            [ S.row
            , S.g4
            , S.overflowAuto
            , S.flex1
            , S.px4
            , S.pb4
            ]
        ]
        body


statusColumn : Maybe Int -> List ( Int, Task ) -> Status -> Html Msg
statusColumn draggedTask tasks status =
    let
        taskView : Int -> ( Int, Task ) -> Html Msg
        taskView viewIndex ( taskIndex, task ) =
            let
                bg : Css.Style
                bg =
                    if (viewIndex |> modBy 2) == 0 then
                        S.bgNightwood3

                    else
                        S.none

                mouseDownDecoder : Decoder Msg
                mouseDownDecoder =
                    JD.map4 TaskPositioning
                        (JD.succeed taskIndex)
                        (JD.field "offsetX" JD.int)
                        (JD.field "offsetY" JD.int)
                        (JD.at [ "currentTarget", "offsetWidth" ] JD.float)
                        |> JD.map MousedDownOnTask

                opacity : Css.Style
                opacity =
                    if Just taskIndex == draggedTask then
                        Css.property "opacity" "0.5"

                    else
                        S.none
            in
            Html.div
                [ Attr.css
                    [ S.p2
                    , bg
                    , S.pointerCursor
                    , opacity
                    ]
                , Attr.class "task"
                , Ev.on "mousedown" mouseDownDecoder
                ]
                (taskBodyView task)

        taskIsCorrectStatus : ( Int, Task ) -> Bool
        taskIsCorrectStatus ( _, task ) =
            task.status == status
    in
    Html.div
        [ Attr.css
            [ S.bgGray1
            , S.p4
            , S.col
            , S.g4
            , S.minW72
            , S.flex1_1_0pct
            , S.outdent
            ]
        , Attr.class "status"
        , Attr.attribute "data-status" (Status.toString status)
        ]
        [ Html.text (Status.toString status)
        , Html.div
            [ Attr.css
                [ S.col
                , S.indent
                , S.bgNightwood2
                , S.flex1
                , S.overflowAuto
                ]
            ]
            (tasks
                |> List.filter taskIsCorrectStatus
                |> List.indexedMap taskView
            )
        ]


taskBodyView : Task -> List (Html Msg)
taskBodyView task =
    [ Html.div
        [ Attr.css
            [ S.textYellow4 ]
        , Attr.class "task-title"
        ]
        [ Html.text task.title ]
    , Html.div
        [ Attr.css
            [ S.textSm ]
        ]
        [ Html.text <| Assignee.toString task.assignee ]
    , Html.div
        [ Attr.css
            [ S.textSm
            , S.textGray3
            ]
        ]
        [ Html.text task.description ]
    ]


jsonView : JsonDialogModel -> String -> List (Html Msg)
jsonView jsonDialogModel jsonTextField =
    let
        closeButton : Html Msg
        closeButton =
            Button.secondary "Close" ClickedCloseDialog
                |> Button.toHtml
    in
    case jsonDialogModel.jsonParseError of
        Just error ->
            let
                goBackButton : Html Msg
                goBackButton =
                    if jsonDialogModel.renderGoBackButton then
                        Button.secondary "Go Back" ClickedGoBackToJson
                            |> Button.toHtml

                    else
                        Html.text ""
            in
            [ Html.text "Error parsing JSON"
            , Textarea.readOnly (JD.errorToString error)
                |> Textarea.toHtml
            , Html.div
                [ Attr.css
                    [ S.row
                    , S.g4
                    , S.justifyEnd
                    ]
                ]
                [ goBackButton
                , closeButton
                ]
            ]

        Nothing ->
            [ Html.text "JSON"
            , Textarea.simple jsonTextField ChangedJsonText
                |> Textarea.toHtml
            , Html.div
                [ Attr.css
                    [ S.row
                    , S.g4
                    , S.justifyEnd
                    ]
                ]
                [ Button.primary "Use Json" ClickedUseJson
                    |> Button.toHtml
                , closeButton
                ]
            ]


controlsView : Html Msg
controlsView =
    Html.div
        [ Attr.css
            [ S.row
            , S.px4
            , S.pt4
            , S.g4
            ]
        ]
        [ Html.div
            [ Attr.css
                [ S.col
                , S.justifyCenter
                ]
            ]
            [ Html.text "Chad's To Do List App" ]
        , Html.div
            [ Attr.css
                [ S.row
                , S.g4
                ]
            ]
            [ Button.secondary "Edit Json" ClickedJsonView
                |> Button.toHtml
            , Button.secondary "Add Task" ClickedAddTask
                |> Button.toHtml
            ]
        ]



--------------------------------------------------------
-- SUBSCRIPTIONS --
--------------------------------------------------------


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        -- Decoder for parsing out the exact x and y coordinates of the mouse
        -- from the json event
        clientPosDecoder : Decoder Pos
        clientPosDecoder =
            JD.map2 Pos
                (JD.field "clientX" JD.int)
                (JD.field "clientY" JD.int)

        dialogSubs : Sub Msg
        dialogSubs =
            case model.dialog of
                Dialog__AddTask subModel ->
                    AddTask.subscriptions subModel
                        |> Sub.map GotAddTaskMsg

                _ ->
                    Sub.none
    in
    Sub.batch
        [ Browser.Events.onMouseMove <|
            JD.map MouseMoved clientPosDecoder
        , Browser.Events.onMouseUp <|
            JD.map MousedUp clientPosDecoder
        , columnStatus (\str -> Status.fromString str |> ColumnStatus)
        , dialogSubs
        ]



--------------------------------------------------------
-- MAIN --
--------------------------------------------------------


main : Program JD.Value Model Msg
main =
    Browser.document
        { init =
            \_ ->
                init (parseJsonText initJsonText)
        , update = update
        , subscriptions = subscriptions
        , view = document
        }
