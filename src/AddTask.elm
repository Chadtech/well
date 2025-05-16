module AddTask exposing
    ( Model
    , Msg
    , OutMsg(..)
    , dialog
    , init
    , subscriptions
    , update
    )

import Assignee exposing (Assignee)
import Ext.Browser as BrowserExt
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Ev
import Status exposing (Status)
import Style as S
import Task_ exposing (Task)
import View.Button as Button
import View.Dialog as Dialog exposing (Dialog)
import View.Dropdown as Dropdown
import View.TextField as TextField
import View.Textarea as Textarea



---------------------------------------------------------------
-- TYPES --
---------------------------------------------------------------


type alias Model =
    { titleField : String
    , descriptionField : String
    , assignee : Assignee
    , status : Status
    , openDropdown : OpenDropdown
    }


type Msg
    = ChangedTitleField String
    | ChangedDescriptionField String
    | ClickedAssignee Assignee
    | ClickedStatus Status
    | ClickedAssigneeDropdown
    | ClickedStatusDropdown
    | ClickedOutsideDropdown
    | ClickedCreateTask
    | ClickedClose



---------------------------------------------------------------
-- INIT --
---------------------------------------------------------------


init : Assignee -> Status -> Model
init assignee status =
    { titleField = ""
    , descriptionField = ""
    , assignee = assignee
    , status = status
    , openDropdown = OpenDropdown__None
    }


type OpenDropdown
    = OpenDropdown__None
    | OpenDropdown__Assignee
    | OpenDropdown__Status



---------------------------------------------------------------
-- HELPERS --
---------------------------------------------------------------


setDropdown : OpenDropdown -> Model -> Model
setDropdown openDropdown model =
    { model
        | openDropdown = openDropdown
    }


closeDropdown : Model -> Model
closeDropdown =
    setDropdown OpenDropdown__None



---------------------------------------------------------------
-- UPDATE --
---------------------------------------------------------------


type OutMsg
    = NoOut
    | NewTask Task
    | Close


withNoOut : Model -> ( Model, OutMsg )
withNoOut model =
    ( model, NoOut )


update : Msg -> Model -> ( Model, OutMsg )
update msg model =
    case msg of
        ChangedTitleField title ->
            { model | titleField = title }
                |> withNoOut

        ChangedDescriptionField description ->
            { model | descriptionField = description }
                |> withNoOut

        ClickedAssignee assignee ->
            { model | assignee = assignee }
                |> closeDropdown
                |> withNoOut

        ClickedStatus status ->
            { model | status = status }
                |> closeDropdown
                |> withNoOut

        ClickedAssigneeDropdown ->
            if model.openDropdown == OpenDropdown__Assignee then
                model
                    |> closeDropdown
                    |> withNoOut

            else
                model
                    |> closeDropdown
                    |> setDropdown OpenDropdown__Assignee
                    |> withNoOut

        ClickedStatusDropdown ->
            if model.openDropdown == OpenDropdown__Status then
                model
                    |> closeDropdown
                    |> withNoOut

            else
                model
                    |> setDropdown OpenDropdown__Status
                    |> withNoOut

        ClickedOutsideDropdown ->
            model
                |> closeDropdown
                |> withNoOut

        ClickedCreateTask ->
            let
                task : Task
                task =
                    { status = model.status
                    , title = model.titleField
                    , description = model.descriptionField
                    , assignee = model.assignee
                    }
            in
            ( model, NewTask task )

        ClickedClose ->
            ( model, Close )



---------------------------------------------------------------
-- VIEW --
---------------------------------------------------------------


dialog : List Assignee -> List Status -> Model -> Dialog Msg
dialog assignees statuses model =
    let
        assigneeOptions : Maybe (List (Dropdown.Option Msg))
        assigneeOptions =
            case model.openDropdown of
                OpenDropdown__Assignee ->
                    List.map
                        (\assignee ->
                            Dropdown.option
                                (Assignee.toString assignee)
                                (ClickedAssignee assignee)
                        )
                        assignees
                        |> Just

                _ ->
                    Nothing

        statusOptions : Maybe (List (Dropdown.Option Msg))
        statusOptions =
            case model.openDropdown of
                OpenDropdown__Status ->
                    List.map
                        (\status ->
                            Dropdown.option
                                (Status.toString status)
                                (ClickedStatus status)
                        )
                        statuses
                        |> Just

                _ ->
                    Nothing
    in
    Dialog.fromBody
        [ S.outdent
        , S.bgGray1
        , S.w196
        , S.h128
        , S.col
        , S.g4
        ]
        [ Html.div
            []
            [ Html.text "Add Task" ]
        , labeledField
            "Title"
            (TextField.simple model.titleField ChangedTitleField
                |> TextField.toHtml
            )
        , labeledField "Assignee"
            (Dropdown.simple
                (Assignee.toString model.assignee)
                ClickedAssigneeDropdown
                |> Dropdown.withHtmlId assigneeDropdownHtmlId
                |> Dropdown.toHtml assigneeOptions
            )
        , labeledField "Status"
            (Dropdown.simple
                (Status.toString model.status)
                ClickedStatusDropdown
                |> Dropdown.withHtmlId statusDropdownHtmlId
                |> Dropdown.toHtml statusOptions
            )
        , Html.div
            []
            [ Html.text "Description" ]
        , Textarea.simple model.descriptionField
            ChangedDescriptionField
            |> Textarea.toHtml
        , Html.div
            [ Attr.css
                [ S.row
                , S.g4
                , S.justifyEnd
                ]
            ]
            [ Button.primary "Create" ClickedCreateTask
                |> Button.toHtml
            , Button.secondary "Close" ClickedClose
                |> Button.toHtml
            ]
        ]


assigneeDropdownHtmlId : String
assigneeDropdownHtmlId =
    "assignee-dropdown"


statusDropdownHtmlId : String
statusDropdownHtmlId =
    "status-dropdown"


labeledField : String -> Html msg -> Html msg
labeledField label content =
    Html.div
        [ Attr.css
            [ S.row ]
        ]
        [ Html.div
            [ Attr.css
                [ S.w48
                , S.col
                , S.justifyCenter
                ]
            ]
            [ Html.text label ]
        , Html.div
            [ Attr.css
                [ S.flex1 ]
            ]
            [ content ]
        ]



---------------------------------------------------------------
-- SUBSCRIPTIONS --
---------------------------------------------------------------


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.openDropdown of
        OpenDropdown__None ->
            Sub.none

        OpenDropdown__Assignee ->
            BrowserExt.clickedOutside
                { htmlId = assigneeDropdownHtmlId
                , msg = ClickedOutsideDropdown
                }

        OpenDropdown__Status ->
            BrowserExt.clickedOutside
                { htmlId = statusDropdownHtmlId
                , msg = ClickedOutsideDropdown
                }
