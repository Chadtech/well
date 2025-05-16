module Ext.Browser exposing (clickedOutside)

import Browser.Events
import Json.Decode as JD exposing (Decoder)


clickedOutside : { htmlId : String, msg : msg } -> Sub msg
clickedOutside { htmlId, msg } =
    let
        isOutsideDropdown : () -> Decoder Bool
        isOutsideDropdown () =
            JD.oneOf
                [ JD.field "id" JD.string
                    |> JD.andThen
                        (\id ->
                            if id == htmlId then
                                JD.succeed False

                            else
                                JD.fail "continue"
                        )
                , JD.lazy
                    (\_ ->
                        isOutsideDropdown ()
                            |> JD.field "parentNode"
                    )
                , JD.succeed True
                ]

        outsideTarget : Decoder msg
        outsideTarget =
            JD.field "target" (isOutsideDropdown ())
                |> JD.andThen
                    (\isOutside ->
                        if isOutside then
                            JD.succeed msg

                        else
                            JD.fail "clicked inside"
                    )
    in
    Browser.Events.onMouseDown outsideTarget
