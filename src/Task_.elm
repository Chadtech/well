module Task_ exposing
    ( Task
    , setStatus
    )

import Assignee exposing (Assignee)
import Status exposing (Status)



--------------------------------------------------------------
-- TYPES --
--------------------------------------------------------------


type alias Task =
    { status : Status
    , title : String
    , description : String
    , assignee : Assignee
    }



----------------------------------------------------------------
-- API --
----------------------------------------------------------------


setStatus : Status -> Task -> Task
setStatus status task =
    { task
        | status = status
    }
