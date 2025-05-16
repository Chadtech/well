module Task_ exposing (Task)

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
