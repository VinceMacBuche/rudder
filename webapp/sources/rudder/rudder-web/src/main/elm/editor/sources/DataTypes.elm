module DataTypes exposing (..)

import Http exposing (Error)

type alias TechniqueId = {value : String}

type alias MethodId = {value : String}

type alias CallId = {value : String}

type alias ParameterId = {value : String}

type alias Technique =
  { id : TechniqueId
  , version : String
  , name : String
  , description : String
  , category : String
  , calls : List MethodCall
  , parameters : List TechniqueParameter
  }

type alias MethodCall =
  { id : String
  , methodName : MethodId
  , parameters : List CallParameter
  , condition : String
  , component : String
  }

type alias CallParameter =
  { id : ParameterId
  , value : String
  }

type alias TechniqueParameter =
  { id : ParameterId
  , name : String
  , description : String
  }

type alias Model =
  { techniques : List Technique
  , methods    : List String
  , mode       : Mode
  , contextPath : String
  }

type ResourceState = New | Unchanged | Deleted | Modified

type alias Resource =
  { name : String
  , state : ResourceState
  }
type Tab =  General |  Parameters | Resources | None

type Mode = Introduction | TechniqueDetails Technique Tab

type Msg =
    SelectTechnique Technique
  | GetTechniques  (Result Error (List Technique))
  | CallApi  (Model -> Cmd Msg)
  | SwitchTab Tab