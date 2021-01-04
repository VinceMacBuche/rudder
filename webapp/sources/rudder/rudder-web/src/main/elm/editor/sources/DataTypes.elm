module DataTypes exposing (..)

import Http exposing (Error)
import Dict exposing (Dict)
import DnDList

config : DnDList.Config MethodCall
config =
    { beforeUpdate = \_ _ list -> list
    , movement = DnDList.Free
    , listen = DnDList.OnDrag
    , operation = DnDList.Rotate
    }


dndSystem : DnDList.System MethodCall Msg
dndSystem =
    DnDList.create config DndEvent

type alias TechniqueId = {value : String}

type alias MethodId = {value : String}

type alias CallId = {value : String}

type alias ParameterId = {value : String}

type Constraint =
    AllowEmpty Bool
  | AllowWhiteSpace Bool
  | MaxLength Int
  | MinLength Int
  | MatchRegex String
  | NotMatchRegexp String
  | Select (List String)

type alias MethodParameter =
  { name : ParameterId
  , description : String
  , type_ : String
  , constraints : List Constraint
  }

type Agent = Cfengine | Dsc

type alias Method =
  { id : MethodId
  , name : String
  , description : String
  , classPrefix : String
  , classParameter : ParameterId
  , agentSupport : List Agent
  , parameters : List MethodParameter
  , documentation : Maybe String
  , deprecated :  Maybe String
  , rename : Maybe String
  }

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
  , methods    : Dict String Method
  , mode       : Mode
  , contextPath : String
  , techniqueFilter : String
  , methodsUI : MethodListUI
  , genericMethodsOpen : Bool
  , dnd : DnDList.Model
  }

type ResourceState = New | Unchanged | Deleted | Modified

type alias Resource =
  { name : String
  , state : ResourceState
  }

type alias MethodListUI =
  { filter : MethodFilter
  , docsOpen : List MethodId
  }

type alias MethodFilter =
  { name : String
  , showDeprecated : Bool
  , agent : Maybe Agent
  }
type MethodCallTab = CallParameters | Conditions | Result
type MethodCallMode = Opened | Closed

type Tab =  General |  Parameters | Resources | None

type Mode = Introduction | TechniqueDetails Technique Tab (Dict String (MethodCallMode, MethodCallTab)) (Maybe Technique)

type Msg =
    SelectTechnique Technique
  | GetTechniques  (Result Error (List Technique))
  | GetMethods  (Result Error (Dict String Method))
  | OpenMethod String
  | CloseMethod String
  | RemoveMethod String
  | CloneMethod MethodCall String
  | GenerateId (String -> Msg)
  | SwitchTabMethod String MethodCallTab
  | CallApi  (Model -> Cmd Msg)
  | SwitchTab Tab
  | UpdateTechniqueFilter String
  | UpdateMethodFilter MethodFilter
  | ToggleDoc MethodId
  | OpenMethods
  | OpenTechniques
  | NewTechnique
  | Ignore
  | AddMethod Method String
  | DndEvent DnDList.Msg