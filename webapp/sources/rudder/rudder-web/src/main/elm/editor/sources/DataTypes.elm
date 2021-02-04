module DataTypes exposing (..)

import Http exposing (Error)
import Dict exposing (Dict)
import DnDList.Groups
import Either exposing (Either(..))
import Json.Decode exposing (Value)
import Regex

type alias TechniqueId = {value : String}

type alias MethodId = {value : String}

type alias CallId = {value : String}

type alias ParameterId = {value : String}


canonify: String -> String
canonify value =
   Regex.replace ((Regex.fromString >> Maybe.withDefault Regex.never) "[^_a-zA-Z\\d]") (always "_") value


type Constraint =
    AllowEmpty Bool
  | AllowWhiteSpace Bool
  | MaxLength Int
  | MinLength Int
  | MatchRegex String
  | NotMatchRegex String
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
  { id : CallId
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



config : DnDList.Groups.Config (Either Method MethodCall)
config =
    { beforeUpdate = \_ _ list -> list
    , listen = DnDList.Groups.OnDrag
    , operation = DnDList.Groups.Swap
    , groups =
                { listen = DnDList.Groups.OnDrag
                , operation = DnDList.Groups.InsertAfter
                , comparator =
                   (\drag drop ->

                     Debug.log "comp" (
                       case (drag,drop) of
                         (Left  _ , Left _ ) -> True
                         (Right _  , Right _ ) -> True
                         _ -> False
                     )
                  )
                , setter =
                   (\drag drop ->
                     Debug.log "setter" (
                       case (drag,drop) of
                         (  Right _, Left method ) ->
                           Right (MethodCall (CallId "") method.id (List.map (\p -> CallParameter p.name "") method.parameters) "any" "")
                         _-> drop
                     )
                  )
                }
    }

dndSystem : DnDList.Groups.System (Either Method MethodCall) Msg
dndSystem =
  DnDList.Groups.create config DndEvent

type TechniqueState = Creation | Edit Technique | Clone Technique

type alias Model =
  { techniques : List Technique
  , methods    : Dict String Method
  , mode       : Mode
  , contextPath : String
  , techniqueFilter : String
  , methodsUI : MethodListUI
  , genericMethodsOpen : Bool
  , dnd : DnDList.Groups.Model
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

type alias TechniqueUIInfo =
  { tab : Tab
  , callsUI : Dict String (MethodCallMode, MethodCallTab)
  , openedParameters : List ParameterId
  , saving : Bool
  }
type MethodCallTab = CallParameters | Conditions | Result
type MethodCallMode = Opened | Closed

type Tab =  General |  Parameters | Resources | None

type Mode = Introduction | TechniqueDetails Technique TechniqueState TechniqueUIInfo

type Msg =
    SelectTechnique Technique
  | GetTechniques  (Result Error (List Technique))
  | SaveTechnique  (Result Error Technique)
  | GetMethods  (Result Error (Dict String Method))
  | OpenMethod CallId
  | CloseMethod CallId
  | RemoveMethod CallId
  | CloneMethod MethodCall CallId
  | UpdateTechnique Technique
  | MethodCallParameterModified CallId ParameterId String
  | TechniqueParameterModified ParameterId TechniqueParameter
  | TechniqueParameterRemoved ParameterId
  | TechniqueParameterAdded ParameterId
  | TechniqueParameterToggle ParameterId
  | GenerateId (String -> Msg)
  | SwitchTabMethod CallId MethodCallTab
  | CallApi  (Model -> Cmd Msg)
  | SwitchTab Tab
  | UpdateTechniqueFilter String
  | UpdateMethodFilter MethodFilter
  | ToggleDoc MethodId
  | OpenMethods
  | OpenTechniques
  | NewTechnique
  | Ignore
  | AddMethod Method CallId
  | DndEvent DnDList.Groups.Msg
  | SetCallId CallId
  | StartSaving
  | Copy String
  | Store String Value
  | GetFromStore Technique (Maybe Technique)
  | CloneTechnique Technique
