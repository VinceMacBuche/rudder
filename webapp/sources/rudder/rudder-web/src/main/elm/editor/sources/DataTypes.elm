module DataTypes exposing (..)

import File exposing (File)
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
  , resources : List Resource
  }

type alias MethodCall =
  { id : CallId
  , methodName : MethodId
  , parameters : List CallParameter
  , condition : Condition
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

type alias TechniqueCategory =
  { path : String
  , name : String
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
                           Right (MethodCall (CallId "") method.id (List.map (\p -> CallParameter p.name "") method.parameters) (Condition Nothing "") "")
                         _-> drop
                     )
                  )
                }
    }

dndSystem : DnDList.Groups.System (Either Method MethodCall) Msg
dndSystem =
  DnDList.Groups.create config DndEvent

type TechniqueState = Creation TechniqueId | Edit Technique | Clone Technique TechniqueId

type ModalState = DeletionValidation Technique

type alias Model =
  { techniques : List Technique
  , methods    : Dict String Method
  , categories : List TechniqueCategory
  , mode       : Mode
  , contextPath : String
  , techniqueFilter : String
  , methodsUI : MethodListUI
  , genericMethodsOpen : Bool
  , dnd : DnDList.Groups.Model
  , modal : Maybe ModalState
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
  , state : MethodFilterState
  }

type MethodFilterState = FilterOpened | FilterClosed

type ValidationState error = Untouched | ValidState | InvalidState error
type TechniqueNameError = EmptyName | AlreadyTakenName
type TechniqueIdError = TooLongId | AlreadyTakenId | InvalidStartId

type MethodCallParamError = ConstraintError (List String)

type alias MethodCallUiInfo =
  { mode : MethodCallMode
  , tab : MethodCallTab
  , validation : Dict String  ( ValidationState MethodCallParamError )
  }

type alias TechniqueUIInfo =
  { tab : Tab
  , callsUI : Dict String MethodCallUiInfo
  , openedParameters : List ParameterId
  , saving : Bool
  , nameState : ValidationState TechniqueNameError
  , idState : ValidationState TechniqueIdError
  }
type MethodCallTab = CallParameters | Conditions | Result
type MethodCallMode = Opened | Closed

type Tab =  General |  Parameters | Resources | None

type Mode = Introduction | TechniqueDetails Technique TechniqueState TechniqueUIInfo

type OS = AIX | Linux (Maybe LinuxOS)  | Solaris | Windows


conditionLinux: LinuxOS -> String
conditionLinux os =
  case os of
    Debian v -> "debian"
    Ubuntu v -> "ubuntu"
    RH v -> "redhat"
    Centos v -> "centos"
    Fedora v -> "fedora"
    Oracle v -> "oracle"
    Amazon -> "amazon"
    Suse -> "SUSE"
    SLES v -> "sles"
    SLED v -> "sled"
    OpenSuse v -> "opensuse"
    Slackware -> "slackware"

conditionOs : OS -> String
conditionOs os =
  case os of
    AIX -> "aix"
    Linux Nothing -> "linux"
    Solaris -> "solaris"
    Windows -> "windows"
    Linux (Just linuxOs) -> conditionLinux linuxOs

conditionStr : Condition -> String
conditionStr condition =
  case condition.os of
    Nothing -> condition.advanced
    Just os ->
      if (String.isEmpty condition.advanced) then conditionOs os else conditionOs os ++ "." ++ condition.advanced

type alias Condition =
  { os : Maybe OS
  , advanced : String
  }

type LinuxOS = Debian { major : Maybe  Int, minor : Maybe Int }
             | RH { major : Maybe  Int, minor : Maybe Int }
             | Centos { major : Maybe  Int, minor : Maybe Int }
             | Fedora { major : Maybe  Int}
             | Ubuntu { major : Maybe  Int, minor : Maybe Int }
             | Slackware
             | Suse
             | Oracle { major : Maybe  Int, minor : Maybe Int }
             | Amazon
             | SLES { version : Maybe  Int, sp : Maybe Int }
             | SLED { version : Maybe  Int, sp : Maybe Int }
             | OpenSuse { version : Maybe  Int }

type Msg =
    SelectTechnique Technique
  | GetTechniques  (Result Error (List Technique))
  | GetCategories  (Result Error (List TechniqueCategory))
  | SaveTechnique  (Result Error Technique)
  | DeleteTechnique  (Result Error (TechniqueId, String))
  | GetMethods  (Result Error (Dict String Method))
  | GetTechniqueResources  (Result Error (List Resource))
  | OpenMethod CallId
  | CloseMethod CallId
  | RemoveMethod CallId
  | CloneMethod MethodCall CallId
  | UpdateTechnique Technique
  | MethodCallParameterModified MethodCall ParameterId String
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
  | NewTechnique TechniqueId
  | Ignore
  | AddMethod Method CallId
  | DndEvent DnDList.Groups.Msg
  | SetCallId CallId
  | StartSaving
  | Copy String
  | Store String Value
  | GetFromStore Technique (Maybe Technique) TechniqueId
  | CloneTechnique Technique TechniqueId
  | ResetTechnique
  | ResetMethodCall MethodCall
  | ToggleFilter
  | OpenDeletionPopup Technique
  | ClosePopup Msg
  | OpenFileManager
  | Export
  | StartImport
  | ImportFile File
  | ParseImportedFile File String
  | ScrollCategory String
  | UpdateCondition CallId Condition
