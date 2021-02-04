port module Editor exposing (..)

import DataTypes exposing (..)
import ApiCalls exposing (..)
import View exposing (view)
import Browser
import Debug
import Dict exposing (Dict)
import Random
import UUID
import List.Extra
import Either exposing (Either(..))
import Maybe.Extra
import  Json.Decode exposing (Value)
import JsonEncoder exposing ( encodeTechnique)
import JsonDecoder exposing ( decodeTechnique)

port copy : String -> Cmd msg

port store : (String , Value) -> Cmd msg
port clear : String  -> Cmd msg
port get : () -> Cmd msg
port response: ((Value, Value) -> msg) -> Sub msg

port successNotification : String -> Cmd msg
port errorNotification   : String -> Cmd msg
port warnNotification   : String -> Cmd msg
port infoNotification    : String -> Cmd msg

parseResponse: (Value, Value) -> Msg
parseResponse (json, optJson) =
  case Json.Decode.decodeValue decodeTechnique json of
    Ok tech ->
      let
        o =
          case  (Json.Decode.decodeValue (Json.Decode.nullable  decodeTechnique)) optJson of
            Ok (m) -> m
            _ -> Nothing
      in
        GetFromStore tech o
    Err _ -> Ignore

mainInit : { contextPath : String  } -> ( Model, Cmd Msg )
mainInit initValues =
  let
    model =  Model [] Dict.empty Introduction initValues.contextPath "" (MethodListUI (MethodFilter "" False Nothing) []) False dndSystem.model
  in
    (model, Cmd.batch ( getMethods model  :: []) )

updatedStoreTechnique: Model -> Cmd msg
updatedStoreTechnique model =
  case model.mode of
    TechniqueDetails t o _ ->
      let
        storeOriginTechnique =
          case o of
            Edit origin -> store ("originTechnique", encodeTechnique origin)
            _ -> clear "originTechnique"
      in
        Cmd.batch [ store ("currentTechnique", encodeTechnique t), storeOriginTechnique ]
    _ -> Cmd.none

main =
  Browser.element
    { init = mainInit
    , update = update
    , view = view
    , subscriptions = subscriptions
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ dndSystem.subscriptions model.dnd
        , response parseResponse
        ]

selectTechnique: Model -> Technique -> (Model, Cmd Msg)
selectTechnique model technique =
  let
    ui = TechniqueUIInfo General (Dict.fromList (List.map (\c -> (c.id.value, (Closed, CallParameters))) technique.calls)) [] False
  in
    ({ model | mode = TechniqueDetails technique  (Edit technique) ui } )
      |> update OpenMethods
      |> Tuple.first
      |> update (Store "storedTechnique" (encodeTechnique technique))


generator : Random.Generator String
generator = Random.map (UUID.toString) UUID.generator

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    SelectTechnique technique ->
      case model.mode of
        TechniqueDetails t _ _ ->
          if t.id == technique.id then
             ( { model | mode = Introduction }, clear "storedTechnique")
          else
            selectTechnique model technique
        _ ->
          selectTechnique model technique

    CloneTechnique technique ->
      let
        ui = TechniqueUIInfo General (Dict.fromList (List.map (\c -> (c.id.value, (Closed, CallParameters))) technique.calls)) [] False
      in
        ({ model | mode = TechniqueDetails technique  (Clone technique) ui } )
          |> update OpenMethods
          |> Tuple.first
          |> update (Store "storedTechnique" (encodeTechnique technique))

    NewTechnique ->
      let
        ui = TechniqueUIInfo General Dict.empty [] False
        t = Technique (TechniqueId "") "1.0" "" "" "ncf_techniques" [] []
        newModel =  { model | mode = TechniqueDetails t Creation ui}
      in
        (newModel, updatedStoreTechnique newModel )

    SwitchTab tab ->
      let
        newMode =
          case model.mode of
           TechniqueDetails technique o ui-> TechniqueDetails technique o { ui | tab = tab }
           m -> m
      in
        ({ model | mode = newMode}, Cmd.none )

    OpenMethod callId ->
      let
        newMode =
          case model.mode of
           TechniqueDetails t o ui->
             let
               newUi = {ui | callsUI = Dict.update  callId.value (Maybe.map (\(_,tab) -> (Opened,tab))) ui.callsUI }
             in
              TechniqueDetails t o newUi
           m -> m
      in
        ({ model | mode = newMode}, Cmd.none )

    CloseMethod callId ->
      let
        newMode =
          case model.mode of
           TechniqueDetails t o ui->
            let
              newUi = {ui | callsUI = Dict.update  callId.value (Maybe.map (\(_,tab) -> (Closed,tab))) ui.callsUI }
            in
              TechniqueDetails t o newUi
           m -> m
      in
        ({ model | mode = newMode}, Cmd.none )

    RemoveMethod callId ->
      let
        newMode =
          case model.mode of
           TechniqueDetails t o ui ->
            let
              technique = { t |  calls = List.filter (\c -> c.id /= callId ) t.calls }
              newUi = {ui | callsUI = Dict.remove callId.value  ui.callsUI }
            in
            TechniqueDetails technique o newUi
           m -> m
        newModel = { model | mode = newMode}
      in
        (newModel, updatedStoreTechnique newModel )

    GenerateId nextMsg ->
      (model, Random.generate nextMsg generator)

    CloneMethod call newId ->
      let
        clone = {call | id = newId }
        newModel =
          case model.mode of
           TechniqueDetails t o ui ->
             let
               newMethods =
                 let
                  (end,beginning) = List.Extra.span (\c -> c.id == call.id ) (List.reverse t.calls)
                 in
                   List.reverse (List.append end (clone :: beginning))
               technique = { t |  calls = newMethods}
               newUi =  { ui | callsUI = Dict.update newId.value (\_ -> Just (Closed,CallParameters)) ui.callsUI }
             in
               { model | mode = TechniqueDetails technique o newUi }
           m -> model
      in
        (newModel, updatedStoreTechnique newModel )

    SwitchTabMethod callId newTab ->
      let
        newMode =
          case model.mode of
            TechniqueDetails t o ui ->
              let
                newUi = { ui | callsUI = Dict.update callId.value (Maybe.map (\(state,_) -> (state,newTab))) ui.callsUI }
              in
                TechniqueDetails t o newUi
            m -> m
      in
        ({ model | mode = newMode}, Cmd.none )

    UpdateTechnique technique ->
      let
        newModel =
          case model.mode of
            TechniqueDetails _ o ui ->
              { model | mode = TechniqueDetails technique o ui }
            m -> model
      in
        (newModel, updatedStoreTechnique newModel )


    GetTechniques (Ok  techniques) ->
      ({ model | techniques = techniques},  get () )
    GetTechniques (Err e) ->
      Debug.log (Debug.toString e) ( model , Cmd.none )

    SaveTechnique (Ok  technique) ->
      let
        techniques = if (List.any (.id >> (==) technique.id) model.techniques) then
           List.Extra.updateIf (.id >> (==) technique.id ) (always technique) model.techniques
         else
           technique :: model.techniques
        newMode = case model.mode of
                    TechniqueDetails t _ ui -> TechniqueDetails t (Edit technique) ui
                    m -> m
      in
        ({ model | techniques = techniques, mode = newMode}, successNotification "Technique saved!" )
    SaveTechnique (Err e) ->
      Debug.log (Debug.toString e) ( model , errorNotification (Debug.toString e) )

    GetMethods (Ok  methods) ->
      ({ model | methods = methods}, getTechniques model  )
    GetMethods (Err e) ->
      Debug.log (Debug.toString e) ( model , Cmd.none )

    CallApi apiCall ->
      ( model , apiCall model)


    StartSaving ->
     case model.mode of
          TechniqueDetails t o ui ->
            case o of
              Edit _ ->
               update (CallApi (saveTechnique t False)) { model | mode = TechniqueDetails t o ui }
              _ ->
               update (CallApi (saveTechnique t True)) { model | mode = TechniqueDetails t o ui }
          _ -> (model, Cmd.none)

    UpdateTechniqueFilter newFilter->
      ( { model | techniqueFilter = newFilter } , Cmd.none)

    UpdateMethodFilter newFilter->
      let
        ui = model.methodsUI
      in
        ( { model | methodsUI = { ui | filter = newFilter } } , Cmd.none )

    ToggleDoc methodId ->
      let
        ui = model.methodsUI
        newDocs = if List.member methodId ui.docsOpen then List.Extra.remove methodId ui.docsOpen else methodId :: ui.docsOpen
      in
        ( { model | methodsUI = { ui | docsOpen = newDocs } } , Cmd.none )

    OpenMethods ->
      ( { model | genericMethodsOpen = True } , Cmd.none )

    OpenTechniques ->
      ( { model | genericMethodsOpen = False } , Cmd.none )

    AddMethod method newId ->
      let
        newCall = MethodCall newId method.id (List.map (\p -> CallParameter p.name "") method.parameters) "" ""
        newModel =
          case model.mode of
            TechniqueDetails t o ui ->
              let
                technique =  { t | calls = newCall :: t.calls }
                newUi = { ui | callsUI = Dict.update newId.value (always (Just (Closed, CallParameters)) ) ui.callsUI }
              in
              { model | mode = TechniqueDetails technique o newUi }
            _ -> model
      in
        (  newModel , updatedStoreTechnique newModel )

    DndEvent dndMsg ->
            let


              ( newModel, c ) =
                case model.mode of
                   TechniqueDetails t o ui ->
                    let
                      (d, calls ) = dndSystem.update dndMsg model.dnd (List.append (List.map (Right) t.calls) (Dict.values model.methods |> List.map (Left)))
                      newMode = TechniqueDetails { t | calls = Either.rights calls } o ui
                    in
                      if (List.any (\call -> call.id.value == "") t.calls) then
                        update (GenerateId (\id -> SetCallId (CallId id))) { model | dnd = d, mode = newMode }
                      else
                        ( { model | dnd = d, mode = newMode }, Cmd.none)
                   _ -> (model, Cmd.none)
            in
               (newModel , Cmd.batch [  dndSystem.commands newModel.dnd, c ] )

    MethodCallParameterModified callId paramId newValue ->
      let
        newModel =
          case model.mode of
            TechniqueDetails t o ui->
              let
                calls = List.Extra.updateIf (\c -> callId == c.id )  (\c -> { c | parameters = List.Extra.updateIf (\p -> p.id == paramId) (\p -> {p | value = newValue} ) c.parameters}) t.calls
                technique = { t | calls = calls }
              in
                { model | mode = TechniqueDetails technique o ui }
            _ -> model
     in
       ( newModel , updatedStoreTechnique newModel  )


    TechniqueParameterModified paramId newValue ->
      let
        newModel =
          case model.mode of
            TechniqueDetails t o ui->
              let
                parameters = List.Extra.updateIf (\c -> paramId == c.id ) (always newValue)  t.parameters
                technique = { t | parameters = parameters }
              in
                { model | mode = TechniqueDetails technique o ui }
            _ -> model
      in
       (newModel , updatedStoreTechnique newModel )

    TechniqueParameterRemoved paramId ->
      let
        newModel =
          case model.mode of
            TechniqueDetails t o ui->
              let
                parameters = List.Extra.filterNot (\c -> paramId == c.id ) t.parameters
                newUI = {ui | openedParameters = List.Extra.remove paramId ui.openedParameters }
                technique = { t | parameters = parameters }
              in
                { model | mode = TechniqueDetails technique o newUI }
            _ -> model
      in
        ( newModel , updatedStoreTechnique newModel )

    TechniqueParameterAdded paramId ->
      let
        newMode =
          case model.mode of
            TechniqueDetails t o ui->
              let
                parameters = List.append t.parameters [  TechniqueParameter paramId "" "" ]
              in
                TechniqueDetails { t | parameters = parameters } o ui
            _ -> model.mode
      in
        ({ model | mode = newMode}, Cmd.none )

    TechniqueParameterToggle paramId ->
      let
        newMode =
          case model.mode of
            TechniqueDetails t o ui->
              let
                newUI = {ui | openedParameters = (if List.member paramId ui.openedParameters then List.Extra.remove else (::) ) paramId ui.openedParameters }
              in
                TechniqueDetails t o newUI
            _ -> model.mode
      in
        ({ model | mode = newMode}, Cmd.none )

    SetCallId newId ->
      let
        newMode =
          case model.mode of
            TechniqueDetails t o ui ->
              let
                technique = { t | calls = List.Extra.updateIf (\c -> c.id.value == "") (\c -> { c | id = newId } ) t.calls }
                newUi = { ui | callsUI = Dict.insert newId.value (Closed,CallParameters) ui.callsUI}
              in
                TechniqueDetails  technique o newUi
            m -> m
      in
        ( { model | mode = newMode } , Cmd.none )
    Ignore->
      ( model , Cmd.none)

    Copy value ->
      (model, copy value)
    Store key value ->
      (model, updatedStoreTechnique model )
    GetFromStore technique originTechnique ->
            let
              ui = TechniqueUIInfo General (Dict.fromList (List.map (\c -> (c.id.value, (Closed, CallParameters))) technique.calls)) [] False
              notification = case (List.Extra.find (.id >> (==) technique.id) model.techniques,originTechnique) of
                (Nothing, Just _) -> warnNotification "Technique reloaded from cache was deleted, Saving will recreate it"
                (Just _ , Nothing) -> warnNotification "Technique from cache was created, change name/id before saving"
                (Just t , Just o) -> if t /= o then warnNotification "Technique reloaded from cache since you modified it, saving will overwrite current changes" else infoNotification "Technique reloaded from cache"
                (Nothing, Nothing) -> infoNotification "Technique reloaded from cache"
              state = Maybe.withDefault Creation (Maybe.map Edit originTechnique)
            in
              ({ model | mode = TechniqueDetails technique state ui } )
                |> update OpenMethods
                |>  Tuple.mapSecond (\c -> Cmd.batch [c, notification ])
