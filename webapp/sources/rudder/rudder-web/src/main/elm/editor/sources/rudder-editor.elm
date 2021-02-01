port module Editor exposing (..)

import DataTypes exposing (..)
import ApiCalls exposing (..)
import View exposing (view)
import Browser
import Debug
import Dict
import Random
import UUID
import List.Extra
import Either exposing (Either(..))
import Maybe.Extra


port copy : String -> Cmd msg

mainInit : {  } -> ( Model, Cmd Msg )
mainInit initValues =
  let
    model =  Model [] Dict.empty Introduction "rudder" "" (MethodListUI (MethodFilter "" False Nothing) []) False dndSystem.model
  in
    (model, Cmd.batch (  getMethods model :: []) )

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
        [ dndSystem.subscriptions model.dnd  ]


generator : Random.Generator String
generator = Random.map (UUID.toString) UUID.generator

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    SelectTechnique technique ->
      let
        ui = TechniqueUIInfo General (Dict.fromList (List.map (\c -> (c.id.value, (Closed, CallParameters))) technique.calls)) [] False
      in
        ({ model | mode = TechniqueDetails technique  (Just technique) ui } )
          |> update OpenMethods

    NewTechnique ->
      let
        ui = TechniqueUIInfo General Dict.empty [] False
        t = Technique (TechniqueId "") "1.0" "" "" "ncf_techniques" [] []
      in
        ({ model | mode = TechniqueDetails t Nothing ui}, Cmd.none )

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
      in
        ({ model | mode = newMode}, Cmd.none )

    GenerateId nextMsg ->
      (model, Random.generate nextMsg generator)

    CloneMethod call newId ->
      let
        clone = {call | id = newId }
        newMode =
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
               TechniqueDetails technique o newUi
           m -> m
      in
        ({ model | mode = newMode }, Cmd.none)

    SwitchTabMethod callId newTab ->
      let
        newMode =
          case model.mode of
            TechniqueDetails t o ui ->
              let
                newUi = { ui | callsUI = Dict.update callId.value (Maybe.map (\(state,_) -> (state,newTab))) ui.callsUI }
              in
                TechniqueDetails t o (Debug.log "lol" newUi)
            m -> m
      in
        ({ model | mode = newMode}, Cmd.none )

    UpdateTechnique technique ->
      let
        newMode =
          case model.mode of
            TechniqueDetails _ o ui ->
              TechniqueDetails technique o ui
            m -> m
      in
        ({ model | mode = newMode}, Cmd.none )


    GetTechniques (Ok  techniques) ->
      ({ model | techniques = techniques}, Cmd.none )
    GetTechniques (Err e) ->
      Debug.log (Debug.toString e) ( model , Cmd.none )

    SaveTechnique (Ok  technique) ->
      let
        techniques = if (List.any (.id >> (==) technique.id) model.techniques) then
           List.Extra.updateIf (.id >> (==) technique.id ) (always technique) model.techniques
         else
           technique :: model.techniques
        newMode = case model.mode of
                    TechniqueDetails t _ ui -> TechniqueDetails t (Just technique) ui
                    m -> m
      in
        ({ model | techniques = techniques, mode = newMode}, Cmd.none )
    SaveTechnique (Err e) ->
      Debug.log (Debug.toString e) ( model , Cmd.none )

    GetMethods (Ok  methods) ->
      ({ model | methods = methods}, getTechniques model  )
    GetMethods (Err e) ->
      Debug.log (Debug.toString e) ( model , Cmd.none )

    CallApi apiCall ->
      ( model , apiCall model)


    StartSaving ->
     case model.mode of
          TechniqueDetails t o ui ->
            update (CallApi (saveTechnique t (Maybe.Extra.isNothing o ))) { model | mode = TechniqueDetails t o ui }
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
        newMode =
          case model.mode of
            TechniqueDetails t o ui ->
              let
                technique =  { t | calls = newCall :: t.calls }
                newUi = { ui | callsUI = Dict.update newId.value (always (Just (Closed, CallParameters)) ) ui.callsUI }
              in
                TechniqueDetails technique o newUi
            m -> m
      in
        ( { model | mode = newMode } , Cmd.none )

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
        newMode =
          case model.mode of
            TechniqueDetails t o ui->
              let
                calls = List.Extra.updateIf (\c -> callId == c.id )  (\c -> { c | parameters = List.Extra.updateIf (\p -> p.id == paramId) (\p -> {p | value = newValue} ) c.parameters}) t.calls
              in
                TechniqueDetails { t | calls = calls } o ui
            _ -> model.mode
     in
       ({ model | mode = newMode}, Cmd.none )


    TechniqueParameterModified paramId newValue ->
      let
        newMode =
          case model.mode of
            TechniqueDetails t o ui->
              let
                parameters = List.Extra.updateIf (\c -> paramId == c.id ) (always newValue)  t.parameters
              in
                TechniqueDetails { t | parameters = parameters } o ui
            _ -> model.mode
      in
       ({ model | mode = newMode}, Cmd.none )

    TechniqueParameterRemoved paramId ->
      let
        newMode =
          case model.mode of
            TechniqueDetails t o ui->
              let
                parameters = List.Extra.filterNot (\c -> paramId == c.id ) t.parameters
                newUI = {ui | openedParameters = List.Extra.remove paramId ui.openedParameters }
              in
                TechniqueDetails { t | parameters = parameters } o newUI
            _ -> model.mode
      in
        ({ model | mode = newMode}, Cmd.none )

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