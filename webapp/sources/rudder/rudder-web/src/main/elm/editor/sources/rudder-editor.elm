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
      ({ model | mode = TechniqueDetails technique General (Dict.fromList (List.map (\c -> (c.id.value, (Closed, CallParameters))) technique.calls)) (Just technique) } )
        |> update OpenMethods

    NewTechnique ->
      ({ model | mode = TechniqueDetails (Technique (TechniqueId "") "1.0" "" "" "ncf_techniques" [] [] ) General Dict.empty Nothing }, Cmd.none )

    SwitchTab tab ->
      let
        newMode =
          case model.mode of
           TechniqueDetails technique _ map o -> TechniqueDetails technique tab map o
           m -> m
      in
        ({ model | mode = newMode}, Cmd.none )

    OpenMethod callId ->
      let
        newMode =
          case model.mode of
           TechniqueDetails t m map o -> TechniqueDetails t m (Dict.update  callId.value (Maybe.map (\(_,tab) -> (Opened,tab))) map) o
           m -> m
      in
        ({ model | mode = newMode}, Cmd.none )

    CloseMethod callId ->
      let
        newMode =
          case model.mode of
           TechniqueDetails t m map o -> TechniqueDetails t m (Dict.update callId.value (Maybe.map (\(_,tab) -> (Closed,tab))) map) o
           m -> m
      in
        ({ model | mode = newMode}, Cmd.none )

    RemoveMethod callId ->
      let
        newMode =
          case model.mode of
           TechniqueDetails t m map o -> TechniqueDetails { t |  calls = List.filter (\c -> c.id /= callId ) t.calls } m  (Dict.remove callId.value  map) o
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
           TechniqueDetails t m map o ->
             let
               newMethods =
                 let
                  (end,beginning) = List.Extra.span (\c -> c.id == call.id ) (List.reverse t.calls)
                 in
                   List.reverse (List.append end (clone :: beginning))
             in
               TechniqueDetails { t |  calls = newMethods} m  (Dict.update newId.value (\_ -> Just (Closed,CallParameters)) map ) o
           m -> m
      in
        ({ model | mode = newMode }, Cmd.none)

    SwitchTabMethod callId newTab ->
      let
        newMode =
          case model.mode of
            TechniqueDetails t m map o  -> TechniqueDetails t m (Dict.update callId.value (Maybe.map (\(s,_) -> (s,newTab))) map) o
            m -> m
      in
        ({ model | mode = newMode}, Cmd.none )

    GetTechniques (Ok  techniques) ->
      ({ model | techniques = techniques}, Cmd.none )
    GetTechniques (Err e) ->
      Debug.log (Debug.toString e) ( model , Cmd.none )

    GetMethods (Ok  methods) ->
      ({ model | methods = methods}, getTechniques model  )
    GetMethods (Err e) ->
      Debug.log (Debug.toString e) ( model , Cmd.none )

    CallApi apiCall ->
      ( model , apiCall model)

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
            TechniqueDetails t m map o  -> TechniqueDetails { t | calls = newCall :: t.calls } m (Dict.update newId.value (always (Just (Closed, CallParameters)) ) map) o
            m -> m
      in
        ( { model | mode = newMode } , Cmd.none )

    DndEvent dndMsg ->
            let


              ( newModel, c ) =
                case model.mode of
                   TechniqueDetails t m map o  ->
                    let
                      (d, calls ) = dndSystem.update dndMsg model.dnd (List.append (List.map (Right) t.calls) (Dict.values model.methods |> List.map (Left)))
                      newMode = TechniqueDetails { t | calls = Either.rights calls } m  map o
                    in
                      if (List.any (\call -> call.id.value == "") t.calls) then
                        update (GenerateId (\s -> SetCallId (CallId s))) { model | dnd = d, mode = newMode }
                      else
                        ( { model | dnd = d, mode = newMode }, Cmd.none)
                   m -> (model, Cmd.none)
            in
               (newModel , Cmd.batch [  dndSystem.commands newModel.dnd, c ] )

    MethodCallParameterModified callId paramId newValue ->
      let
        newMode =
          case model.mode of
            TechniqueDetails t m map o ->
              let
                calls = List.Extra.updateIf (\c -> callId == c.id )  (\c -> { c | parameters = List.Extra.updateIf (\p -> p.id == paramId) (\p -> {p | value = newValue} ) c.parameters}) t.calls
              in
                TechniqueDetails { t | calls = calls } m map o
            _ -> model.mode
     in
       ({ model | mode = newMode}, Cmd.none )

    SetCallId newId ->
      let
        newMode =
          case model.mode of
            TechniqueDetails t m map o  ->
              TechniqueDetails { t | calls = List.Extra.updateIf (\c -> c.id.value == "") (\c -> { c | id = newId } ) t.calls } m  (Dict.insert newId.value (Closed,CallParameters) map) o
            m -> m
      in
        ( { model | mode = newMode } , Cmd.none )
    Ignore->
      ( model , Cmd.none)