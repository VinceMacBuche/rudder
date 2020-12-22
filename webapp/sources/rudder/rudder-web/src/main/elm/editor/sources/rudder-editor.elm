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

mainInit : {  } -> ( Model, Cmd Msg )
mainInit initValues =
  let
    model =  Model [] Dict.empty Introduction "rudder"
  in
    (model, Cmd.batch (  getMethods model :: []) )

main =
  Browser.element
    { init = mainInit
    , update = update
    , view = view
    , subscriptions = \m -> Sub.none
    }



generator : Random.Generator String
generator = Random.map (UUID.toString) UUID.generator

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    SelectTechnique technique ->
      ({ model | mode = TechniqueDetails technique General (Dict.fromList (List.map (\c -> (c.id, (Closed, CallParameters))) technique.calls))}, Cmd.none )
    SwitchTab tab ->
      let
        newMode =
          case model.mode of
           TechniqueDetails technique _ map -> TechniqueDetails technique tab map
           m -> m
      in
        ({ model | mode = newMode}, Cmd.none )
    OpenMethod callId ->
      let
        newMode =
          case model.mode of
           TechniqueDetails t m map -> TechniqueDetails t m (Dict.update (Debug.log callId callId) (Maybe.map (\(_,tab) -> (Opened,tab))) map)
           m -> m

      in
        ({ model | mode = newMode}, Cmd.none )
    CloseMethod callId ->
      let
        newMode =
          case model.mode of
           TechniqueDetails t m map -> TechniqueDetails t m (Dict.update callId (Maybe.map (\(_,tab) -> (Closed,tab))) map)
           m -> m
      in
        ({ model | mode = newMode}, Cmd.none )
    RemoveMethod callId ->
      let
        newMode =
          case model.mode of
           TechniqueDetails t m map -> TechniqueDetails { t |  calls = List.filter (\c -> c.id /= callId ) t.calls } m  (Dict.remove callId  map)
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
           TechniqueDetails t m map ->
             let
               newMethods =
                 let
                  (end,beginning) = List.Extra.span (\c -> c.id == call.id ) (List.reverse t.calls)
                 in
                   List.reverse (List.append end (clone :: beginning))
             in
               TechniqueDetails { t |  calls = newMethods} m  (Dict.update newId (\_ -> Just (Closed,CallParameters)) map )
           m -> m
      in
        (model,Cmd.none)
    SwitchTabMethod callId newTab ->
      let
        newMode =
          case model.mode of
            TechniqueDetails t m map -> TechniqueDetails t m (Dict.update callId (Maybe.map (\(s,_) -> (s,newTab))) map)
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