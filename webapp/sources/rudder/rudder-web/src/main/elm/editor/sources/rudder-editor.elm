port module Editor exposing (..)

import DataTypes exposing (..)
import ApiCalls exposing (..)
import View exposing (view)
import Browser
import Debug

mainInit : {  } -> ( Model, Cmd Msg )
mainInit initValues =
  let
    model =  Model [] [] Introduction "rudder"
  in
    (model,  getTechniques model )

main =
  Browser.element
    { init = mainInit
    , update = update
    , view = view
    , subscriptions = \m -> Sub.none
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    SelectTechnique technique ->
      ({ model | mode = TechniqueDetails technique General}, Cmd.none )
    SwitchTab tab ->
      let
        newMode =
          case model.mode of
           TechniqueDetails technique _ -> TechniqueDetails technique tab
           m -> m
      in
        ({ model | mode = newMode}, Cmd.none )
    GetTechniques (Ok  techniques) ->
      ({ model | techniques = techniques}, Cmd.none )
    GetTechniques (Err e) ->
      Debug.log (Debug.toString e) ( model , Cmd.none )
    CallApi apiCall ->
      ( model , apiCall model)