module View exposing (..)

import DataTypes exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Tabs exposing (..)
import TechniqueList exposing (..)
import MethodCall exposing (..)
import Dict exposing (Dict)
import MethodsList exposing (..)
import ApiCalls exposing (..)



showTechnique : Model -> Technique ->  TechniqueState -> TechniqueUIInfo -> Html Msg
showTechnique model technique origin ui =
  let
    activeTabClass = (\tab -> "ui-tabs-tab " ++ (if ui.tab == tab then "active" else ""))
    creation = case origin of
                 Creation ->True
                 Clone _ -> True
                 Edit _ -> False
    isUnchanged = case origin of
                    Edit t -> t == technique
                    Creation -> False
                    Clone t -> t == technique
    topButtons =  [ li [] [
                      a [ class "action-success", disabled creation , onClick (CloneTechnique technique)] [
                        text "Clone "
                      , i [ class "fa fa-clone"] []
                      ]
                    ]
                  , li [] [
                      a [ class "action-primary" ] [ --ng-disabled="isNotSaved()"  ng-click=""exportTechnique(selectedTechnique)
                        text "Export "
                      , i [ class "fa fa-download"] []
                      ]
                    ]
                  , li [] [
                      a [ class "action-danger", disabled creation ] [ --ng-disabled="isNotSaved()"  ng-click="confirmPopup('Delete','Technique', deleteTechnique, selectedTechnique, selectedTechnique.name)"
                        text "Delete "
                      , i [ class "fa fa-times-circle"] []
                      ]
                    ]
                  ]
    title = if creation then
              [ i [] [ text "New Technique" ] ]
            else
              [ span [class "technique-version" ] [ text technique.version ] , text (" - " ++ technique.name) ]
  in
  div [ class "main-container" ] [
    div [ class "main-header" ] [
      div [ class "header-title" ] [
        h1 [] title
      , div [ class "header-buttons btn-technique" ] [
          div [ class "btn-group" ] [
            button [ class "btn btn-default dropdown-toggle" , attribute "data-toggle" "dropdown" ] [
              text "Actions "
            , i [ class "caret" ] []
            ]
          , ul [ class "dropdown-menu" ] topButtons
          ]
        , button [ class "btn btn-primary", disabled (isUnchanged && creation)  ] [ --  ng-click="resetTechnique()">
            text "Reset "
          , i [ class "fa fa-undo"] []
          ]
        , button [ class "btn btn-success btn-save", disabled (isUnchanged && ui.saving), onClick (CallApi (saveTechnique technique creation)) ] [ --ng-disabled="ui.editForm.$pending || ui.editForm.$invalid || CForm.form.$invalid || checkSelectedTechnique() || saving"  ng-click="saveTechnique()">
            text "Save "
          , i [ class ("fa fa-download " ++ (if ui.saving then "glyphicon glyphicon-cog fa-spin" else "")) ] []
          ]
        ]
      ]
    ]
  , div [ class "main-navbar" ] [
      ul [ class "ui-tabs-nav nav nav-tabs" ] [
        li [ class (activeTabClass General) , onClick (SwitchTab General)] [
          a [] [ text "General information" ]
        ]
      , li [ class (activeTabClass Parameters), onClick (SwitchTab Parameters) ] [
          a [] [
            text "Parameters "
          , span [ class ( "badge badge-secondary badge-resources " ++ if List.isEmpty technique.parameters then "empty" else "") ] [
              span [] [ text (String.fromInt (List.length technique.parameters)) ]
            ]
          ]
        ]
      , li [ class (activeTabClass Resources)  , onClick (SwitchTab Resources)] [
          a [] [
            text "Resources "
          , span [  class "badge badge-secondary badge-resources tooltip-bs" ] [
               -- ng-class="{'empty' : selectedTechnique.resources.length <= 0}"
               -- data-toggle="tooltip"
               -- data-trigger="hover"
               -- data-container="body"
              --  data-placement="right"
              --  data-title="{{getResourcesInfo()}}"
               -- data-html="true"
               -- data-delay='{"show":"400", "hide":"100"}'
               -- >
              span [] []--ng-if="(selectedTechnique.resources.length <= 0 || getResourcesByState(selectedTechnique.resources, 'new').length != selectedTechnique.resources.length && getResourcesByState(selectedTechnique.resources, 'deleted').length != selectedTechnique.resources.length)" class="nb-resources">{{selectedTechnique.resources.length - (getResourcesByState(selectedTechnique.resources, 'new').length + getResourcesByState(selectedTechnique.resources, 'deleted').length)}}</span>
            , span [] [] -- ng-if="getResourcesByState(selectedTechnique.resources, 'new').length>0" class="nb-resources new"> {{getResourcesByState(selectedTechnique.resources, 'new').length}}+ </span>
            , span [] [] --ng-if="getResourcesByState(selectedTechnique.resources, 'deleted').length>0" class="nb-resources del"> {{getResourcesByState(selectedTechnique.resources, 'deleted').length}}- </span>
            ]
          ]
        ]
      ]
    ]
  , div [ class "main-details", id "details"] [
      div [ class "editForm",  name "ui.editForm" ] [
        techniqueTab model technique creation ui
      , h5 [] [
          text "Generic Methods"
        , span [ class "badge badge-secondary" ] [
            span [] [ text (String.fromInt (List.length technique.calls ) ) ]
          ]
        , if model.genericMethodsOpen then text "" else
              button [class "btn-sm btn btn-success", type_ "button", onClick OpenMethods] [
                text "Add "
              , i [ class "fa fa-plus-circle" ] []
              ]
        ]
     ,  div [ class "row"] [
          ul [ id "methods", class "list-unstyled" ]
            ( ( if List.isEmpty technique.calls then
                  [ li [ id "no-methods" ] [
                      text "Drag and drop generic methods here from the list on the right to build target configuration for this technique."
                    ]
                  ]
              else
                  List.indexedMap (\ index call ->
                    let
                      method = case Dict.get call.methodName.value model.methods of
                                     Just m -> m
                                     Nothing -> Method call.methodName call.methodName.value "" "" (Maybe.withDefault (ParameterId "") (Maybe.map .id (List.head call.parameters))) [] [] Nothing Nothing Nothing
                      errors = List.map2 (\m c -> List.map (checkConstraint c) m.constraints) method.parameters call.parameters
                      (state, tab) = Maybe.withDefault (Closed,CallParameters) (Dict.get call.id.value ui.callsUI)
                    in
                      showMethodCall model state errors tab model.dnd (index) call
                 ) technique.calls
            ))

        ]
      ]
    ]


  ]

view : Model -> Html Msg
view model =
  let
    central = case model.mode of
                Introduction ->
                    div [ class "main-container" ] [
                      div [ class "col no-technique" ] [
                        h1 [] [ text "Technique editor" ]
                      , p [] [ text "Create a new technique or edit one from the list on the left."]
                      , p [] [ text "Define target configuration using the generic methods from the list on the right as building blocks."]
                      , button [ class "btn btn-success btn-lg" , onClick NewTechnique] [
                          text "Create Technique "
                        , i [ class "fa fa-plus-circle" ] []
                        ]
                      ]
                    ]

                TechniqueDetails technique state uiInfo ->
                  showTechnique model technique state uiInfo
    classes = "rudder-template " ++ if model.genericMethodsOpen then "show-methods" else "show-techniques"


  in
    div [ id "technique-editor", class classes] [
      techniqueList model model.techniques
    , div [ class "template-main" ] [central]
    , methodsList model
    , ( case model.mode of
         TechniqueDetails technique _ _->
           case maybeDragCard model technique.calls of
             Just c ->
               callBody model Closed c [] ( List.reverse (class "method" :: [] )) True
             _ ->
               text ""
         _ -> text ""
       )
    ]


maybeDragCard : Model -> List MethodCall -> Maybe MethodCall
maybeDragCard model methods =
   dndSystem.info model.dnd
        |> Maybe.andThen (\{ dragIndex } -> methods |> List.drop dragIndex |> List.head)