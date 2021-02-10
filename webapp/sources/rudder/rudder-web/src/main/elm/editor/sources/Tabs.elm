module Tabs exposing (..)

import DataTypes exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



techniqueResource : Technique -> Resource -> Html Msg
techniqueResource technique resource =
  let
    stateClass =
      case resource.state of
        New -> "new"
        Deleted -> "deleted"
        Modified -> "modified"
        Unchanged -> "unchanged"
  in
    li [ class stateClass ] [
      span [ class "fa fa-file" ] []
    , span [ class "target-name" ] [ text resource.name ]
    , span [ class "border" ] []
    , div [ class "use-with" ] [
        span [] [ text ( "${resources_dir}/"++ resource.name) ]
      , button [ class "btn btn-outline-secondary clipboard", title "Copy to clipboard" ] [ -- data-clipboard-text="{{'${resources_dir}/' + resource.name}}" >
          i [  class "ion ion-clipboard" ] []
        ]
      ]
    ]


techniqueParameter :  Technique -> TechniqueParameter -> Bool -> Html Msg
techniqueParameter technique param opened =
  let
    param_name =
      if (String.isEmpty param.name) then
        [  div [ class "empty-name"] [ text "Set a parameter name" ]]

      else
        [ div [ class "use-with" ] [
            span [] [ text ("${"++(canonify param.name)++"}") ]
          ]
        , div [ class "full-name" ] [
            span [] [ text (technique.id.value ++"." ++ (canonify param.name))]
          ]

        ]
  in
    li [] [
      span [ class "border" ] []
    , div [ class "param" ] [
        div [ class "input-group" ] [
          input [ type_ "text",  class "form-control", value param.name, placeholder "Parameter name", onInput (\s -> TechniqueParameterModified param.id {param | name = s }), required True] []
        , div [ class "input-group-btn" ] [
            button [ class "btn btn-outline-secondary clipboard", title "Copy to clipboard" , onClick (Copy ("${" ++ (canonify param.name) ++ "}")) ] [
              i [ class "ion ion-clipboard" ] []
            ]
          ]
        ]
      , div [] param_name
      , button [ class "btn btn-sm btn-outline-primary",  style "margin-top" "5px" , onClick (TechniqueParameterToggle param.id)] [
          text "Description "
        , i [ class (if opened then "fa fa-times" else "ion ion-edit") ] []
        ]
      , if opened then textarea [ style "margin-top" "10px",  class "form-control",  rows 1,  value param.description, onInput (\s -> TechniqueParameterModified param.id {param | description = s })] [] else text "" -- msd-elastic
      ]
    , div [ class "remove-item", onClick (TechniqueParameterRemoved param.id) ] [
        i [ class "fa fa-times"] []
      ]
    ]

techniqueTab : Model -> Technique -> Bool -> TechniqueUIInfo -> Html Msg
techniqueTab model technique creation ui =
  case ui.tab of
    General -> div [ class "tab tab-general" ] [
                         {-div [ class "col-xs-12" ] [
                           div [ class "alert alert-warning" ] [ -- ng-if="conflictFlag">
                             text """Your changes have been kept. If you save this Technique, your version will replace the current one.
                                   You can still reset it or click on the following button to update it."""
                           , div [ class "btn-group-alert" ] [
                               button [ class "btn btn-sm btn-default" ] [-- ng-click="resetTechnique()">Update</button>
                                 text "Update"
                               ]
                             ]
                           ]
                         ]
                       ,-} div [ class "row form-group" ] [--ng-class="{'has-error':ui.editForm.name.$dirty && (ui.editForm.name.$error.required || ui.editForm.name.$error.techniqueName)}">
                           label [ for "techniqueName", class "col-xs-12 control-label" ] [ text "Name" ]
                         , div  [ class "col-sm-8" ] [
                             input [type_ "text" , id "techniqueName",  name "name",  class "form-control" , placeholder "Technique Name", value technique.name
                              , onInput (\newName -> UpdateTechnique {technique | name = newName, id = TechniqueId(if creation then canonify newName else technique.id.value) })
                              ] []
                           ]
                         , case ui.nameState of
                             InvalidState AlreadyTakenName -> span [ class "text-danger col-sm-8" ] [ text "Technique name must be unique" ]
                             InvalidState EmptyName -> span [ class "text-danger col-sm-8" ] [ text "Technique name is required" ]
                             _ -> text ""
                         ]
                       , div [ class "row form-group" ] [
                           label [ for "techniqueDescription", class "col-xs-12 control-label" ] [
                             span [ class "text-fit" ] [ text "Documentation" ]
                           , img  [ class "markdown-icon tooltip-icon popover-bs",  src "../../techeditor/css/markdown-mark-solid.svg" ] []
                                     --data-toggle="popover"
                                    -- data-trigger="hover"
                                    -- data-container="body"
                                    -- data-placement="right"
                                    -- data-content="This content will be displayed as <b>Markdown</b> in <b>directives</b> using this technique"
                                   --  data-html="true"
                           ]
                         , div [ class "col-sm-8" ] [
                             textarea [ name "description",  class "form-control technique-description", id "techniqueDescription", rows  4, value technique.description, placeholder "documentation"
                             , onInput (\desc -> UpdateTechnique {technique | description = desc })
                             ] []--msd-elastic
                           ]
                         ]
                       , div [ class "row form-group" ] [ -- show-errors>
                           label [ for "bundleName", class "col-xs-12 control-label"] [ text "ID" ]
                         , div [ class "col-sm-8" ] [
                             input [ readonly True,  id "bundleName", name "bundle_name", class "form-control", value technique.id.value ] [] -- bundlename ng-model="selectedTechnique.bundle_name" ng-maxlength="252" ng-pattern="/^[^_].*$/">
                           ]
                         , case ui.idState of
                             InvalidState TooLongId -> span [ class "text-danger col-sm-8 col-sm-offset-3" ] [text "Technique IDs longer than 255 characters won't work on most filesystems." ]
                             InvalidState AlreadyTakenId -> span [ class "text-danger col-sm-8 col-sm-offset-3" ] [ text "Technique ID must be unique." ]
                             InvalidState InvalidStartId -> span [ class "text-danger col-sm-8 col-sm-offset-3"] [ text "Technique ID should start with an alphanumeric character." ]
                             _ -> if String.length technique.id.value > 100 then
                                    span [ class "rudder-text-warning col-sm-8 col-sm-offset-3" ] [
                                      b [] [ span [ class "glyphicon glyphicon-exclamation-sign" ] [] ]
                                    , text "Technique IDs longer than 100 characters may not work on some filesystems (Windows, in particular)."
                                    ]
                                  else text ""
                         ]
                       , div [ class "row form-group" ] [ -- show-errors>
                           label [ for "category",  class "col-xs-12 control-label"] [ text "Category" ]
                         , div [ class "col-sm-8" ] [
                             select [ readonly (not creation),  class "form-control", name "category", id "category", value technique.category]
                               (List.map (\c -> option [ selected (c.path == technique.category), value c.path] [ text c.name ] ) model.categories)

                           -- Used to be a else on creation with a readonly input a tried a readonly select <input  ng-if="originalTechnique.bundle_name !== undefined" readonly id="category" bundlename name="category" class="form-control" ng-model="getCategory(selectedTechnique.category).value">
                           ]
                         ]
                       ]
    None -> text ""
    Parameters ->
      let
        paramList =
          if List.isEmpty technique.parameters then
            [ li [ class "empty"] [
                span [] [ text "There is no parameter defined." ]
              , span [ class "warning-sign" ] [
                  i [ class "fa fa-info-circle" ] []
                ]
              ]
            ]
          else
            List.map (\p -> techniqueParameter technique p (List.member p.id ui.openedParameters ) ) technique.parameters
      in
        div [ class "tab tab-parameters" ] [
          ul [ class "files-list parameters" ]
            paramList
        , div [ class "text-center btn-manage" ] [
            div [ class "btn btn-success btn-outline", onClick (GenerateId (\s -> TechniqueParameterAdded (ParameterId s)))] [
              text "Add parameter "
            , i [ class  "fa fa-plus-circle"] []
            ]
          ]
        ]
    Resources ->
      let
        resourceList =
          if True then -- empty resources
            [ li [class "empty" ] [
              span [] [ text "There is no resource files."]
            , span [ class "warning-sign" ] [
                i [ class "fa fa-info-circle" ] []
              ]
            ] ]
          else
            List.map (techniqueResource technique) []
      in
        div [ class "tab tab-resources" ] [
          ul [ class "files-list" ]
            resourceList
        , div [ class "resources-uncommitted" ] [ -- ng-if="getResourcesByState(selectedTechnique.resources, 'new').length>0 || getResourcesByState(selectedTechnique.resources, 'deleted').length > 0">
            span [] [
              i [ class "fa fa-exclamation-triangle"] []
            , text "TODO resources to save" --There {{(getResourcesByState(selectedTechnique.resources, 'new').length + getResourcesByState(selectedTechnique.resources, 'deleted').length) > 1 ? "are" : "is" }} <b>{{(getResourcesByState(selectedTechnique.resources, 'new').length + getResourcesByState(selectedTechnique.resources, 'deleted').length)}}</b> unsaved file{{(getResourcesByState(selectedTechnique.resources, 'new').length + getResourcesByState(selectedTechnique.resources, 'deleted').length) > 1 ? "s" : "" }}, save your changes to complete upload.
            ]
          ]
        , div [ class "text-center btn-manage" ] [
            div [ class "btn btn-success btn-outline" ] [ --ng-click="fileManagerState.open = true"  >
              text "Manage resources "
            , i [ class "fa fa-folder" ] []
            ]
          ]
        ]
