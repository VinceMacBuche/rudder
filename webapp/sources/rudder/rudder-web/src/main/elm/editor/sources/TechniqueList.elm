module TechniqueList exposing (..)

import DataTypes exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


techniqueList : List Technique -> Html Msg
techniqueList techniques =

                  --    <div class="empty" ng-if="listTechniques.length<=0 && techniques.length>0">
                    --                  No technique matches the search filter.
                      --              </div>

  let
    html = case techniques of
        [] -> [ div [ class "empty"] [text "The techniques list is empty."] ]
        --  filter ->  [ div [ class "empty"] [text "No technique matches the search filter."] ]
        list -> (List.map techniqueItem list)
  in
  div [ class "template-sidebar sidebar-left col-techniques" ] [ -- ng-click="toggleDisplay(true)"
    div [ class "sidebar-header"] [
      div [ class "header-title" ] [
        h1 [] [ text "Techniques"]
      , div [ class "header-buttons"] [ -- Need to add technique-write rights
          label [class "btn btn-sm btn-primary", for "btn-import"] [ --ng-click="toggleDisplay(true)"
            text "Import "
          , i [ class "fa fa-upload" ] []
          , input [ id "btn-import",  type_ "file", class "hidden" ] []--onchange="angular.element(this).scope().onImportFileChange(this)" >

          ]
        , button [ class "btn btn-sm btn-success" ] [-- ng-click="newTechnique();$event.stopPropagation();"
            text "Create "
          , i [ class "fa fa-plus-circle"] []
          ]
        ]
      ]
    , div [ class "header-filter" ] [
        input [ class "form-control",  type_ "text",  placeholder "Filter" ]  []--ng-model="searchText.name">
      ]
    ]
  , div [ class "sidebar-body" ] [
      div [ class "techniques-list-container" ] [
        ul [] html

      ]
    ]
  ]




techniqueItem: Technique -> Html Msg
techniqueItem technique =
  let
    activeClass = """ng-class="{'active': isSelected(technique)}">"""
  in
    li [ ] [ --  ng-class="{'active': isSelected(technique)}"
      div [ class "item",  onClick (SelectTechnique technique) ] [
        span [] [ text technique.name ]
        --, <span class="cursor-help popover-bs"  ng-if="hasDeprecatedMethod(technique)" data-toggle="popover"
        --  data-trigger="hover" data-container="body" data-placement="right" data-title="{{technique.name}}"
        --  data-content="<div>This technique uses <b>deprecated</b> generic methods.</div>"
        --  data-html="true">
         --   <i class="glyphicon glyphicon-info-sign deprecated-icon"></i>
         --     </span>
           --                       </div>
      ]
    , div [ class "col-auto align-self-center" ,  onClick (SelectTechnique technique) ] [--ng-click="checkSelect(technique,selectTechnique);toggleDisplay(false); $event.stopPropagation();">
        i [ class "ion ion-edit" ] []
      ]
    ]
