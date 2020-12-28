module TechniqueList exposing (..)

import DataTypes exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


techniqueList : Model -> List Technique -> Html Msg
techniqueList model techniques =

                  --    <div class="empty" ng-if="listTechniques.length<=0 && techniques.length>0">
                    --                  No technique matches the search filter.
                      --              </div>

  let

    filteredTechniques = List.filter (\t -> String.contains model.techniqueFilter t.name) techniques
    html =
      if List.isEmpty techniques then
        [ div [ class "empty"] [text "The techniques list is empty."] ]
      else
        case filteredTechniques of
          []   ->  [ div [ class "empty"] [text "No technique matches the search filter."] ]
          list -> (List.map (techniqueItem model) list)
  in
  div [ class "template-sidebar sidebar-left col-techniques" ] [ -- ng-click="toggleDisplay(true)"
    div [ class "sidebar-header"] [
      div [ class "header-title" ] [
        h1 [] [ text "Techniques"]
      , div [ class "header-buttons"] [ -- Need to add technique-write rights
          label [class "btn btn-sm btn-primary", for "btn-import"] [ --ng-click="toggleDisplay(true)"
            text "Import "
          , i [ class "fa fa-upload" ] []
          , input [ id "btn-import",  type_ "file", class "hidden"] []--onchange="angular.element(this).scope().onImportFileChange(this)" >

          ]
        , button [ class "btn btn-sm btn-success", onClick NewTechnique ] [
            text "Create "
          , i [ class "fa fa-plus-circle"] []
          ]
        ]
      ]
    , div [ class "header-filter" ] [
        input [ class "form-control",  type_ "text",  placeholder "Filter", onInput UpdateTechniqueFilter  ]  []
      ]
    ]
  , div [ class "sidebar-body" ] [
      div [ class "techniques-list-container" ] [
        ul [] html

      ]
    ]
  ]




techniqueItem: Model -> Technique -> Html Msg
techniqueItem model technique =
  let
    activeClass = case model.mode of
                    TechniqueDetails t _ _ _ ->
                      if t.id == technique.id then
                        [ class "active"]
                      else
                        [ ]
                    _ -> []
  in
    li activeClass [
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
