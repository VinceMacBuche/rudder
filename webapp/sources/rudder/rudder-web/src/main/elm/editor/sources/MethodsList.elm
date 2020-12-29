module MethodsList exposing (..)

import DataTypes exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dict
import Dict.Extra
import List.Extra




showMethod: Method -> Html Msg
showMethod method =
  li []
    ( div [ class "method", id method.id.value ]  --ng-class="{'used':isUsed(method), 'doc-opened':method.showDoc}"
        --              dnd-draggable="method"
        --              dnd-effect-allowed="move"
        --              dnd-type="'bundle'"
      (div [ class "cursorMove" ] [
        b [] [ text ":::" ]
      ] ::
      div [ class "method-name col" ] -- ng-click="addMethod(method)">
        ( text method.name ::
          ( case method.deprecated of
            Nothing -> text ""
            Just deprecationMessage ->
              span [ class "cursor-help" ] [
                i [ class "fa fa-info-circle tooltip-icon deprecated-icon popover-bs"
                  , attribute "data-toggle" "popover", attribute "data-trigger" "hover"
                  , attribute "data-container" "body", attribute "data-placement" "top"
                  , attribute "data-title" method.name, attribute "data-content" "{{getTooltipContent(method)}}"
                  , attribute "data-html" "true"
                ] []
              ]
          ) ::
          if (List.member Dsc method.agentSupport) then  [ img [ src "../../techeditor/css/dsc-icon.svg",  class "dsc-icon" ] [] ] else []
        )
       ::
      case method.documentation of
       Just doc ->
         [ div [ class "show-doc" ] [-- ng-click="$event.stopPropagation(); method.showDoc = !method.showDoc">
           i [ class "fa fa-book" ] [] ] ]
       Nothing -> []
      )
     ::
    case method.documentation of
      Just doc ->
        [ div [ class "markdown" ] [] ] -- ng-if="method.documentation !== undefined && method.showDoc" ng-bind-html="getMethodDocumentationHtml(method)"></div>
      Nothing -> []
    )
showMethodsCategories : Model -> (String, List Method) -> Html Msg
showMethodsCategories model (category, methods) =
  ul [ class "list-unstyled" ]
    (h5 [ id category ] [ text category ]
    :: (List.map showMethod methods) )

filterMethod: MethodFilter -> Method -> Bool
filterMethod filter method =
  (String.contains filter.name method.name) &&
    ( case filter.agent of
      Nothing -> True
      Just ag -> List.member ag method.agentSupport
    ) && (filter.showDeprecated ||
           case method.deprecated of
             Nothing -> True
             _ -> False
         )

methodsList: Model -> Html Msg
methodsList model =
  let
    filter = model.methodFilter
    filterMethods = List.filter (filterMethod filter) (Dict.values model.methods)
    methodByCategories = Dict.Extra.groupBy (\m -> Maybe.withDefault m.id.value (List.head (String.split "_" m.id.value))) (filterMethods)
    dscIcon = if filter.agent == Just Dsc then "dsc-icon-white.svg" else "dsc-icon.svg"
  in
    div [ class "template-sidebar sidebar-right col-methods" ] [ -- ng-click="toggleDisplay(false)" ng-show="selectedTechnique">
    div [ class "sidebar-header" ] [
      div  [ class "header-title" ] [
        h1 [] [ text "Generic Methods" ]
      , div [ class "header-buttons" ] [
          button [ class "btn btn-sm btn-default" ] [ text "Close"] -- ng-click="toggleDisplay(true); ; $event.stopPropagation();">Close</button>
        ]
      ]

    , div [ class "header-filter" ] [
        div [ class "input-group" ] [
          input [ class "form-control",  type_ "text",  placeholder "Filter", value model.methodFilter.name, onInput (\s -> UpdateMethodFilter { filter | name = s  }) ] [] 
        , div [ class "input-group-btn" ] [
            button [ class "btn btn-outline-secondary btn-toggle-filters" ] [ --ng-click="ui.showMethodsFilter=!ui.showMethodsFilter">
              i [ class "ion ion-android-options"] []
            ]
          ]
        ]
      ]
    ]
  , div [ class "filters-container" ] [-- ng-class="{'hidden':!ui.showMethodsFilter}">
      label [ class "label-btn-group align-self-center" ] [
        text "Agent type:"
      ]
    , div [ class "btn-group" ] [
        button [ class ("btn btn-default" ++ (if filter.agent == Nothing then " active" else "")), onClick (UpdateMethodFilter {filter | agent = Nothing })  ] [text "All"]
      , button [ class ("btn btn-default" ++ (if filter.agent == Just Cfengine then " active" else "")), onClick (UpdateMethodFilter {filter | agent = Just Cfengine }) ] [text "Classic"]
      , button [ class ("btn btn-default" ++ (if filter.agent == Just Dsc then " active" else "")), onClick (UpdateMethodFilter {filter | agent = Just Dsc }) ] [
          text "DSC "
        , img [ src ("../../techeditor/css/" ++ dscIcon),  class "dsc-icon" ] []
        ]
      ]
    , div [ class "input-group" ] [
        label [ for "showDeprecated", class "input-group-addon" ] [
          input [ id "showDeprecated",  type_ "checkbox", checked filter.showDeprecated, onCheck (\b -> UpdateMethodFilter { filter | showDeprecated = b}) ]  []
        ]
      , label [ for "showDeprecated",  class "form-control label-checkbox" ][
          text "Show deprecated generic methods"
        , i [ class "fa fa-info-circle deprecated-icon" ] []
        ]
      ]
    ]

  , div [ class "sidebar-body" ] [
      div [ class "generic-methods-container" ] [
        if List.isEmpty (Dict.toList model.methods) then
          div [ class "empty" ] [ text "No method matches the filters." ]

        else
          div [ id "methods-list-container" ] (List.map (showMethodsCategories model) (Dict.toList methodByCategories) )
      , ul [ id "categories-list" ]
          ( h4 [ id "categories" ] [ text "Categories" ] ::
            List.map (showCategory model) (Dict.keys methodByCategories)
          )

      ]
    ]
  ]

showCategory: Model -> String -> Html Msg
showCategory model category=
  li [ class "active" ] [ --ng-if="checkFilterCategory(methods)" ng-class="{'deprecatedCategory':!checkDeprecatedFilter(methods)}">
    a [ href "" ] [ --ng-click="scroll(kind)">
      text category
    {-                <span
                    class="cursor-help popover-bs"
                    ng-if="!checkDeprecatedFilter(methods)"
                    data-toggle="popover"
                    data-trigger="hover"
                    data-container="body"
                    data-placement="bottom"
                    data-title="{{capitaliseFirstLetter(kind)}}"
                    data-content="<div>All generic methods in this category are <b>deprecated</b>.</div>"
                    data-html="true"
                    ><i class="glyphicon glyphicon-info-sign deprecated-icon"></i>
                    </span>
                  </a>
                </li>

                -}
    ]
  ]