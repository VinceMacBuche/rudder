module MethodCall exposing (..)

import DataTypes exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dict
import List.Extra
import String.Extra
import Regex
import DnDList




{- Should be done otherwise in elm
showParam: CallParameter -> Html Msg
showParam param =
 div [ class "method-errors" ] [ -- ng-form="arg_method"
  input ng-hide="true" type="text" name="param" ng-model="parameter.value" constraint parameter="parameter" parameterinfo="(method.parameter[$index])" >
                          </div>
-}

getClassParameter: Method -> MethodParameter
getClassParameter method =
  case findClassParameter method of
    Just p -> p
    Nothing -> MethodParameter method.classParameter "" "" []

canonify: String -> String
canonify value =
   Regex.replace ((Regex.fromString >> Maybe.withDefault Regex.never) "[!_a-zA-Z\\d]") (always "_") value

findClassParameter: Method -> Maybe MethodParameter
findClassParameter method =
  List.Extra.find (\p -> p.name == method.classParameter) method.parameters

parameterName: MethodParameter -> String
parameterName param =
  String.replace "_" " " (String.Extra.toSentenceCase param.name.value)

showParam: MethodParameter -> CallParameter -> Html Msg
showParam methodParam param =
  div [class "form-group method-parameter"] [
    label [ for "param-index" ] [
      span [] [
        text (param.id.value ++ " - ")
      , span [ class "badge badge-secondary ng-binding" ] [ text methodParam.type_ ]
      ]
    , small [] [ text methodParam.description ]
    ]
  , textarea  [ name "param", class "form-control", rows  1 , id "param-{{$index}}" , value param.value ] [] --msd-elastic     ng-trim="{{trimParameter(parameterInfo)}}" ng-model="parameter.value"></textarea>
  , ul [ class "list-unstyled" ]
      (List.map (\e -> li [ class "text-danger" ] [ text e ]) [])
  ]


showMethodTab: Method -> MethodCall -> MethodCallTab -> Html Msg
showMethodTab method call tab =
  case tab of
    CallParameters ->
      div [ class "tab-paramerers"] (List.map2 showParam method.parameters call.parameters)
    Conditions -> text ""
    {-
                        <div class="tab-conditions" ng-if="ui.methodTabs[method_call['$$hashKey']]=='conditions'">
                          <div class="form-group condition-form" id="os-form">
                            <label for="os_class">Operating system:</label>
                            <form class="form-inline" role="form">
                              <div class="form-group">
                                <label for="os_class">Type:</label>
                                <select class="form-control" ng-change="updateOSType(method_call)" ng-model="method_call.OS_class.type" ng-options="type for type in type_classes" ></select>
                              </div>
                              <div class="form-group" ng-show="os_classes_by_type[method_call.OS_class.type].length > 0" >
                                <label for="os_class">Name:</label>
                                <select class="form-control" ng-change="updateOSName(method_call)" ng-model="method_call.OS_class.name" ng-options="os for os in os_classes_by_type[method_call.OS_class.type]"></select>
                              </div>
                            </form>
                            <form class="form-inline sm-space-top" name="CForm.versionForm" role="form">
                              <div class="form-group" ng-show="checkMajorVersion(method_call)">
                                <label for="os_class">Version (Major):</label>
                                <input type="text" ng-pattern="versionRegex" class="form-control" ng-change="updateClassContext(method_call)" ng-model="method_call.OS_class.majorVersion" name="versionMaj" placeholder="">
                              </div>
                              <div class="form-group" ng-show="checkMinorVersion(method_call)">
                                <label for="os_class">Version (Minor):</label>
                                <input type="text"  ng-pattern="versionRegex" class="form-control" ng-change="updateClassContext(method_call)" ng-disabled="method_call.OS_class.majorVersion === undefined || method_call.OS_class.majorVersion === '' " ng-model="method_call.OS_class.minorVersion"  placeholder="" name="versionMin">
                              </div>
                              <div ng-messages="CForm.versionForm.versionMaj.$error" class="sm-space-top" role="alert">
                                <div ng-message="pattern" class="text-danger">Invalid major version's number</div>
                              </div>
                              <div ng-messages="CForm.versionForm.versionMin.$error" role="alert">
                                <div ng-message="pattern" class="text-danger">Invalid minor version's number</div>
                              </div>
                            </form>
                          </div>
                          <form name="CForm.form">
                            <div class="form-group condition-form">
                              <label for="advanced">Other conditions:</label>
                              <textarea msd-elastic name="cfClasses" class="form-control" rows="1" id="advanced" ng-pattern="/^[a-zA-Z0-9_!.|${}\[\]()@:]+$/" ng-model="method_call.advanced_class" ng-change="updateClassContext(method_call)"></textarea>
                              <div ng-messages="CForm.form.cfClasses.$error" role="alert">
                                <div ng-message="pattern" class="text-danger">This field should only contains alphanumerical characters (a-zA-Z0-9) or the following characters _!.|${}[]()@:</div>
                              </div>
                            </div>
                            <div class="form-group condition-form">
                              <label for="class_context">Applied condition expression:</label>
                              <textarea msd-elastic name="cfClassContext" class="form-control" rows="1" id="advanced" ng-maxlength="2048" ng-model="method_call.class_context" ng-readonly="true"></textarea>
                              <span class="text-danger" ng-show="CForm.form.cfClassContext.$error.maxlength">
                                    Classes over 2048 characters are currently not supported.
                                  </span>
                            </div>
                          </form>
                        </div>

                          -}
    Result     ->
      let
        classParameter = getClassParameter method
        paramValue = call.parameters |> List.Extra.find (\c -> c.id == classParameter.name) |> Maybe.map (.value)  |> Maybe.withDefault ""
      in
      div [ class "tab-result" ] [
        label [] [
          small [] [ text "Result conditions defined by this method" ]
        ]
      , div [ class "form-horizontal editForm result-class" ] [
          div [ class "input-group result-success" ] [
            div [ class "input-group-addon" ] [
              text "Success"
            ]
          , input [ readonly True, type_ "text", class "form-control",  value (classParameter.name.value ++ "_" ++ (canonify paramValue) ++ "_kept") ] []
          , span [ class "input-group-btn" ] [
              button [ class "btn btn-outline-secondary clipboard", title "Copy to clipboard" ] [ --data-clipboard-text="{{getClassKind(method_call,'kept')}}" title="Copy to clipboard">
                i [ class "ion ion-clipboard" ] []
              ]
            ]
          ]
        , div [ class "input-group result-repaired" ] [
            div [ class "input-group-addon" ] [
              text "Repaired"
            ]
          , input [ readonly True, type_ "text", class "form-control",  value (classParameter.name.value ++ "_" ++ (canonify paramValue) ++ "_repaired") ] []
          , span [ class "input-group-btn" ] [
              button [ class "btn btn-outline-secondary clipboard", title "Copy to clipboard" ] [ --data-clipboard-text="{{getClassKind(method_call,'kept')}}" title="Copy to clipboard">
                i [ class "ion ion-clipboard" ] []
              ]
            ]
          ]
        , div [ class "input-group result-error" ] [
            div [ class "input-group-addon" ] [
              text "Error"
            ]
          , input [ readonly True, type_ "text", class "form-control",  value (classParameter.name.value ++ "_" ++ (canonify paramValue) ++ "_error") ] []
          , span [ class "input-group-btn" ] [
              button [ class "btn btn-outline-secondary clipboard", title "Copy to clipboard" ] [ --data-clipboard-text="{{getClassKind(method_call,'kept')}}" title="Copy to clipboard">
                i [ class "ion ion-clipboard" ] []
              ]
            ]
          ]
        ]
      ]

methodDetail: Method -> MethodCall -> MethodCallTab -> Html Msg
methodDetail method call currentTab =
  let
    activeClass = (\c -> if c == currentTab then "active" else "" )
  in
  div [ class "method-details" ] [
    div [] [
      div [ class "form-group"] [
        label [ for "component"] [ text "Report component:"]
      , input [ type_ "text", name "component", class "form-control", value call.component,  placeholder method.name] []
      ]
    , ul [ class "tabs-list"] [
        li [ class (activeClass CallParameters), onClick (SwitchTabMethod call.id CallParameters) ] [text "Parameters"] -- click select param tabs, class active si selectionnée
      , li [ class (activeClass Conditions), onClick (SwitchTabMethod call.id Conditions) ] [text "Conditions"]
      , li [class (activeClass Result), onClick (SwitchTabMethod call.id Result) ] [text "Result conditions"]
      ]
    , div [ class "tabs" ] [ (showMethodTab method call CallParameters) ]
    ]
  ]

showMethodCall: Model -> MethodCallMode -> MethodCallTab -> DnDList.Model -> Int -> MethodCall -> Html Msg
showMethodCall model mode tab dnd index call =
  let
    editAction = case mode of
                   Opened -> CloseMethod call.id
                   Closed -> OpenMethod  call.id
    method = case Dict.get call.methodName.value model.methods of
               Just m -> m
               Nothing -> Method call.methodName call.methodName.value "" "" (Maybe.withDefault (ParameterId "") (Maybe.map .id (List.head call.parameters))) [] [] Nothing Nothing Nothing
    dragAttributes =
       class "method" :: id call.id ::
       case dndSystem.info dnd of
         Just { dragIndex } ->
           if dragIndex /= index then
              dndSystem.dropEvents index call.id
           else
             []

         Nothing ->
           dndSystem.dragEvents index call.id
  in
  {-
      let
          itemId : String
          itemId =
              "id-" ++ item
      in
      case system.info dnd of
          Just { dragIndex } ->
              if dragIndex /= index then
                  Html.p
                      (Html.Attributes.id itemId :: system.dropEvents index itemId)
                      [ Html.text item ]

              else
                  Html.p
                      [ Html.Attributes.id itemId ]
                      [ Html.text "[---------]" ]

          Nothing ->
              Html.p
                  (Html.Attributes.id itemId :: system.dragEvents index itemId)
                  [ Html.text item ]
  -}
    li [] [ --     ng-class="{'active': methodIsSelected(method_call), 'missingParameters': checkMissingParameters(method_call.parameters, method.parameter).length > 0, 'errorParameters': checkErrorParameters(method_call.parameters).length > 0, 'is-edited' : canResetMethod(method_call)}"
      div dragAttributes [ {-
                    dnd-draggable="method_call"
                    dnd-effect-allowed="move"
                    dnd-moved="selectedTechnique.method_calls.splice($index,1)">
                    -}
        div [ class "cursorMove"] [ p [] [ text ":::"]]

      , div [ class "method-info"] [ -- dnd-nodrag>
          div [ class "btn-holder" ] [
            button [ class "text-success method-action tooltip-bs", title "Clone this method", attribute "data-toogle" "tooltip"
                   , attribute "data-trigger" "hover", attribute "data-container" "body", attribute "data-placement" "left"
                   , attribute "data-html" "true", attribute "data-delay" """'{"show":"400", "hide":"100"}'""" ] [
              i [ class "fa fa-clone"] []
            ]

            , button [  class "text-danger method-action", onClick (GenerateId (CloneMethod call) ) ] [ --  ng-click="removeMethod($index);$event.stopPropagation();"
              i [ class "fa fa-times-circle" ] []
            ]
          ]
        , div [ class "flex-column" ] [
            div [ class "method-condition flex-form" ] [ -- ng-if="method_call.class_context !== 'any'">
              label [] [ text "Condition:" ]
            , textarea [ class "form-control popover-bs", rows 1, readonly True, value call.condition, title call.condition
                          --msd-elastic
                          --ng-click="$event.stopPropagation();"
                       , attribute "data-toggle" "popover", attribute "data-trigger" "hover", attribute "data-placement" "top"
                       , attribute "data-title" call.condition, attribute "data-content" "<small>Click <span class='text-info'>3</span> times to copy the whole condition below</small>"
                       , attribute "data-template" """<div class="popover condition" role="tooltip"><div class="arrow"></div><h3 class="popover-header"></h3><div class="popover-body"></div></div>"""
                       , attribute "data-html" "true" ] []
            ]
          , div [ class "method-name" ] [
              text (if (String.isEmpty call.component) then method.name else call.component)
            , span [ class "cursor-help" ] [
                i [ class "fa fa-info-circle tooltip-icon popover-bs"
                            --ng-class="{'deprecated-icon':(method_call.deprecated || isDeprecated(method_call.method_name))}"
                  , attribute "data-toggle" "popover", attribute "data-trigger" "hover", attribute "data-container" "body"
                  , attribute "data-placement" "auto", attribute "data-title" method.name, attribute "data-content" "{{getTooltipContent(method_call)}}"
                  , attribute "data-html" "true" ]  []
              ]
            ]
          , div [ class "method-content"] [
              div [ class "method-param flex-form" ] [ --  ng-if="getClassParameter(method_call).value && checkMissingParameters(method_call.parameters, method.parameter).length<=0 && checkErrorParameters(method_call.parameters).length<=0"
                label [] [ text ((parameterName (getClassParameter method))++":")]
              , textarea [ class "form-control", rows 1, readonly True ] [] -- msd elastic ng-model="getClassParameter(method_call).value"  ng-click="$event.stopPropagation();"
              ]
            -- here used to be one entry for each for each param but we can be smart with elm

{- error display <div class="warns" ng-if="!getClassParameter(method_call).value || checkMissingParameters(method_call.parameters, method.parameter).length>0 || checkErrorParameters(method_call.parameters).length>0">
                                <span
                                  class="warn-param warning popover-bs"
                                  ng-click="selectMethod(method_call)"
                                  data-toggle="popover"
                                  data-trigger="hover"
                                  data-container="body"
                                  data-placement="top"
                                  data-title="<b>{{checkMissingParameters(method_call.parameters, method.parameter).length}}</b> required parameter{{checkMissingParameters(method_call.parameters, method.parameter).length > 1 ? 's' : ''}} missing"
                                  data-content="{{getWarningTooltipMessage(checkMissingParameters(method_call.parameters, method.parameter))}}"
                                  data-html="true"
                                  >
                                  <b>{{checkMissingParameters(method_call.parameters, method.parameter).length}}</b> required parameter{{checkMissingParameters(method_call.parameters, method.parameter).length > 1 ? 's' : ''}} missing
                                </span>
                                <span
                                  class="warn-param error popover-bs"
                                  ng-click="selectMethod(method_call)"
                                  data-toggle="popover"
                                  data-trigger="hover"
                                  data-container="body"
                                  data-placement="top"
                                  data-title="<b>{{checkErrorParameters(method_call.parameters).length}}</b> invalid parameter{{checkErrorParameters(method_call.parameters).length > 1 ? 's' : ''}}"
                                  data-content="{{getErrorTooltipMessage(checkErrorParameters(method_call.parameters))}}"
                                  data-html="true"
                                  >
                                  <b>{{checkErrorParameters(method_call.parameters).length}}</b> invalid parameter{{checkErrorParameters(method_call.parameters).length > 1 ? "s" : ""}}
                                </span>
                              </div> -}
            ]
          ]
        ]
      , div [ class "edit-method popover-bs", onClick editAction ] [
      {-
                    ng-click="selectMethod(method_call)"
                    dnd-nodrag
                    data-toggle="popover"
                    data-trigger="hover"
                    data-container="body"
                    data-placement="left"
                    data-content="{{getStatusTooltipMessage(method_call)}}"
                    data-html="true"
                    data-delay='{"show":"400", "hide":"100" -}

          i [ class "ion ion-edit"] []
        ]
      ]
    , case mode of
       Opened -> div [ class "method-details"] [ methodDetail method call tab ]
       Closed -> div [] []
    , div [ class "method-details-footer"] [
        button [ class "btn btn-outline-secondary btn-sm" , disabled True] [ -- ng-disabled="!canResetMethod(method_call)" ng-click="resetMethod(method_call)"
          text "Reset "
        , i [ class "fa fa-undo-all"] []
        ]
      , case method.documentation of
          Just doc -> button [ class "btn btn-sm btn-primary show-doc"] [
                    {-              ng-class="{ 'doc-opened' : generic_methods[method_call.method_name].showDoc}"
                                  ng-click="showMethodDocumentation($event,method_call.method_name)">
                      -}
                        text "Show docs"
                      , i [ class "fa fa-book"] []
                      ]
          Nothing -> text ""
      ]
    ]