module  JsonEncoder exposing (..)

import Json.Encode exposing (..)
import DataTypes exposing (..)


encodeTechnique: Technique -> Value
encodeTechnique technique =
  object [
    ("bundle_name", string technique.id.value)
  , ("version", string technique.version)
  , ("name", string technique.name)
  , ("description", string technique.description)
  , ("category", string technique.category)
  , ("parameter" , list encodeTechniqueParameters technique.parameters)
  , ("method_calls", list encodeMethodCall technique.calls)
  ]

encodeTechniqueParameters: TechniqueParameter -> Value
encodeTechniqueParameters param =
  object [
    ("id", string param.id.value)
  , ("name", string param.name)
  , ("description", string param.description)
  ]

encodeMethodCall: MethodCall -> Value
encodeMethodCall call =
  object [
    ("id", string call.id.value)
  , ("method_name", string call.methodName.value)
  , ("class_context", string call.condition)
  , ("component", string call.component)
  , ("parameters", list encodeCallParameters call.parameters)
  ]

encodeCallParameters: CallParameter -> Value
encodeCallParameters param =
  object [
    ("name", string param.id.value)
  , ("value", string param.value)
  ]