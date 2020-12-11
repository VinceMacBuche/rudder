module  JsonDecoder exposing (..)


import DataTypes exposing (..)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)

decodeTechniqueParameter : Decoder TechniqueParameter
decodeTechniqueParameter =
  succeed TechniqueParameter
    |> required "id" (map ParameterId string)
    |> required "name" string
    |> required "description" string

decodeMethodParameter : Decoder CallParameter
decodeMethodParameter =
  succeed CallParameter
    |> required "name" (map ParameterId string)
    |> required "value" string

decodeMethodCall : Decoder MethodCall
decodeMethodCall =
  succeed (MethodCall "id")
    |> required "method_name" (map MethodId string)
    |> required "parameters"  (list decodeMethodParameter )
    |> required "class_context"  string
    |> required "component"  string

decodeTechnique : Decoder Technique
decodeTechnique =
  succeed Technique
    |> required "bundle_name" (map TechniqueId string)
    |> required "version"  string
    |> required "name"  string
    |> required "description"  string
    |> required "category"  string
    |> required "method_calls" (list decodeMethodCall)
    |> required "parameter" (list decodeTechniqueParameter)
