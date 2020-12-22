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

decodeCallParameter : Decoder CallParameter
decodeCallParameter =
  succeed CallParameter
    |> required "name" (map ParameterId string)
    |> required "value" string

decodeMethodCall : Decoder MethodCall
decodeMethodCall =
  succeed MethodCall
    |> required "id" string
    |> required "method_name" (map MethodId string)
    |> required "parameters"  (list decodeCallParameter )
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


decodeAgent : Decoder Agent
decodeAgent =
  andThen (\v ->
    case v of
      "cfengine-community" -> succeed Cfengine
      "dsc"      -> succeed Dsc
      _          -> fail (v ++ " is not a valid agent")
  ) string


decodeConstraint: Decoder (List Constraint)
decodeConstraint =
  andThen ( \v ->
    List.foldl (\val acc ->
      case val of
        ("allow_empty_string", value) ->
          case decodeValue bool value of
            Ok b -> andThen (\t -> succeed (AllowEmpty b :: t) ) acc
            Err e-> fail (errorToString e)
        ("allow_whitespace_string", value) ->
          case decodeValue bool value of
            Ok b -> andThen (\t -> succeed (AllowWhiteSpace b :: t) ) acc
            Err e-> fail (errorToString e)
        ("max_length", value) ->
          case decodeValue int value of
            Ok b -> andThen (\t -> succeed (MaxLength b :: t) ) acc
            Err e-> fail (errorToString e)
        ("min_length", value) ->
          case decodeValue int value of
            Ok b -> andThen (\t -> succeed (MinLength b :: t) ) acc
            Err e-> fail (errorToString e)
        ("regex", value) ->
          case decodeValue string value of
            Ok b -> andThen (\t -> succeed (MatchRegex b :: t) ) acc
            Err e-> fail (errorToString e)
        ("not_regex", value) ->
          case decodeValue string value of
            Ok b -> andThen (\t -> succeed (NotMatchRegexp b :: t) ) acc
            Err e-> fail (errorToString e)
        ("select", value) ->
          case decodeValue (list string) value of
            Ok b -> andThen (\t -> succeed (Select b :: t) ) acc
            Err e-> fail (errorToString e)
        _ -> acc

    ) (succeed []) v

  ) (keyValuePairs value)

decodeMethodParameter: Decoder MethodParameter
decodeMethodParameter =
  succeed MethodParameter
    |> required "name" (map ParameterId string)
    |> required "description" string
    |> required "type" string
    |> required "constraints" decodeConstraint

decodeMethod : Decoder Method
decodeMethod =
  succeed Method
    |> required "bundle_name" (map MethodId string)
    |> required "name" string
    |> required "description" string
    |> required "class_prefix" string
    |> required "class_parameter" (map ParameterId string)
    |> required "agent_support" (list decodeAgent)
    |> required "parameter" (list decodeMethodParameter)
    |> optional "documentation" (maybe string) Nothing
    |> optional "deprecated" (maybe string) Nothing
    |> optional "rename" (maybe string) Nothing