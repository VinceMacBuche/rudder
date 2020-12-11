module  ApiCalls exposing (..)

import DataTypes exposing (..)
import Http exposing (..)
import JsonDecoder exposing (..)
import Json.Encode as Encode
import Json.Decode
import Dict


getUrl: Model -> String -> String
getUrl m url =
  "/" ++ m.contextPath ++ "/secure/api/" ++ url

getTechniques : Model -> Cmd Msg
getTechniques  model =
  let
    req =
      request
        { method  = "GET"
        , headers = []
        , url     = getUrl model "internal/techniques"
        , body    = emptyBody
        , expect  = expectJson GetTechniques ( Json.Decode.at ["data", "techniques" ] ( Json.Decode.list decodeTechnique))
        , timeout = Nothing
        , tracker = Nothing
        }
  in
    req