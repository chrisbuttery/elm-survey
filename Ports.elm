port module Ports exposing (..)

import Model exposing (Model)

-- Ports


port modelChange : Model -> Cmd msg

port getStoredModel : (Model -> msg) -> Sub msg

port newOptionTitle : (String -> msg) -> Sub msg
