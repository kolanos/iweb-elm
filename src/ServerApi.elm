module ServerApi where

import Effects exposing (Effects)
import Http
import Json.Decode as JsonD exposing ((:=))
import Json.Encode as JsonE
import Task


type alias HostRequest a =
  { a | cpu : Int
      , memory : Int
      , disk_space : Int
      , status : String }

type alias Host =
  { id : Int
  , cpu : Int
  , memory : Int
  , disk_space : Int
  , status : String
  }

baseUrl : String
baseUrl = "http://api.iwebhires.com"

getHost : Int -> (Maybe Host -> a) -> Effects.Effects a
getHost id action =
  Http.get hostDecoder (baseUrl ++ "/hosts/" ++ toString id)
    |> Task.toMaybe
    |> Task.map action
    |> Effects.task

getHosts : (Maybe (List Host) -> a) -> Effects a
getHosts action =
  Http.get hostsDecoder (baseUrl ++ "/hosts")
    |> Task.toMaybe
    |> Task.map action
    |> Effects.task

createHost : HostRequest a -> (Maybe Host -> b) -> Effects.Effects b
createHost host action =
  Http.send Http.defaultSettings
        { verb = "POST"
        , url = baseUrl ++ "/hosts"
        , body = Http.string (encodeHost host)
        , headers = [("Content-Type", "application/json")]
        }
    |> Http.fromJson hostDecoder
    |> Task.toMaybe
    |> Task.map action
    |> Effects.task

deleteHost : Int -> (Maybe Http.Response -> a) -> Effects.Effects a
deleteHost id action =
  Http.send Http.defaultSettings
        { verb = "DELETE"
        , url = baseUrl ++ "/hosts/" ++ toString id
        , body = Http.empty
        , headers = []
        }
    |> Task.toMaybe
    |> Task.map action
    |> Effects.task

hostsDecoder : JsonD.Decoder (List Host)
hostsDecoder =
  JsonD.list hostDecoder

hostDecoder : JsonD.Decoder Host
hostDecoder =
  JsonD.object5 Host
    ("id" := JsonD.int)
    ("cpu" := JsonD.int)
    ("memory" := JsonD.int)
    ("disk_space" := JsonD.int)
    ("status" := JsonD.string)

encodeHost : HostRequest a -> String
encodeHost a =
  JsonE.encode 0 <|
    JsonE.object
      [
        ("cpu", JsonE.int a.cpu),
        ("memory", JsonE.int a.memory),
        ("disk_space", JsonE.int a.disk_space),
        ("status", JsonE.string a.status)
      ]
