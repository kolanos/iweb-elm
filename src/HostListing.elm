module HostListing (Model, Action (..), init, view, update) where

import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on, targetValue)
import Http

import Routes
import ServerApi exposing (..)


type alias Model =
  {hosts : List Host}


type Action =
    Show
  | HandleHostsRetrieved (Maybe (List Host))
  | DeleteHost (Int)
  | HandleHostDeleted (Maybe Http.Response)


init : Model
init =
  Model []


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Show ->
      (model, getHosts HandleHostsRetrieved)

    HandleHostsRetrieved xs ->
      ( {model | hosts = (Maybe.withDefault [] xs) }
      , Effects.none
      )

    DeleteHost id ->
      ( model, deleteHost id HandleHostDeleted)

    HandleHostDeleted res ->
      ( model, getHosts HandleHostsRetrieved)


hostRow : Signal.Address Action -> Host -> Html
hostRow address host =
  tr [] [
      td [] [text (toString host.id)]
     ,td [] [text (toString host.cpu ++ " Cores")]
     ,td [] [text (toString host.memory ++ " MB")]
     ,td [] [text (toString host.disk_space ++ " MB")]
     ,td [] [text host.status]
     ,td [] [button [onClick address (DeleteHost (.id host))] [text "Delete"]]
  ]


view : Signal.Address Action -> Model -> Html
view address model =
  div [] [
      h1 [] [text "Hosts"]
    , button [
            class "pull-right btn btn-default"
          , Routes.clickAttr Routes.NewHostPage
        ]
        [text "New Host"]
    , table [class "table table-striped"] [
          thead [] [
            tr [] [
               th [] [text "ID"]
               , th [] [text "CPU"]
               , th [] [text "Memory"]
               , th [] [text "Disk Space"]
               , th [] [text "Status"]
              ,th [] []
          ]
        ]
        , tbody [] (List.map (hostRow address) model.hosts)
    ]
  ]
