module HostDetail (Model, Action (..), init, view, update) where

import Debug
import Effects exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on, targetValue)
import Routes
import ServerApi exposing (Host, HostRequest, getHost, createHost)

type alias Model =
  { id : Maybe Int
  , cpu : Int
  , memory : Int
  , disk_space : Int
  , status : String
  }

type Action =
    NoOp
  | GetHost (Int)
  | ShowHost (Maybe Host)
  | SaveHost
  | HandleSaved (Maybe Host)

init : Model
init =
   Model Nothing 0 0 0 ""

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp ->
      (model, Effects.none)
    GetHost id ->
      (model, getHost id ShowHost)
    ShowHost maybeHost ->
      case maybeHost of
        Just host ->
          ( { model | id = Just host.id
                    , cpu = host.cpu
                    , memory = host.memory
                    , disk_space = host.disk_space
                    , status = host.status}
          , Effects.none
          )
        Nothing ->
          ( { model | id = Nothing
                    , cpu = 0
                    , memory = 0
                    , disk_space = 0
                    , status = "ACTIVE"}
          , Effects.none
          )
    SaveHost ->
      ( model, createHost {cpu = model.cpu, memory = model.memory, disk_space = model.disk_space, status = "ACTIVE"} HandleSaved )
    HandleSaved maybeHost ->
      case maybeHost of
        Just host ->
          ({ model | id = Just host.id
                   , cpu = host.cpu
                   , memory = host.memory
                   , disk_space = host.disk_space
                   , status = host.status }
            , Effects.map (\_ -> NoOp) (Routes.redirect Routes.HostListingPage)
          )
        Nothing ->
          Debug.crash "Save failed... we're not handling it..."

view : Signal.Address Action -> Model -> Html
view address model =
  div [] [
      h1 [] [text "Create Host"]
    , Html.form [class "form-horizontal"] [
        div [class "form-group"] [
            label [class "col-sm-2 control-label"] [text "CPU"]
          , div [class "col-sm-10"] [
              input [
                class "form-control"
                , name "cpu"
                , value (toString model.cpu)
              ] []
            ]
        ]
        , div [class "form-group"] [
            label [class "col-sm-2 control-label"] [text "Memory"]
          , div [class "col-sm-10"] [
              input [
                class "form-control"
                , name "memory"
                , value (toString model.memory)] []
            ]
        ]
        , div [class "form-group"] [
            label [class "col-sm-2 control-label"] [text "Disk Space"]
          , div [class "col-sm-10"] [
              input [
                class "form-control"
                , name "disk_space"
                , value (toString model.disk_space)] []
            ]
        ]
        , div [class "form-group"] [
            div [class "col-sm-offset-2 col-sm-10"] [
              button [
                  class "btn btn-default"
                , type' "button"
                , onClick address SaveHost
              ]
              [text "Save"]
            ]
        ]
    ]
  ]
