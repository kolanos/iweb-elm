module HostDetail (Model, Action (..), init, view, update) where

import Debug
import Effects exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on, targetValue)
import String exposing (toInt)

import Routes
import ServerApi exposing (Host, HostRequest, getHost, createHost)


convertToInt : String -> Int
convertToInt str =
  toInt str |> Result.toMaybe |> Maybe.withDefault 0


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
  | SetHostCPU (String)
  | SetHostMemory (String)
  | SetHostDiskSpace (String)
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

    SetHostCPU str ->
      ( { model | cpu = (convertToInt str) }
      , Effects.none
      )

    SetHostMemory str ->
      ( { model | memory = (convertToInt str) }
      , Effects.none
      )

    SetHostDiskSpace str ->
      ( { model | disk_space = (convertToInt str) }
      , Effects.none
      )


view : Signal.Address Action -> Model -> Html
view address model =
  div [] [
    h1 [] [text "Create Host"],
    Html.form [class "form-horizontal"] [
      div [class "form-group"] [
        label [class "col-sm-2 control-label"] [text "CPU"],
        div [class "col-sm-10"] [
          div [class "input-group"] [
            input [
              class "form-control",
              id "cpu",
              autofocus True,
              name "cpu",
              value (toString model.cpu),
              on "input" targetValue (\str -> Signal.message address (SetHostCPU str))
            ] [],
            span [class "input-group-addon"] [text " Cores"]
          ]
        ]
      ],
      div [class "form-group"] [
        label [class "col-sm-2 control-label"] [text "Memory"],
        div [class "col-sm-10"] [
          div [class "input-group"] [
            input [
              class "form-control",
              id "memory",
              name "memory",
              value (toString model.memory),
              on "input" targetValue (\str -> Signal.message address (SetHostMemory str))
            ] [],
            span [class "input-group-addon"] [text "MB"]
          ]
        ]
      ],
      div [class "form-group"] [
        label [class "col-sm-2 control-label"] [text "Disk Space"],
        div [class "col-sm-10"] [
          div [class "input-group"] [
            input [
              class "form-control",
              id "disk_space",
              name "disk_space",
              value (toString model.disk_space),
              on "input" targetValue (\str -> Signal.message address (SetHostDiskSpace str))
            ] [],
            span [class "input-group-addon"] [text "MB"]
          ]
        ]
      ],
      div [class "form-group"] [
        div [class "col-sm-offset-2 col-sm-10"] [
          button [
            class "btn btn-default",
            type' "button",
            onClick address SaveHost
          ] [text "Save"]
        ]
      ]
    ]
  ]
