module Main where

import HostListing
import HostDetail
import Home
import Routes exposing (..)
import ServerApi
import Html exposing (..)
import Html.Attributes exposing (..)
import Task exposing (..)
import Effects exposing (Effects, Never)
import Signal exposing (message)
import StartApp
import TransitRouter exposing (WithRoute, getTransition)
import TransitStyle

type alias Model = WithRoute Routes.Route
  { homeModel : Home.Model
  , hostListingModel : HostListing.Model
  , hostDetailModel : HostDetail.Model
  }

type Action =
    NoOp
  | HomeAction Home.Action
  | HostListingAction HostListing.Action
  | HostDetailAction HostDetail.Action
  | RouterAction (TransitRouter.Action Routes.Route)

initialModel : Model
initialModel =
  { transitRouter = TransitRouter.empty Routes.EmptyRoute
  , homeModel = Home.init
  , hostListingModel = HostListing.init
  , hostDetailModel = HostDetail.init
  }

actions : Signal Action
actions =
  Signal.map RouterAction TransitRouter.actions

mountRoute : Route -> Route -> Model -> (Model, Effects Action)
mountRoute prevRoute route model =
  case route of
    Home ->
      (model, Effects.none)
    HostListingPage ->
      (model, Effects.map HostListingAction (ServerApi.getHosts
      HostListing.HandleHostsRetrieved))
    HostDetailPage hostId ->
      (model, Effects.map HostDetailAction (ServerApi.getHost hostId
      HostDetail.ShowHost))
    NewHostPage ->
      ({ model | hostDetailModel = HostDetail.init } , Effects.none)
    EmptyRoute ->
      (model, Effects.none)

routerConfig : TransitRouter.Config Routes.Route Action Model
routerConfig =
  { mountRoute = mountRoute
  , getDurations = \_ _ _ -> (50, 200)
  , actionWrapper = RouterAction
  , routeDecoder = Routes.decode
  }

init : String -> (Model, Effects Action)
init path =
  TransitRouter.init routerConfig path initialModel

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp ->
      (model, Effects.none)
    HomeAction homeAction ->
      let (model', effects) = Home.update homeAction model.homeModel
      in ( { model | homeModel = model' }
         , Effects.map HomeAction effects )
    HostListingAction act ->
      let (model', effects) = HostListing.update act model.hostListingModel
      in ( { model | hostListingModel = model' }
         , Effects.map HostListingAction effects )
    HostDetailAction act ->
      let (model', effects) = HostDetail.update act model.hostDetailModel
      in ( { model | hostDetailModel = model' }
         , Effects.map HostDetailAction effects )
    RouterAction routeAction ->
      TransitRouter.update routerConfig routeAction model

-- Main view/layout functions

menu : Signal.Address Action -> Model -> Html
menu address model =
  header [class "navbar navbar-default"] [
    div [class "container"] [
        div [class "navbar-header"] [
          div [ class "navbar-brand" ] [
            a (linkAttrs Home) [ text "iWeb" ]
          ]
        ]
      , ul [class "nav navbar-nav"] [
          li [] [a (linkAttrs HostListingPage) [ text "Hosts" ]]
      ]
    ]
  ]

contentView : Signal.Address Action -> Model -> Html
contentView address model =
  case (TransitRouter.getRoute model) of
    Home ->
      Home.view (Signal.forwardTo address HomeAction) model.homeModel
    HostListingPage ->
      HostListing.view (Signal.forwardTo address HostListingAction) model.hostListingModel
    HostDetailPage i ->
      HostDetail.view (Signal.forwardTo address HostDetailAction) model.hostDetailModel
    NewHostPage  ->
      HostDetail.view (Signal.forwardTo address HostDetailAction) model.hostDetailModel
    EmptyRoute ->
      text "No route specified"

view : Signal.Address Action -> Model -> Html
view address model =
  div [class "container-fluid"] [
      menu address model
    , div [ class "content"
          , style (TransitStyle.fadeSlideLeft 100 (getTransition model))]
          [contentView address model]
  ]

-- wiring up start app

app : StartApp.App Model
app =
  StartApp.start
    { init = init initialPath
    , update = update
    , view = view
    , inputs = [actions]
    }

main : Signal Html
main =
  app.html

port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks

port initialPath : String
