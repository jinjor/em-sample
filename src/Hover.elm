effect module Hover where { subscription = MySub } exposing (..)


import Dict
import Dom.LowLevel as Dom
import Json.Decode as Json
import Process
import Task exposing (Task)


-- DECODER


targetId : Json.Decoder String
targetId =
  Json.at ["target", "id"] Json.string



-- MOUSE EVENTS


hover : String -> (Bool -> msg) -> Sub msg
hover id tagger =
  subscription (MySub id tagger)



-- SUBSCRIPTIONS


type MySub msg
  = MySub String (Bool -> msg)


subMap : (a -> b) -> MySub a -> MySub b
subMap func (MySub id tagger) =
  MySub id (tagger >> func)



-- EFFECT MANAGER STATE


type alias State msg =
  { pid : Maybe (Process.Id, Process.Id)
  , watchers : SubDict msg
  }


type alias SubDict msg =
  Dict.Dict String (List (Bool -> msg))


toSubDict : List (MySub msg) -> SubDict msg
toSubDict subs =
  toSubDictHelp subs Dict.empty


toSubDictHelp : List (MySub msg) -> SubDict msg -> SubDict msg
toSubDictHelp subs subDict =
  case subs of
    [] ->
      subDict

    MySub id tagger :: rest ->
      toSubDictHelp rest <|
        Dict.update id (toSubDictHelpHelp tagger) subDict


toSubDictHelpHelp : a -> Maybe (List a) -> Maybe (List a)
toSubDictHelpHelp value maybeValues =
  case maybeValues of
    Nothing ->
      Just [value]

    Just values ->
      Just (value :: values)



-- EFFECT MANAGER


init : Task Never (State msg)
init =
  Task.succeed { pid = Nothing, watchers = Dict.empty }


type alias Msg =
  (Bool, String)


(&>) task1 task2 =
  Task.andThen (\_ -> task2) task1


onEffects : Platform.Router msg Msg -> List (MySub msg) -> State msg -> Task Never (State msg)
onEffects router newSubs oldState =
  let
    spawn task =
      task
        |> Task.andThen (\state -> Process.spawn (Dom.onWindow "mouseover" targetId (Platform.sendToSelf router << (,) True))
        |> Task.andThen (\enterPid -> Process.spawn (Dom.onWindow "mouseout" targetId (Platform.sendToSelf router << (,) False))
        |> Task.map (\leavePid -> { state | pid = Just (enterPid, leavePid) } )))

    kill pid task =
      Process.kill pid &> task

    handleProcess =
      case (oldState.pid, List.isEmpty newSubs) of
        (Nothing, False) ->
          spawn

        (Just (enterPid, leavePid), True) ->
          kill enterPid >> kill leavePid

        _ ->
          identity

    updateWatchers state =
      { state | watchers = toSubDict newSubs }
  in
    Task.succeed oldState
      |> Task.map updateWatchers
      |> handleProcess


onSelfMsg : Platform.Router msg Msg -> Msg -> State msg -> Task Never (State msg)
onSelfMsg router (hover, id) state =
  case Dict.get id state.watchers of
    Nothing ->
      Task.succeed state

    Just taggers ->
      let
        send tagger =
          Platform.sendToApp router (tagger hover)
      in
        Task.sequence (List.map send taggers)
          |> Task.andThen (\_ -> Task.succeed state)
