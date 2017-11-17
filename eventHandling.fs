namespace Arena

module EventHandling =
  open Core
  open System
  open Store

  let mutable private lastId = 0

  let generateSpawnTime () =
    let time = DateTime.UtcNow
    let epoch = DateTime (1970, 1, 1, 0, 0, 0, DateTimeKind.Utc)
    let diff = time - epoch
    Math.Round (diff.TotalMilliseconds, 0)

  let generateEventId () =
    lastId <- lastId + 1
    let id = string lastId
    id, None, generateSpawnTime (), None

  let createEvent eventType =
    (generateEventId (), eventType)




  // EVENTS

  // LoginEvent has id as optional, so no matching is needed here
  let loginEvent ({ id = playerId } : EventParameters) =
    LoginEvent playerId |> createEvent
//    match eventParameters with
//    | { id = Some playerId } -> createEvent LoginEvent playerId
//    | _ -> createEvent EventParameterErrorEvent "loginEvent"

  let handleLoginEvent state =
    match getProcessingEventType state with
    | LoginEvent (Some playerId) -> updateMyId (Some playerId) state
    | _ -> state // make sure this gets send to the server




  let runProcessingEvent state =
    getProcessingEventType state
    |> function
      | (LoginEvent _) -> handleLoginEvent state
      | _ -> state

  let storeEvent evnt =
    loadState evnt
    |> pushProcessingEventToStore
    |> saveState

  let runEvent evnt =
    loadState evnt
    |> runProcessingEvent
    |> saveState

  let eventNotFoundHandler _ =
    [ReportError "event not found"]

  let orderEventStore =
    updateEventStoreList (fun eventStoreList -> List.sortBy getEventSpawnTime eventStoreList)
