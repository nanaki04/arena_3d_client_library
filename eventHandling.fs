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
  let debugEvent (_ : I.EventParameters) =
    createEvent DebugEvent

  let handleDebugEvent state =
    pushRenderCommand (Debug state) state

  let loginEvent ({ id = playerId } : I.EventParameters) =
    match playerId with
    | None -> LoginEvent None |> createEvent
    | Some playerId -> LoginEvent (Some playerId) |> createEvent

  let handleLoginEvent state =
    match getProcessingEventType state with
    | LoginEvent (Some playerId) -> updateMyId (Some playerId) state |> pushRenderCommand (Print playerId)
    | _ -> state // make sure this gets send to the server




  let runProcessingEvent state =
    getProcessingEventType state
    |> function
      | (LoginEvent _) -> handleLoginEvent state
      | DebugEvent -> handleDebugEvent state
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
