namespace Arena

module Core =
  // Generics
  type Id = string

  let maybe handle arg =
    match arg with
    | Some arg -> Some <| handle arg
    | _ -> None

  let maybeApply handle arg arg2 =
    maybe handle arg |> function
      | Some handler -> handler arg2
      | None -> arg2

  let push elem lst = elem :: lst

  // Rendering
  type Vector2 = float * float
  type Vector3 = float * float * float
  type Vector4 = float * float * float * float

  type Transform =
    | Position of Vector3
    | Rotation of Vector3
    | Scale of Vector3

  type RenderCommand<'T> =
    | TransformCommand of Transform
    | Animate
    | ChangeScene
    | Reload
    | Destroy
    | Spawn of Id
    | Print of string
    | ReportError of string
    | Debug of 'T

  type RenderData<'T> = RenderCommand<'T> list

  // State
  type Me = Option<Id>

  type Player = {
    id: Id
  }

  type Players = Map<Id, Player>

  type Projectile = {
    id: Id
  }

  type Projectiles = Map<Id, Projectile>

  type Fighter = {
    id: Id
  }

  type Fighters = Map<Id, Fighter>

  type Movement = {
    id: Id
  }

  type Movements = Map<Id, Movement>

  type GameState = {
    players: Players
    projectiles: Projectiles
    fighters: Fighters
    movements: Movements
  }

  // Events
  type EventSpawner = Id
  type ProgressId = Id
  type SpawnTime = float
  type EventId = Id * Option<EventSpawner> * SpawnTime * Option<ProgressId>

  type KeyCode = int
  type DeltaTime = float

  type EventType =
    | ButtonDownEvent of KeyCode
    | ButtonUpEvent of KeyCode
    | TickEvent of DeltaTime
    | LeftMouseDownEvent
    | RightMouseDownEvent
    | LeftMouseUpEvent
    | RightMouseUpEvent
    | MovementEvent of Vector4
    | OrientationEvent of Vector4
    | LoginEvent of Option<Id>
    | EventParameterErrorEvent of string
    | DebugEvent

  type Event = EventId * EventType

  type EventArchiveEntry = {
    archive: Event list
    archivedUntil: int
    snapshot: GameState
  }

  type EventArchive = EventArchiveEntry list

  type State = {
    me: Me
    processing: Event
    eventStore: Event list * ProgressId
    eventArchive: EventArchive
    gameState: GameState
    renderData: RenderData<State>
  }

  type StateRecord =
    | MainStateRecord of State
    | GameStateRecord of GameState
    | PlayerRecord of Player
    | FighterRecord of Fighter
    | ProjectileRecord of Projectile
    | MovementRecord of Movement

  // Mutations
  type Transformer<'T> = 'T -> 'T
  type Combiner<'T> = 'T -> 'T -> 'T
  
  type ValueUpdater<'T> =
    | Value of 'T
    | ValueUpdateTransformer of Transformer<'T>

  type Updater<'T> = ValueUpdater<'T> -> StateRecord -> StateRecord
  type StateUpdater<'T> = Updater<'T> -> State -> State

  let getMyId state = state.me
  let getProcessingEvent state = state.processing
  let getEventId (id, _) = id
  let getEventIdNumber ((id, _, _, _), _) = id
  let getEventSpawner ((_, spawner, _, _), _) = spawner
  let getEventSpawnTime ((_, _, spawnTime, _), _) = spawnTime
  let getEventProgressId ((_, _, _, progressId), _) = progressId
  let getEventType (_, eventType) = eventType
  let getProcessingEventId = getProcessingEvent >> getEventId
  let getProcessingEventType = getProcessingEvent >> getEventType
  let getProcessingEventIdNumber = getProcessingEvent >> getEventIdNumber
  let getProcessingEventSpawner = getProcessingEvent >> getEventSpawner
  let getProcessingEventSpawnTime = getProcessingEvent >> getEventSpawnTime
  let getProcessingEventProgressId = getProcessingEvent >> getEventProgressId

  let getEventStore state = state.eventStore
  let getGameState state = state.gameState

  let getPlayersFromGameState gameState = gameState.players
  let getPlayers = getGameState >> getPlayersFromGameState
  let getPlayerFromPlayers id players = Map.tryFind id players
  let getPlayer id = getPlayers >> getPlayerFromPlayers id
  let getPlayerIdFromPlayer (player : Player) = player.id
  let getSelectedPlayer = getPlayer "selected"
  let getSelectedPlayerId = getSelectedPlayer >> maybe getPlayerIdFromPlayer
  let getMe = getMyId >> maybe getPlayer

  let getProjectilesFromGameState gameState = gameState.projectiles
  let getProjectiles = getGameState >> getProjectilesFromGameState
  let getProjectileFromProjectiles id projectiles = Map.tryFind id projectiles
  let getProjectile id = getProjectiles >> getProjectileFromProjectiles id
  let getProjectileIdFromProjectile (projectile : Projectile) = projectile.id
  let getSelectedProjectile = getProjectile "selected"
  let getSelectedProjectileId = getSelectedProjectile >> maybe getProjectileIdFromProjectile

  let getFightersFromGameState gameState = gameState.fighters
  let getFighters = getGameState >> getFightersFromGameState
  let getFighterFromFighters id fighters = Map.tryFind id fighters
  let getFighter id = getFighters >> getFighterFromFighters id
  let getFighterIdFromFighter (fighter : Fighter) = fighter.id
  let getSelectedFighter = getFighter "selected"
  let getSelectedFighterId = getSelectedFighter >> maybe getFighterIdFromFighter

  let getMovementsFromGameState gameState = gameState.movements
  let getMovements = getGameState >> getMovementsFromGameState
  let getMovementFromMovements id movements = Map.tryFind id movements
  let getMovement id = getMovements >> getMovementFromMovements id
  let getMovementIdFromMovement (movement : Movement) = movement.id
  let getSelectedMovement = getMovement "selected"
  let getSelectedMovementId = getSelectedMovement >> maybe getMovementIdFromMovement

  let updateProcessingEvent evnt state =
    { state with processing = evnt }
  let updateEventStore updater state =
    { state with eventStore = updater state.eventStore }
  let updateEventStoreList updater =
    (fun (eventStore, progressId) -> (updater eventStore, progressId)) |> updateEventStore
  let updateEventStoreProgressId progressId =
    (fun (eventStore, _) -> (eventStore, progressId)) |> updateEventStore
  let pushEvent evnt = push evnt |> updateEventStoreList
  let pushProcessingEventToStore state =
    getProcessingEvent state |> pushEvent <| state

  let updateGameState updater state =
    { state with gameState = updater state.gameState }

  let updateMyId myId state =
    { state with me = myId }

  let updatePlayersOnGameState updater gameState =
    { gameState with players = updater gameState.players }
  let updatePlayers updater = updatePlayersOnGameState updater |> updateGameState
  let updatePlayerOnPlayers id updater players =
    updater >> Map.add id |> maybeApply <| getPlayerFromPlayers id players <| players
  let updatePlayer id updater =
    updatePlayerOnPlayers id updater
    |> updatePlayers
  let updateSelectedPlayer = updatePlayer "selected"
  let selectPlayer id state =
    Map.add "selected" >> updatePlayers |> maybeApply <| getPlayer id state <| state
  let updateMe updater state =
    getMyId state |> function
      | Some id -> updatePlayer id updater state
      | None -> state

  let updateProjectilesFromGameState updater gameState =
    { gameState with projectiles = updater gameState.projectiles }
  let updateProjectiles updater = updateProjectilesFromGameState updater |> updateGameState
  let updateProjectileOnProjectiles id updater projectiles =
    updater >> Map.add id |> maybeApply <| getProjectileFromProjectiles id projectiles <| projectiles
  let updateProjectile id updater = updateProjectileOnProjectiles id updater |> updateProjectiles
  let updateSelectedProjectile = updateProjectile "selected"
  let selectProjectile id state =
    Map.add "selected" >> updateProjectiles |> maybeApply <| getProjectile id state <| state

  let updateFightersFromGameState updater gameState =
    { gameState with fighters = updater gameState.fighters }
  let updateFighters updater = updateFightersFromGameState updater |> updateGameState
  let updateFighterOnFighters id updater fighters =
    updater >> Map.add id |> maybeApply <| getFighterFromFighters id fighters <| fighters
  let updateFighter id updater = updateFighterOnFighters id updater |> updateFighters
  let updateSelectedFighter = updateFighter "selected"
  let selectFighter id state =
    Map.add "selected" >> updateFighters |> maybeApply <| getFighter id state <| state

  let updateMovementsFromGameState updater gameState =
    { gameState with movements = updater gameState.movements }
  let updateMovements updater = updateMovementsFromGameState updater |> updateGameState
  let updateMovementOnMovements id updater movements =
    updater >> Map.add id |> maybeApply <| getMovementFromMovements id movements <| movements
  let updateMovement id updater = updateMovementOnMovements id updater |> updateMovements
  let updateSelectedMovement = updateMovement "selected"
  let selectMovement id state =
    Map.add "selected" >> updateMovements |> maybeApply <| getMovement id state <| state

  let pushRenderCommand renderCommand state =
    { state with renderData = push renderCommand state.renderData }
