namespace Arena

module Store =
  open Core

  let private createInitialGameState =
    {
      players = Map.empty;
      projectiles = Map.empty;
      fighters = Map.empty;
      movements = Map.empty;
    }

  let private createInitialState processingEvent =
    {
      me = None;
      processing =  processingEvent;
      eventStore = [], "0";
      eventArchive = [];
      gameState = createInitialGameState
      renderData = [];
    }

  let mutable private state = None

  let loadState processingEvent =
    match state with
    | None -> createInitialState processingEvent
    | Some savedState -> { savedState with processing = processingEvent }

  let saveState stateToSave =
    let renderData = stateToSave.renderData
    state <- Some { stateToSave with renderData = [] }
    renderData
