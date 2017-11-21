namespace Arena

module I =

  type EventParameters = {
    id: Option<string>
    keyCode: Option<int>
    index: Option<int>
    page: Option<int>
    recordsPerPage: Option<int>
    message: Option<string>
  }

  let emptyParameters = {
    id = None;
    keyCode = None;
    index = None;
    page = None;
    recordsPerPage = None;
    message = None;
  }

  // not passing unit fails web gl build
  let Null () = emptyParameters
  let Id identifier = { emptyParameters with id = (Some identifier) }
  let KeyCode keyCode = { emptyParameters with keyCode = (Some keyCode) }
  let Index index = { emptyParameters with index = (Some index) }
  let Page page = { emptyParameters with page = (Some page) }
  let RecordsPerPage recordsPerPage = { emptyParameters with recordsPerPage = (Some recordsPerPage) }
  let Message message = { emptyParameters with message = (Some message) }

  let private combineId (p1, p2) =
    match p2.id with
    | Some identifier -> { p1 with id = (Some identifier) }, p2
    | _ -> p1, p2

  let private combineKeyCode (p1, p2) =
    match p2.keyCode with
    | Some keyCode -> { p1 with keyCode = (Some keyCode) }, p2
    | _ -> p1, p2

  let private combineIndex (p1, p2) =
    match p2.index with
    | Some index -> { p1 with index = (Some index) }, p2
    | _ -> p1, p2

  let private combineRecordsPerPage (p1, p2) =
    match p2.recordsPerPage with
    | Some recordsPerPage -> { p1 with recordsPerPage = (Some recordsPerPage) }, p2
    | _ -> p1, p2

  let private combineMessage (p1, p2) =
    match p2.message with
    | Some message -> { p1 with message = (Some message) }, p2
    | _ -> p1, p2

  let private combineAll =
    combineId
    >> combineKeyCode
    >> combineIndex
    >> combineRecordsPerPage
    >> combineMessage
    >> fst

  let combine (p1, p2) = combineAll (p1, p2)

  let combine3 (p1, p2, p3) =
    combineAll (p2, p3)
    |> fun p23 -> combineAll (p1, p23)
