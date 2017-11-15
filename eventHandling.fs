namespace Arena

module EventHandling =
  open Core

  let generateEventId =
    "0", "0", 0, None

  let orderEventStore =
    updateEventStoreList (fun eventStoreList -> List.sortBy getEventSpawnTime eventStoreList)
