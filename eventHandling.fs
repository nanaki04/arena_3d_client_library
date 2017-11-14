namespace Arena

module EventHandling =
  open Core

  let orderEventStore =
    updateEventStoreList (fun eventStoreList -> List.sortBy getEventSpawnTime eventStoreList)
