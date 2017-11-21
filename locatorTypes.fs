namespace Arena

module LocatorTypes =
  open Core

  type EventHandler = I.EventParameters -> Core.RenderData<Core.State>
  type EventHandlerList = Map<string, EventHandler>
  type RegistrationList = Map<string, EventHandlerList>
