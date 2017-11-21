namespace Arena

module Locators =
  open LocatorTypes
  open EventHandling
  open Mock

  let private locatorPlugs = [
    eventMockPlug;
  ]

  let private debugLocatorRegistrationList : EventHandlerList =
    Map.empty
      .Add("state", debugEvent >> runEvent)

  let private battleLocatorRegistrationList : EventHandlerList =
    Map.empty
      .Add("login", loginEvent >> runEvent)

  let private registrationList : RegistrationList =
    Map.empty
      .Add("debug", debugLocatorRegistrationList)
      .Add("battle", battleLocatorRegistrationList)

  let applyPlugs (domain, address, registrationList) =
    List.fold (fun result plug -> plug result) (domain, address, registrationList) locatorPlugs

  let locate domain address eventParameters =
    let (domain, address, registrationList) = applyPlugs (domain, address, registrationList)
    let eventHandler = Map.tryFind domain registrationList |> function
      | Some eventList ->
        Map.tryFind address eventList |> function
          | Some evnt -> evnt
          | None -> eventNotFoundHandler
      | None -> eventNotFoundHandler
    eventHandler eventParameters
