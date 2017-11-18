namespace Arena

module Locators =
  open Locator
  open EventHandling
  open EventParameterAdapter

  let private locatorPlugs = []

  let private battleLocatorRegistrationList =
    Map.empty
      .Add("login", loginEvent >> runEvent)

  let private registrationList =
    Map.empty
      .Add("battle", battleLocatorRegistrationList)

  let private locateEvent =
    createLocator registrationList locatorPlugs eventNotFoundHandler

  let locate domain address =
    let event = locateEvent domain address
    fun eventParameters -> convertEventParameters eventParameters |> event