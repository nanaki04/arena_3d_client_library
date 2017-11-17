namespace Arena

module Locators =
  open Locator
  open EventHandling
  open EventParameterAdapterPlug

  //let locatorPlugs = [eventParameterAdapterPlug]
  let locatorPlugs = []

  let battleLocatorRegistrationList =
    Map.empty
      .Add("login", loginEvent >> runEvent)

  let registrationList =
    Map.empty
      .Add("battle", battleLocatorRegistrationList)

  let locate =
    createLocator registrationList locatorPlugs eventNotFoundHandler
