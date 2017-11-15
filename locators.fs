namespace Arena

module Locators =
  open Locator
  open Events

  let locatorPlugs = []

  let battleLocatorRegistrationList =
    Map.empty
      .Add("login", loginEvent)
      .Add("onLogin", onLoginEvent)

  let registrationList =
    Map.empty
      .Add("battle", battleLocatorRegistrationList)

  let locate =
    createLocator registrationList locatorPlugs
