namespace Arena

module Mock =
  open Env
  open Core
  open LocatorTypes

  let private loginMock ({ id = playerId } : I.EventParameters) =
    match playerId with
    | Some playerId -> [Print playerId]
    | None -> [Print "player id"]

  let private mockBattleLocatorRegistrationList : EventHandlerList =
    Map.empty
      .Add("login", loginMock)

  let private mockRegistrationList : RegistrationList =
    Map.empty
      .Add("battle", mockBattleLocatorRegistrationList)

  let getMockEvent domain address =
    Map.tryFind domain mockRegistrationList |> function
    | Some eventList -> Map.tryFind address eventList
    | _ -> None

  let setEventToRegistrationList domain address registrationList evnt =
    Map.tryFind domain registrationList |> function
      | Some eventList -> eventList
      | None -> Map.empty
    |> Map.add address evnt
    |> Map.add domain
    <| registrationList

  let eventMockPlug (domain, address, (registrationList : RegistrationList)) =
    match Env.EventMock with
    | false -> (domain, address, registrationList)
    | _ ->
      getMockEvent domain address |> function
        | Some evnt ->
          (domain, address, (setEventToRegistrationList domain address registrationList evnt))
        | _ -> (domain, address, registrationList)

