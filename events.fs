namespace Arena

module Events =
  open Core

  let loginEvent state =
    state

  let onLoginEvent state =
    updateMyId (Some "clientId") state
