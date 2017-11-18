namespace Arena

module EventParameterAdapter =
  open Core

  type ParametersIn = {
    id: string
    index: int
    code: int
  }

  let private convertId parametersIn parametersOut =
    match parametersIn with
    | { ParametersIn.id = id } when box id <> null -> { (parametersOut: EventParameters) with id = Some id }
    | _ -> { (parametersOut: EventParameters) with id = None }

  let private convertIndex parametersIn parametersOut =
    match parametersIn with
    | { ParametersIn.index = index } when box index <> null -> { (parametersOut: EventParameters) with index = Some index }
    | _ -> { (parametersOut: EventParameters) with index = None }

  let private convertCode parametersIn parametersOut =
    match parametersIn with
    | { ParametersIn.code = code } when box code <> null -> { (parametersOut: EventParameters) with code = Some code }
    | _ -> { (parametersOut: EventParameters) with code = None }

  let private emptyEventParameters : EventParameters = {
    id = None;
    index = None;
    code = None;
  }

  let convertEventParameters parametersIn =
    emptyEventParameters
    |> convertId parametersIn
    |> convertIndex parametersIn
    |> convertCode parametersIn
    
