module Counter

open Feliz.Bulma
open Elmish
open Feliz
open Feliz.UseElmish

type Model = { Count: int }

type Msg =
    | Increment
    | Decrement
    | Reset

let init () = { Count = 0 }, Cmd.none

let update (msg: Msg) (model: Model)  =
    match msg with
    | Increment -> { model with Count = model.Count + 1 }, Cmd.none
    | Decrement -> { model with Count = model.Count - 1 }, Cmd.none
    | Reset -> { model with Count = 0 }, Cmd.none


[<ReactComponent>]
let View () =
    let model, dispatch = React.useElmish(init, update, [||])
    Bulma.box [
        Bulma.content [
            prop.style [ style.textAlign.center ]
            prop.text model.Count
        ]
        Bulma.columns [
            Bulma.column [
                Bulma.button.a [
                    color.isPrimary
                    prop.onClick (fun _ -> dispatch Increment)
                    prop.text "Increment"
                ]
            ]
            Bulma.column [
                Bulma.button.a [
                    color.isDanger
                    prop.onClick (fun _ -> dispatch Decrement)
                    prop.text "Decrement"
                ]
            ]
            Bulma.column [
                Bulma.button.a [
                    color.isInfo
                    prop.onClick (fun _ -> dispatch Reset)
                    prop.text "Reset"
                ]
            ]
        ]
    ]