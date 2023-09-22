module Index

open System
open Elmish
open Fable.Remoting.Client
open Shared

type Model = {
    TodoModel: TodoList.Model
    }

type Msg =
    | TodoListMsg of TodoList.Msg

type Url =
    | TodoList
    | TodoDetail of Guid
    | NotFound

let parseUrl =
    function
    | [ "todo" ] -> Url.TodoList
    | [ "todo";  Feliz.Router.Route.Guid item ] -> Url.TodoDetail item
    | _ -> Url.NotFound

let init () : Model * Cmd<Msg> =
    let todoModel, cmd = TodoList.init ()
    let model = { TodoModel =  todoModel }

    model, cmd |> Cmd.map TodoListMsg

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | TodoListMsg todoMsg ->
        let newTodoModel, msg = TodoList.update todoMsg model.TodoModel
        let newModel = {model with TodoModel = newTodoModel }
        newModel, msg |> Cmd.map TodoListMsg

open Feliz
open Feliz.Bulma

let navBrand =
    Bulma.navbarBrand.div [
        Bulma.navbarItem.a [
            prop.href "https://safe-stack.github.io/"
            navbarItem.isActive
            prop.children [
                Html.img [
                    prop.src "/favicon.png"
                    prop.alt "Logo"
                ]
            ]
        ]
    ]

let view (model: Model) (dispatch: Msg -> unit) =
    Bulma.hero [
        hero.isFullHeight
        color.isPrimary
        prop.style [
            style.backgroundSize "cover"
            style.backgroundImageUrl "https://unsplash.it/1200/900?random"
            style.backgroundPosition "no-repeat center center fixed"
        ]
        prop.children [
            Bulma.heroHead [
                Bulma.navbar [
                    Bulma.container [ navBrand ]
                ]
            ]
            Bulma.heroBody [
                Bulma.container [
                    Bulma.column [
                        column.is6
                        column.isOffset3
                        prop.children [
                            Bulma.title [
                                text.hasTextCentered
                                prop.text "SAFE.App"
                            ]
                            TodoList.view model.TodoModel (TodoListMsg>>dispatch)
                        ]
                    ]
                ]
            ]
        ]
    ]