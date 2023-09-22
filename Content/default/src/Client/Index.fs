module Index

open Elmish
open Shared
open Feliz.Router
open Fable.Remoting.Client


type Url =
    | TodoList
    | Counter
    | NotFound
type Model = {
    CurrentUrl: Url
    Count: int
    Todos: Todo List
    Input: string
    }
let TodoApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<ITodosApi>

type Msg =
    | Increment
    | Decrement
    | Reset
    | GotTodos of Todo list
    | SetInput of string
    | AddTodo
    | AddedTodo of Todo
    | UrlChanged of Url

let parseUrl =
    function
    | [ "todo" ] -> Url.TodoList
    | [ "counter" ] -> Url.Counter
    | _ -> Url.NotFound

let init () : Model * Cmd<Msg>  =
    let url = Router.currentPath() |> parseUrl
    let command = Cmd.OfAsync.perform TodoApi.getTodos () GotTodos
    {Count=0; CurrentUrl = url; Input = ""; Todos = [] },  command


let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | Increment -> { model with Count = model.Count + 1 }, Cmd.none
    | Decrement -> { model with Count = model.Count - 1 }, Cmd.none
    | Reset -> { model with Count = 0 }, Cmd.none
    | GotTodos todos -> { model with Todos = todos }, Cmd.none
    | SetInput value -> { model with Input = value }, Cmd.none
    | AddTodo ->
        let todo = Todo.create model.Input
        let cmd = Cmd.OfAsync.perform TodoApi.addTodo todo AddedTodo

        { model with Input = "" }, cmd
    | AddedTodo todo -> { model with Todos = model.Todos @ [ todo ] }, Cmd.none
    | UrlChanged url ->
        {model with CurrentUrl = url}, Cmd.none

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

let TodoView (model: Model) (dispatch: Msg -> unit) =
    Bulma.box [
        Bulma.content [
            Html.ol [
                for todo in model.Todos do
                    Html.li [ prop.text todo.Description ]
            ]
        ]
        Bulma.field.div [
            field.isGrouped
            prop.children [
                Bulma.control.p [
                    control.isExpanded
                    prop.children [
                        Bulma.input.text [
                            prop.value model.Input
                            prop.placeholder "What needs to be done?"
                            prop.onChange (fun x -> SetInput x |> dispatch)
                        ]
                    ]
                ]
                Bulma.control.p [
                    Bulma.button.a [
                        color.isPrimary
                        prop.disabled (Todo.isValid model.Input |> not)
                        prop.onClick (fun _ -> dispatch AddTodo)
                        prop.text "Add"
                    ]
                ]
            ]
        ]
    ]

let CounterView (model: Model) (dispatch: Msg -> unit) =
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


let view (model: Model) (dispatch: Msg -> unit) =
    React.router[
        router.onUrlChanged (parseUrl>>UrlChanged>>dispatch)
        router.children[
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
                                    match model.CurrentUrl with
                                    | Counter -> CounterView model dispatch
                                    | TodoList -> TodoView model dispatch
                                    | NotFound -> Bulma.box "Page Not Found"
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]