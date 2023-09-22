module Index

open Elmish
open Feliz.Router

[<RequireQualifiedAccess>]
type Url =
    | TodoList
    | Counter
    | NotFound

type Msg =
    | TodoListMsg of TodoList.Msg
    | CounterMsg of Counter.Msg
    | UrlChanged of Url

type Page =
    | TodoList of TodoList.Model
    | Counter of Counter.Model
    | NotFound
type Model = {
    CurrentPage: Page
    CurrentUrl: Url
    }

let parseUrl =
    function
    | [ "todo" ] -> Url.TodoList
    | [ "counter" ] -> Url.Counter
    | _ -> Url.NotFound


let initFromUrl url =
    match url with
    | Url.TodoList ->
        let todoModel, command = TodoList.init ()
        let model = {CurrentPage = TodoList todoModel; CurrentUrl = url }
        model, command |> Cmd.map TodoListMsg
    | Url.Counter ->
        let counterModel, command = Counter.init ()
        let model = {CurrentPage = Counter counterModel; CurrentUrl = url }
        model, command |> Cmd.map CounterMsg
    | Url.NotFound ->
        let model = { CurrentPage = NotFound; CurrentUrl = url }
        model, Cmd.none

let init () : Model * Cmd<Msg> =
    (Router.currentPath())
    |> parseUrl
    |> initFromUrl

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match model.CurrentPage, msg with
    | TodoList todo, TodoListMsg todoMsg ->
        let newTodoModel, msg = TodoList.update todoMsg todo
        let newModel = {model with CurrentPage = TodoList newTodoModel }
        newModel, msg |> Cmd.map TodoListMsg
    | Counter counter, CounterMsg counterMsg ->
        let newCounterModel, msg = Counter.update counterMsg counter
        let newModel = {model with CurrentPage = Counter newCounterModel }
        newModel, msg |> Cmd.map CounterMsg
    | _, UrlChanged url ->
        initFromUrl url

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
let content page dispatch =
    match page with
    | TodoList todoModel -> TodoList.view todoModel (TodoListMsg>>dispatch)
    | Counter counterModel -> Counter.View counterModel (CounterMsg>>dispatch)
    | NotFound ->
        Bulma.box "page not found"


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
                                    content model.CurrentPage dispatch
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]