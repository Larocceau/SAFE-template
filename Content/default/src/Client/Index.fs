module Index

open Elmish
open Feliz.Router

[<RequireQualifiedAccess>]
type Url =
    | TodoList
    | Counter
    | NotFound

type Msg =
    | UrlChanged of Url

type Page =
    | TodoList
    | Counter
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
        let model = {CurrentPage = TodoList; CurrentUrl = url }
        model, Cmd.none
    | Url.Counter ->
        let model = {CurrentPage = Counter ; CurrentUrl = url }
        model, Cmd.none
    | Url.NotFound ->
        let model = { CurrentPage = NotFound; CurrentUrl = url }
        model, Cmd.none

let init () : Model * Cmd<Msg> =
    (Router.currentPath())
    |> parseUrl
    |> initFromUrl

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | UrlChanged url ->
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
                                    match model.CurrentPage with
                                    | TodoList -> Counter.View()
                                    | Counter -> Counter.View ()
                                    | NotFound -> Bulma.box "page not found"
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]