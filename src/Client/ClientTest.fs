module ClientTest

open Elmish
open Elmish.React
open Fable.FontAwesome
open Fable.FontAwesome.Free
open Fable.React
open Fable.React.Props
open Fetch.Types
open Thoth.Fetch
open Fulma
open Thoth.Json

open Shared

// The model holds data that you want to keep track of while the application is running
// in this case, we are keeping track of a counter
// we mark it as optional, because initially it will not be available from the client
// the initial value will be requested from server
type Model = { Counter: Counter option }

// The Msg type defines what events/actions can occur while the application is running
// the state of the application changes *only* in reaction to these events
type Msg =
| Increment
| Decrement
| InitialCountLoaded of Counter



let initialCounter () = Fetch.fetchAs<Counter> "/api/init"

// defines the initial state and initial command (= side-effect) of the application
let init () : Model * Cmd<Msg> =
    let initialModel = { Counter = None }
    let loadCountCmd =
        Cmd.OfPromise.perform initialCounter () InitialCountLoaded
    initialModel, loadCountCmd



// The update function computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// these commands in turn, can dispatch messages to which the update function will react.
let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    match currentModel.Counter, msg with
    | Some counter, Increment ->
        let nextModel = { currentModel with Counter = Some { Value = counter.Value + 1 } }
        nextModel, Cmd.none
    | Some counter, Decrement ->
        let nextModel = { currentModel with Counter = Some { Value = counter.Value - 1 } }
        nextModel, Cmd.none
    | _, InitialCountLoaded initialCount->
        let nextModel = { Counter = Some initialCount }
        nextModel, Cmd.none

    | _ -> currentModel, Cmd.none


let safeComponents =
    let components =
        span [ ]
           [
             a [ Href "https://saturnframework.github.io" ] [ str "Saturn" ]
             str ", "
             a [ Href "http://fable.io" ] [ str "Fable" ]
             str ", "
             a [ Href "https://elmish.github.io/elmish/" ] [ str "Elmish" ]
             str ", "
             a [ Href "https://fulma.github.io/Fulma" ] [ str "Fulma" ]
             str ", "
             a [ Href "https://dansup.github.io/bulma-templates/" ] [ str "Bulma\u00A0Templates" ]
           ]

    span [ ]
        [ strong [] [ str "SAFE Template" ]
          str " powered by: "
          components ]

let show = function
| { Counter = Some counter } -> string counter.Value
| { Counter = None   } -> "Loading..."

let navBrand =
    Navbar.Brand.div [ ]
        [ Navbar.Item.a
            [ Navbar.Item.Props [ Href "https://safe-stack.github.io/" ] ]
            [ img [ Src "https://safe-stack.github.io/images/safe_top.png"
                    Alt "Logo" ] ] ]

let navMenu =
    Navbar.menu [ ]
        [ Navbar.End.div [ ]
            [ Navbar.Item.a [ ]
                [ str "Home" ]
              Navbar.Item.a [ ]
                [ str "Examples" ]
              Navbar.Item.a [ ]
                [ str "Documentation" ]
              Navbar.Item.div [ ]
                [ Button.a
                    [ Button.Color IsWhite
                      Button.IsOutlined
                      Button.Size IsSmall
                      Button.Props [ Href "https://github.com/SAFE-Stack/SAFE-template" ] ]
                    [ Icon.icon [ ]
                        [ Fa.i [Fa.Brand.Github; Fa.FixedWidth] [] ]
                      span [ ] [ str "View Source" ] ] ] ] ]

let buttonBox (model : Model) (dispatch : Msg -> unit) =
    Box.box' [ CustomClass "cta" ]
        [ Level.level [ ]
            [ Level.item [ ]
                [ Button.a
                    [ Button.Color IsPrimary
                      Button.OnClick (fun _ -> dispatch Increment) ]
                    [ str "+" ] ]

              Level.item [ ]
                [ p [ ] [ str (show model) ] ]

              Level.item [ ]
                [ Button.a
                    [ Button.Color IsPrimary
                      Button.OnClick (fun _ -> dispatch Decrement) ]
                    [ str "-" ] ] ] ]

let card icon heading body =
  Column.column [ Column.Width (Screen.All, Column.Is4) ]
    [ Card.card [ ]
        [ Card.image
            [ Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
            [ Icon.icon [ Icon.Size IsMedium
                          Icon.Props [ Style [ MarginTop "15px" ] ] ]
                [ Fa.i [icon; Fa.IconOption.Size Fa.Fa2x] [] ] ]
          Card.content [ ]
            [ Content.content [ ]
                [ h4 [ ] [ str heading ]
                  p [ ] [ str body ]
                  p [ ]
                    [ a [ Href "#" ]
                        [ str "Learn more" ] ] ] ] ] ]

let features =
    Columns.columns [ Columns.CustomClass "features" ]
        [ card Fa.Solid.Paw "Tristique senectus et netus et." "Purus semper eget duis at tellus at urna condimentum mattis. Non blandit massa enim nec. Integer enim neque volutpat ac tincidunt vitae semper quis. Accumsan tortor posuere ac ut consequat semper viverra nam."
          card Fa.Regular.IdCard "Tempor orci dapibus ultrices in." "Ut venenatis tellus in metus vulputate. Amet consectetur adipiscing elit pellentesque. Sed arcu non odio euismod lacinia at quis risus. Faucibus turpis in eu mi bibendum neque egestas cmonsu songue. Phasellus vestibulum lorem sed risus."
          card Fa.Solid.Rocket "Leo integer malesuada nunc vel risus." "Imperdiet dui accumsan sit amet nulla facilisi morbi. Fusce ut placerat orci nulla pellentesque dignissim enim. Libero id faucibus nisl tincidunt eget nullam. Commodo viverra maecenas accumsan lacus vel facilisis." ]

let intro =
    Column.column
        [ Column.CustomClass "intro"
          Column.Width (Screen.All, Column.Is8)
          Column.Offset (Screen.All, Column.Is2) ]
        [ h2 [ ClassName "title" ] [ str "Perfect for developers or designers!" ]
          br [ ]
          p [ ClassName "subtitle"] [ str "Vel fringilla est ullamcorper eget nulla facilisi. Nulla facilisi nullam vehicula ipsum a. Neque egestas congue quisque egestas diam in arcu cursus." ] ]

let tile title subtitle content =
    let details =
        match content with
        | Some c -> c
        | None -> nothing

    Tile.child [ ]
        [ Notification.notification [ Notification.Color IsWhite ]
            [ Heading.p [ ] [ str title ]
              Heading.p [ Heading.IsSubtitle ] [ str subtitle ]
              details ] ]

let content txts =
    Content.content [ ]
        [ for txt in txts -> p [ ] [ str txt ] ]

module Tiles =
    let hello = tile "Hello World" "What is up?" None

    let foo = tile "Foo" "Bar" None

    let third =
        tile
          "Third column"
          "With some content"
          (Some (content ["Lorem ipsum dolor sit amet, consectetur adipiscing elit. Proin ornare magna eros, eu pellentesque tortor vestibulum ut. Maecenas non massa sem. Etiam finibus odio quis feugiat facilisis."]))

    let verticalTop = tile "Vertical tiles" "Top box" None

    let verticalBottom = tile "Vertical tiles" "Bottom box" None

    let middle =
        tile
            "Middle box"
            "With an image"
            (Some (Image.image [ Image.Is4by3 ] [ img [ Src "http://bulma.io/images/placeholders/640x480.png"] ]))

    let wide =
        tile
            "Wide column"
            "Aligned with the right column"
            (Some (content ["Lorem ipsum dolor sit amet, consectetur adipiscing elit. Proin ornare magna eros, eu pellentesque tortor vestibulum ut. Maecenas non massa sem. Etiam finibus odio quis feugiat facilisis."]))

    let tall =
        tile
            "Tall column"
            "With even more content"
            (Some (content
                    ["Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam semper diam at erat pulvinar, at pulvinar felis blandit. Vestibulum volutpat tellus diam, consequat gravida libero rhoncus ut. Morbi maximus, leo sit amet vehicula eleifend, nunc dui porta orci, quis semper odio felis ut quam."
                     "Suspendisse varius ligula in molestie lacinia. Maecenas varius eget ligula a sagittis. Pellentesque interdum, nisl nec interdum maximus, augue diam porttitor lorem, et sollicitudin felis neque sit amet erat. Maecenas imperdiet felis nisi, fringilla luctus felis hendrerit sit amet. Aenean vitae gravida diam, finibus dignissim turpis. Sed eget varius ligula, at volutpat tortor."
                     "Integer sollicitudin, tortor a mattis commodo, velit urna rhoncus erat, vitae congue lectus dolor consequat libero. Donec leo ligula, maximus et pellentesque sed, gravida a metus. Cras ullamcorper a nunc ac porta. Aliquam ut aliquet lacus, quis faucibus libero. Quisque non semper leo."]))

    let side =
        tile
            "Side column"
            "With some content"
            (Some (content ["Lorem ipsum dolor sit amet, consectetur adipiscing elit. Proin ornare magna eros, eu pellentesque tortor vestibulum ut. Maecenas non massa sem. Etiam finibus odio quis feugiat facilisis."]))

    let main =
        tile
            "Main column"
            "With some content"
            (Some (content ["Lorem ipsum dolor sit amet, consectetur adipiscing elit. Proin ornare magna eros, eu pellentesque tortor vestibulum ut. Maecenas non massa sem. Etiam finibus odio quis feugiat facilisis."]))


let sandbox =
    div [ ClassName "sandbox" ]
        [ Tile.ancestor [ ]
            [ Tile.parent [ ]
                [ Tiles.hello ]
              Tile.parent [ ]
                [ Tiles.foo ]
              Tile.parent [  ]
                [ Tiles.third ] ]
          Tile.ancestor [ ]
            [ Tile.tile [ Tile.IsVertical; Tile.Size Tile.Is8 ]
                [ Tile.tile [ ]
                    [ Tile.parent [ Tile.IsVertical ]
                        [ Tiles.verticalTop
                          Tiles.verticalBottom ]
                      Tile.parent [ ]
                        [ Tiles.middle ] ]
                  Tile.parent [ ]
                    [ Tiles.wide ] ]
              Tile.parent [ ]
                [ Tiles.tall ] ]
          Tile.ancestor [ ]
            [ Tile.parent [ ]
                [ Tiles.side ]
              Tile.parent [ Tile.Size Tile.Is8 ]
                [ Tiles.main ] ] ]

let footerContainer =
    Container.container [ ]
        [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
            [ p [ ]
                [ safeComponents ]
              p [ ]
                [ a [ Href "https://github.com/SAFE-Stack/SAFE-template" ]
                    [ Icon.icon [ ]
                        [ Fa.i [Fa.Brand.Github; Fa.FixedWidth] [] ] ] ] ] ]

let view (model : Model) (dispatch : Msg -> unit) =
    div [ ]
        [ Hero.hero
            [ Hero.Color IsPrimary
              Hero.IsMedium
              Hero.IsBold ]
            [ Hero.head [ ]
                [ Navbar.navbar [ ]
                    [ Container.container [ ]
                        [ navBrand
                          navMenu ] ] ]
              Hero.body [ ]
                [ Container.container [ Container.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                    [ Heading.p [ ]
                        [ str "SAFE Template" ]
                      Heading.p [ Heading.IsSubtitle ]
                          [ safeComponents ] ] ] ]

          buttonBox model dispatch

          Container.container [ ]
            [ features
              intro
              sandbox ]

          footer [ ClassName "footer" ]
            [ footerContainer ] ]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
