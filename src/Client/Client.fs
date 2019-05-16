module Client

open Elmish
open Elmish.React

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.PowerPack.Fetch
open Fable.Core.JsInterop
open Fable.FontAwesome
open Fable.FontAwesome.Free

open Thoth.Json

open Shared
open PathfinderAttackSimulator.Library
open AuxLibFunctions
open Modifications
open Server
open PathfinderAttackSimulator.StandardAttackAction
open PathfinderAttackSimulator.FullRoundAttackAction


open Fulma
open Fulma.Extensions.Wikiki


let exmpCharArr = [|Characters.myParrn; Characters.myTumor; Characters.myElemental|] |> Array.sort
let exmpWeaponArr = [|Weapons.bite;Weapons.butchersAxe;Weapons.claw; Weapons.greatswordParrn|] |> Array.sort
let ModificationArr = [|Modifications.Charging; DivineFavor; Wrath; Multiattack; Flanking; Haste; FlurryOfBlows; TwoWeaponFighting; TwoWeaponFightingImproved; MutagenStrength; Invisibility; BlessingOfFervorAttackBonus|] |> Array.sort
let VarModificationArr = [|PowerAttack;SneakAttack;SneakAttackOnce;PlanarFocusFire|] |> Array.sortBy (fun x -> (x 0).Name)
   

// The model holds data that you want to keep track of while the application is running
// in this case, we are keeping track of a counter
// we mark it as optional, because initially it will not be available from the client
// the initial value will be requested from server
type Model = { 
    ActiveWeapons : Weapon list
    ActiveChar : CharacterStats
    ActiveModifications : AttackModification list
    PathfinderResult : string
    VariableForVarModifications: int
    TabList: (int*Fable.Import.React.ReactElement*Fable.Import.React.ReactElement) list
    HiddenTabsList : (int*bool) list
    IDCounter : int
    }

// The Msg type defines what events/actions can occur while the application is running
// the state of the application changes *only* in reaction to these events
type Msg =
| UpdateCharacter of CharacterStats
| UpdateWeaponList of Weapon list
| UpdateModificationArr of AttackModification
| UpdateVariableForVarModifications of int
| CalculateStandardAttackAction of CharacterStats * Weapon list * AttackModification list
| CalculateFullRoundAttackAction of CharacterStats * Weapon list * AttackModification list
| AddTabToTabList of (Fable.Import.React.ReactElement)*(Fable.Import.React.ReactElement)
| HideShowTab of int
| CloseTab of int


// defines the initial state and initial command (= side-effect) of the application
let init () : Model * Cmd<Msg> =
    let initialModel = { 
        ActiveWeapons = []
        ActiveChar = EmptyChar
        ActiveModifications = []
        PathfinderResult = ""
        VariableForVarModifications = 0
        TabList = []
        HiddenTabsList = []
        IDCounter = 0
        }

    initialModel, Cmd.none

// The update function computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// these commands in turn, can dispatch messages to which the update function will react.
let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    match currentModel, msg with
    | _, UpdateCharacter stats ->
        let nextModel = {
            currentModel with
                ActiveChar = stats }
        nextModel, Cmd.none
    | _, UpdateWeaponList newWeapList ->
        let nextModel = {
            currentModel with
                ActiveWeapons = if newWeapList = [] then []
                                    else List.append newWeapList currentModel.ActiveWeapons
            }
        nextModel, Cmd.none
    | _, UpdateModificationArr newModification ->
        let nextModel = {
            currentModel with
                ActiveModifications = if newModification = ZeroMod then []
                                        else newModification::currentModel.ActiveModifications
            }
        nextModel, Cmd.none
    | _, UpdateVariableForVarModifications lvlBab ->
        let nextModel = {
            currentModel with
                VariableForVarModifications = lvlBab
            }
        nextModel, Cmd.none
    | _, CalculateStandardAttackAction (char,weaponList,modiList) ->
        let nextModel = {
            currentModel with
                PathfinderResult = myStandardAttack char Medium weaponList.[0] (modiList |> List.toArray)
            }
        nextModel, Cmd.none
    | _, AddTabToTabList (tab,tabTrue)->
        let nextModel = {
            currentModel with
                HiddenTabsList = (currentModel.IDCounter,false)::currentModel.HiddenTabsList
                TabList = (currentModel.IDCounter,tab,tabTrue)::currentModel.TabList
                IDCounter = currentModel.IDCounter+1
            }
        nextModel,Cmd.none
    | _, CloseTab (id) ->
        let nextModel = {
            currentModel with
                TabList = List.except [currentModel.TabList.Item (List.tryFindIndex (fun (i,y,z) -> i = id) currentModel.TabList).Value] currentModel.TabList
            }
        nextModel,Cmd.none
    | _, HideShowTab (id) ->
        let nextModel = {
            currentModel with
                HiddenTabsList = (List.tryFind (fun (index,bool) -> index = id) currentModel.HiddenTabsList)
                                 |> fun x -> if x.IsSome && x.Value = (id,true)
                                             then (id,false)::(List.except [(id,true)] currentModel.HiddenTabsList)
                                             else (id,true)::(List.except [(id,false)] currentModel.HiddenTabsList)
            }
        nextModel,Cmd.none
    //| _, CalculateFullRoundAttackAction (char,weaponList,modiList) ->
    //     let nextModel = {
    //        currentModel with
    //            PathfinderResult = Pathfinder.FullRoundAttack.myFullAttack char
    //        }
    
    
    | _ -> currentModel, Cmd.none


let safeComponents =
    let components =
        span [ ]
           [
             a [ Href "http://suave.io" ] [ str "Suave" ]
             str ", "
             a [ Href "http://fable.io" ] [ str "Fable" ]
             str ", "
             a [ Href "https://elmish.github.io/elmish/" ] [ str "Elmish" ]
             str ", "
             a [ Href "https://mangelmaxime.github.io/Fulma" ] [ str "Fulma" ]
             str ", "
             a [ Href "https://dansup.github.io/bulma-templates/" ] [ str "Bulma\u00A0Templates" ]
           ]

    p [ ]
        [ strong [] [ str "SAFE Template" ]
          str " powered by: "
          components ]

let navBrand =
    Navbar.Brand.div [ ]
        [ Navbar.Item.a
            [ Navbar.Item.Props [ Href "https://safe-stack.github.io/" ] ]
            [ img [ Src "https://safe-stack.github.io/images/safe_top.png"
                    Alt "Logo" ] ]
          Navbar.burger [ ]
            [ span [ ] [ ]
              span [ ] [ ]
              span [ ] [ ] ] ]

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
                    [] ] ] ]


let DropDownWeapons (activeWeaponArr:Weapon []) (model : Model) (dispatch : Msg -> unit) =
    Dropdown.dropdown [ Dropdown.IsHoverable ]
        [ div [ ]
            [ Button.button [ ]
                [ span [ ]
                    [ str "Weapons" ]
                  Icon.icon [ Icon.Size IsSmall
                            ]
                    [ i [ClassName "fa fa-angle-down"] [] ]
                ]
            ]
          Dropdown.menu [ ]
            [ Dropdown.content [ ]
                (activeWeaponArr
                    |> Array.map (fun x -> Button.a 
                                            [Button.OnClick (fun _ -> dispatch (UpdateWeaponList [x])
                                                            )
                                             Button.Props [Style [Width "300px"; OverflowX "auto"] ]
                                             Button.Size IsSmall
                                            ]
                                            [str (sprintf "%A" x.Name)]
                                 ) |> Seq.ofArray)
            ]
        ]

let DropDownCharacters activeCharArr (dispatch : Msg -> unit) =
    Dropdown.dropdown [ Dropdown.IsHoverable ]
        [ div [ ]
            [ Button.button [ ]
                [ span [ ]
                    [ str "Characters" ]
                  Icon.icon [ Icon.Size IsSmall ]
                    [ i [ClassName "fa fa-angle-down"] [] ]
                ] 
            ]
          Dropdown.menu [ ]
            [ Dropdown.content [ ]
                (activeCharArr
                    |> Array.map (fun x -> Button.a
                                            [ Button.OnClick (fun _ -> dispatch (UpdateCharacter x))
                                              Button.Props [Style [Width "300px"; OverflowX "auto"] ]
                                              Button.Size IsSmall
                                            ]
                                            [ str (sprintf "%A" x.CharacterName) ]
                                 ) |> Seq.ofArray
                )
            ]
        ]


let DropDownModifications (model:Model) (dispatch : Msg -> unit) =
    Dropdown.dropdown [ Dropdown.IsHoverable
                        Dropdown.IsRight
                      ]
        [ div [ ]
              [ Button.button [ ]
                              [ span [ ]
                                  [ str "Modifications" ]
                                Icon.icon [ Icon.Size IsSmall ]
                                  [ i [ClassName "fa fa-angle-down"] [] ]
                              ]
              ]
          Dropdown.menu
            [ ]
            [ Dropdown.content [ Props [ Style [CSSProp.MaxHeight "250px"; (*CSSProp.OverflowY "scroll"; CSSProp.OverflowX "Visible"*)] ]
                               ]
                              ([  
                                  ( VarModificationArr
                                      |> Array.map (fun x -> Button.a
                                                                  [ Button.Size IsSmall
                                                                    Button.OnClick (fun _ -> dispatch (UpdateModificationArr (x model.VariableForVarModifications)) )
                                                                    Button.Props [ Tooltip.dataTooltip (str (sprintf "%A" (x 0).Description))
                                                                                   Style [ Width "300px" ]
                                                                                    ]
                                                                    Button.CustomClass (Tooltip.ClassName + " " + Tooltip.IsMultiline)
                                                                  ]
                                                                  [ (str (sprintf "%A" (x 0).Name))
                                                                  ]
                                                       )
                                  );
                                  [| Dropdown.divider [] |];
                                  [| Control.div []
                                                 [ Input.text [ Input.Size IsSmall
                                                                Input.Placeholder "BAB/Lvl/etc."
                                                                Input.OnChange (fun e -> let x = !!e.target?value
                                                                                         dispatch (UpdateVariableForVarModifications x) )]
                                                 ]
                                  |];
                                  [| Dropdown.divider [] |];
                                  ( ModificationArr
                                       |> Array.map (fun x -> Button.a
                                                                  [ Button.Size IsSmall
                                                                    Button.OnClick (fun _ -> dispatch (UpdateModificationArr x) )
                                                                    Button.Props [ Tooltip.dataTooltip (str (sprintf "%A" x.Description))
                                                                                   Style [ Width "300px" ] 
                                                                                    ]
                                                                    Button.CustomClass (Tooltip.ClassName + " " + Tooltip.IsMultiline (*+ " " + Tooltip.IsTooltipLeft*) ) 
                                                                  ]
                                                                  [ str (sprintf "%A" x.Name) ]
                                                    )
                                  )
                               ] |> Seq.concat)
            ]
        ]
let pathfinderCoreChooseModule activeCharArr activeWeaponArr (model: Model) (dispatch : Msg -> unit) =
    Level.level [ ]
        [ Level.item [ Level.Item.HasTextCentered ]
            [ div [ ]
                [ Level.heading [ ]
                    [ str "Choose the Attacker" ]
                  Level.title [ ]
                    [ DropDownCharacters activeCharArr dispatch ]
                  Level.title []
                    [ Button.button [Button.OnClick (fun _ -> dispatch (UpdateCharacter EmptyChar))
                                     Button.Size IsSmall
                                     Button.IsOutlined
                                     Button.Color IsDanger ]
                                    [str "Reset Character"]
                    ]
                ]
            ]
          Level.item [ Level.Item.HasTextCentered ]
            [ div [ ]
                [ Level.heading [] 
                    [ str "Choose your Weapons" ]
                  Level.title [ ]
                    [ DropDownWeapons activeWeaponArr model dispatch ]
                  Level.title []
                    [ Button.button [Button.OnClick (fun _ -> dispatch (UpdateWeaponList []) )
                                     Button.Size IsSmall
                                     Button.IsOutlined
                                     Button.Color IsDanger ]
                                    [str "Reset Weapons"]
                    ]
                ]
            ]
          Level.item [ Level.Item.HasTextCentered ]
            [ div [ ]
                [ Level.heading [ ]
                    [ str "Choose your Modifications" ]
                  Level.title [ ]
                    [ DropDownModifications model dispatch ]
                  Level.title []
                    [ Button.button [Button.OnClick (fun _ -> dispatch (UpdateModificationArr ZeroMod))
                                     Button.Size IsSmall
                                     Button.IsOutlined
                                     Button.Color IsDanger ]
                                    [str "Reset Modifications"]
                    ]
                ]
            ]
        ]

let pathfinderCoreShowModule (model: Model) (dispatch : Msg -> unit) =
            Container.container [ Container.IsFluid ]
                                [ Content.content [ ]
                                                  [ h1 [ ] [ str model.ActiveChar.CharacterName ]
                                                    p [ ]
                                                      [str  ( (string (model.ActiveWeapons |> List.map (fun x -> x.Name)
                                                                      )
                                                              )
                                                              |> fun x -> x.Trim([|'[';']'|])
                                                              |> fun x -> x.Replace(';',',')
                                                            )
                                                      ]
                                                    p []
                                                      [str ((string (model.ActiveModifications |> List.map (fun x -> x.Name)
                                                                   )
                                                            )
                                                           |> fun x -> x.Trim([|'[';']'|])
                                                           |> fun x -> x.Replace(';',',')
                                                           )
                                                      ]
                                                    Container.container [ Container.IsFluid ]
                                                                        [ Content.content [ ]
                                                                                          [ h1 []
                                                                                               [ Button.button [Button.OnClick (fun _ -> dispatch (CalculateStandardAttackAction (model.ActiveChar,model.ActiveWeapons,model.ActiveModifications)
                                                                                                                                                  )
                                                                                                                               )
                                                                                                               ]
                                                                                                               [str "Standard Attack Action"]
                                                                                                 Button.button []
                                                                                                               [str "Fullround Attack Action"]
                                                                                               ]
                                                                                            p [ ]
                                                                                              [str (sprintf "%A \n" model.PathfinderResult)]
                                                                                          ]
                                                                        ]
                                                  ]
                                ]


let attackCalculatorCard (model: Model) (dispatch : Msg -> unit) (id:int) (hidden:bool)=

    Card.card [  ]
        [ Card.header [ ]
            [ Card.Header.title [ Card.Header.Title.IsCentered ]
                                [ str "Attack Calculator" ]
              Button.button [ Button.Color IsWhite
                              Button.OnClick (fun _ -> dispatch (HideShowTab id))
                              Button.Props [ Tooltip.dataTooltip "hide/show" ]
                              Button.CustomClass (Tooltip.ClassName + " " + Tooltip.IsTooltipLeft)
                            ]
                            [ Icon.icon [ Icon.Size IsSmall ]
                                        [ i [ClassName "fa fa-angle-down"] [] ]
                            ]
              Button.button [ Button.Color IsWhite
                              Button.OnClick (fun _ -> dispatch (CloseTab id))
                              Button.Props [ Tooltip.dataTooltip "close" ]
                              Button.CustomClass (Tooltip.ClassName + " " + Tooltip.IsTooltipLeft)
                            ]
                            [ Icon.icon [ Icon.Size IsSmall ]
                                        [ i [ClassName "fa fa-times-circle"] [] ]
                            ]
            ]
          Card.content [ Props [Props.Hidden hidden] ]
            [ Content.content [ ]
                [ str "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Phasellus nec iaculis mauris." ] ] 
          Card.footer [ ]
            [ Card.Footer.a [ ]
                            [ str "Characters" ]
              Card.Footer.a [ ]
                            [ str "Weapons" ]
              Card.Footer.a [ ]
                            [ str "Modifications" ] ] ]
///Props [Props.Hidden true]

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

let footerContainer =
    Container.container [ ]
        [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
            [ p [ ]
                [ safeComponents ]
              p [ ]
                [ ] ] ]

let view (model : Model) (dispatch : Msg -> unit) =
    div [ ]
        [   Hero.hero
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
                          [ str "Pathfinder Attack Calculator" ]
                        Heading.p [ Heading.IsSubtitle ]
                            [ safeComponents ] ] ]
              ]
            Container.container [ Container.IsFluid ]
                                [ Button.button [ Button.Color IsWhite
                                                  Button.OnClick (fun _ -> dispatch (AddTabToTabList ((attackCalculatorCard model dispatch model.IDCounter false),(attackCalculatorCard model dispatch model.IDCounter true))
                                                                                    )
                                                                 )
                                                ]
                                                [ str "Open new Attack Calculator tab"]
                                  Content.content [ ]
                                                  (List.map (fun (i,tabFalse,tabTrue) -> if (List.exists (fun (index,boolean) -> index = i && boolean = false) model.HiddenTabsList)
                                                                                         then tabFalse
                                                                                         else tabTrue
                                                            ) model.TabList
                                                  )      
                                ]

            pathfinderCoreChooseModule exmpCharArr exmpWeaponArr model dispatch

            pathfinderCoreShowModule model dispatch

            footer [ ClassName "footer" ]
              [ footerContainer ]
        ]


#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
|> Program.withHMR
#endif
|> Program.withReact "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
