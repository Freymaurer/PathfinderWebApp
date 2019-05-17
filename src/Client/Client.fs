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
open Fable.Import
open Fable.Import
open Fable.Import
open Fable.Import
open Fable.Import
open System.Diagnostics.Tracing


let exmpCharArr = [|Characters.myParrn; Characters.myTumor; Characters.myElemental;|] |> Array.sort
let exmpWeaponArr = [|Weapons.bite;Weapons.butchersAxe;Weapons.claw; Weapons.greatswordParrn|] |> Array.sort
let ModificationArr = [|Modifications.Charging; DivineFavor; Wrath; Multiattack; Flanking; Haste; FlurryOfBlows; TwoWeaponFighting; TwoWeaponFightingImproved; MutagenStrength; Invisibility; BlessingOfFervorAttackBonus|] |> Array.sort
let VarModificationArr = [|PowerAttack;SneakAttack;SneakAttackOnce;PlanarFocusFire|] |> Array.sortBy (fun x -> (x 0).Name)
let testArr = [|
                Multiattack;SneakAttackOnce 0;TwoWeaponFighting;TwoWeaponFightingImproved;Haste;FlurryOfBlows;Shaken;WeaponFocus;EnlargePerson;MutagenStrength;
                Invisibility;PlanarFocusFire 0;SneakAttack 0;Wrath;DivineFavor;FuriousFocus 0;PowerAttack 0;Flanking;Charging;WeaponSpecialization;Fatigued;
                AidAnother;VitalStrike;VitalStrikeImproved;VitalStrikeGreater;InspireCourage 0; ShockingGrasp 0 true; ShockingGraspIntensifiedEmpowered 0 true; PowerAttackURL OffHand 0;
                BlessingOfFervorAttackBonus; BonusAttackDamage 0 0;
              |] |> Array.sortBy (fun x -> x.Name)


let sorensenCoefficent (str1:string) (str2:string) =
    let charSeq1 = str1.ToCharArray()
    let charSeq2 = str2.ToCharArray()
    let startLength1 = str1.Length
    let startLength2 = str2.Length
    let mutable str1Mut = str1
    let mutable str2Mut = str2
    let numberOfCommonSpecies1 =
        charSeq1
        |> Array.map (fun x -> (str2Mut.IndexOf x) 
                               |> fun x -> if x < 0 
                                           then str2Mut <- str2Mut 
                                           else str2Mut <- str2Mut.Remove(x,1)
                     )
    let numberOfCommonSpecies2 =
        charSeq2
        |> Array.map (fun x -> (str1Mut.IndexOf x) 
                               |> fun x -> if x < 0 
                                           then str1Mut <- str1Mut 
                                           else str1Mut <- str1Mut.Remove(x,1)
                     )
    let numberOfSpeciesCommon =
        (startLength2 - str2Mut.Length,startLength1 - str1Mut.Length)
        |> fun (x,y) -> if x <> y then failwith "unknown case"
                        else float x
    let numberOfSpeciesSpecificToFst =
        float startLength1 - numberOfSpeciesCommon
    let numberOfSpeciesSpecificToSnd =
        float startLength2 - numberOfSpeciesCommon
    (2. * numberOfSpeciesCommon)/((2. * numberOfSpeciesCommon) + numberOfSpeciesSpecificToFst + numberOfSpeciesSpecificToSnd)

let searchForCharacters (arr: CharacterStats []) (searchForStr:string)=
    arr
    |> Array.map (fun x -> sorensenCoefficent x.CharacterName searchForStr,x)
    |> Array.sortByDescending (fun (score,characterSt) -> score)
    |> Array.truncate 5
    |> Array.filter (fun (score,x) -> score > 0.2)
    |> Array.map snd

let searchForModifications (arr: AttackModification []) (searchForStr:string)=
    let (garantiedHits,restArr) = arr
                                  |> Array.mapi (fun i x ->i, x.Name.Contains(searchForStr) )
                                  |> Array.filter (fun x -> snd x = true)
                                  |> fun x -> Array.map (fun foundHit -> arr.[fst foundHit]) x, Array.except (Array.map (fun foundHit -> arr.[fst foundHit]) x) arr
    restArr
    |> Array.map (fun x -> sorensenCoefficent x.Name searchForStr
                           ,x)
    |> Array.sortByDescending (fun (score,characterSt) -> score)
    |> Array.filter (fun (score,x) -> score > 0.2) 
    |> Array.map snd
    |> Array.append garantiedHits
    |> Array.truncate 5

let searchForWeapons (arr: Weapon []) (searchForStr:string)=
    arr
    |> Array.map (fun x -> sorensenCoefficent x.Name searchForStr,x)
    |> Array.sortByDescending (fun (score,characterSt) -> score)
    |> Array.truncate 5
    |> Array.filter (fun (score,x) -> score > 0.2)
    |> Array.map snd

type SubTabsSearchBars = {
    SearchCharacter     : string
    SearchWeapon        : string
    SearchModification  : string
    }

let createSubTabsSearchBars searchCha searchWea searchModi= {
    SearchCharacter    = searchCha
    SearchWeapon       = searchWea
    SearchModification = searchModi
    }

type SearchResult = {
    SearchResultChar          : string []
    SearchResultWeapons       : string []
    SearchResultModifications : string []
    }

let createSearchResult searchResultChar searchResultWea searchResultModi= {
    SearchResultChar          = searchResultChar
    SearchResultWeapons       = searchResultWea
    SearchResultModifications = searchResultModi
    }

type ActiveModifiers = {
    ActiveCharacter     : CharacterStats
    ActiveSize          : SizeType
    ActiveWeapons       : Weapon list
    ActiveModifications : AttackModification list
    }

let createActiveModifiers activeChar activeSize activeWeapons activeModi= {
    ActiveCharacter     = activeChar
    ActiveSize          = activeSize
    ActiveWeapons       = activeWeapons
    ActiveModifications = activeModi
    }

// The model holds data that you want to keep track of while the application is running
// in this case, we are keeping track of a counter
// we mark it as optional, because initially it will not be available from the client
// the initial value will be requested from server
type Model = { 
    ActiveWeapons       : Weapon list
    ActiveChar          : CharacterStats
    ActiveModifications : AttackModification list
    PathfinderResult : string
    VariableForVarModifications: int
    TabList: (int*Fable.Import.React.ReactElement) list
    SearchBarList: (int*SubTabsSearchBars) list
    SearchResultList : (int*SearchResult) list
    ActiveModifierList : (int * ActiveModifiers) list
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
| AddTabToTabList of (Fable.Import.React.ReactElement)
| UpdateSearchBarList of int * int * string
| UpdateSearchResultList of int * int
| UpdateActiveModifierList of int * int * string
| CloseTab of int

let createInsertButton id intForWhichTab (searchForName:string) (dispatch : Msg -> unit) =
    Button.button [ Button.Props [ Props.Id (sprintf "Button%i%i" id intForWhichTab) ]
                    Button.OnClick (fun _ -> dispatch (UpdateActiveModifierList (id,intForWhichTab,searchForName)
                                                      )
                                   )
                    Button.Color IsSuccess; Button.IsInverted
                  ]
                  [ str (sprintf "add %s" searchForName)]

//<button class="button is-success is-inverted" id="Button01">add Parrn</button>

// defines the initial state and initial command (= side-effect) of the application
let init () : Model * Cmd<Msg> =
    let initialModel = { 
        ActiveWeapons = []
        ActiveChar = EmptyChar
        ActiveModifications = []
        PathfinderResult = ""
        VariableForVarModifications = 0
        TabList = []
        SearchBarList = []
        SearchResultList = []
        ActiveModifierList = []
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
    | _, AddTabToTabList (tab)->
        let nextModel = {
            currentModel with
                TabList = (currentModel.IDCounter,tab)::currentModel.TabList
                IDCounter = currentModel.IDCounter+1
                SearchBarList = (currentModel.IDCounter,createSubTabsSearchBars "" "" "")::currentModel.SearchBarList
                SearchResultList = (currentModel.IDCounter,createSearchResult [||] [||] [||])::currentModel.SearchResultList
                ActiveModifierList = (currentModel.IDCounter, createActiveModifiers EmptyChar Medium [] [])::currentModel.ActiveModifierList
            }
        nextModel,Cmd.none
    | _, CloseTab (id) ->
        let nextModel = {
            currentModel with
                TabList = List.except [currentModel.TabList.Item (List.tryFindIndex (fun (i,y) -> i = id) currentModel.TabList).Value] currentModel.TabList
                SearchBarList = List.except [currentModel.SearchBarList.Item (List.tryFindIndex (fun (i,y) -> i = id) currentModel.SearchBarList).Value] currentModel.SearchBarList
                SearchResultList = List.except [currentModel.SearchResultList.Item (List.tryFindIndex (fun (i,y) -> i = id) currentModel.SearchResultList).Value] currentModel.SearchResultList
                ActiveModifierList = List.except [currentModel.ActiveModifierList.Item (List.tryFindIndex (fun (i,y) -> i = id) currentModel.ActiveModifierList).Value] currentModel.ActiveModifierList
            }
        nextModel,Cmd.none     
    | _, UpdateSearchBarList (id,intForWhichTab,input) ->
         let nextModel = {
            currentModel with
                SearchBarList = ( (List.tryFind (fun (cardId,searchBarList) -> cardId = id) currentModel.SearchBarList)
                                  |> fun x -> let updatedSearch =
                                                  x.Value
                                                  |> fun (index,searchBar) -> match intForWhichTab with
                                                                              | 1 -> index,createSubTabsSearchBars input searchBar.SearchWeapon searchBar.SearchModification
                                                                              | 2 -> index,createSubTabsSearchBars searchBar.SearchCharacter input searchBar.SearchModification
                                                                              | 3 -> index,createSubTabsSearchBars searchBar.SearchCharacter searchBar.SearchWeapon input
                                                                              | _ -> failwith "unknown case, you should not get this 001"
                                              if x.IsNone
                                              then failwith "unknown case, you should not get this 002"
                                              else updatedSearch::(List.except [x.Value] currentModel.SearchBarList)
                                )
            }   
         nextModel,Cmd.none
    | _, UpdateSearchResultList (id,intForWhichTab) ->
         let tabCategory =
            match intForWhichTab with
            | 1 -> "characters"
            | 2 -> "weapons"
            | 3 -> "modifications"
            | _ -> "oh no"
         let hugeTest = (Browser.document.getElementById (sprintf "SearchResultTable%s%i" tabCategory id))
         let searchInput =
            ((List.tryFind (fun (index,searchBarType) -> index = id) currentModel.SearchBarList)
            |> fun x -> if x.IsNone
                        then "Error"
                        else match intForWhichTab with
                             | 1 -> (snd x.Value).SearchCharacter
                             | 2 -> (snd x.Value).SearchWeapon
                             | 3 -> (snd x.Value).SearchModification
                             | _ -> failwith "unknown case, you should not get this 004"
            )
         let filter =
             match intForWhichTab with
             | 1 -> ((searchForCharacters exmpCharArr searchInput) |> Array.map (fun x -> x.CharacterName) )
             | 2 -> ((searchForWeapons exmpWeaponArr searchInput) |> Array.map (fun x -> x.Name) )
             | 3 -> ((searchForModifications ModificationArr searchInput) |> Array.map (fun x -> x.Name) )
             | _ -> failwith "unknown case, you should not get this 004"                                                                                                                                
         let (oldValue,heavyCalculation) =
            (List.tryFind (fun (cardId,searchBarList) -> cardId = id) currentModel.SearchResultList)
            |> fun x -> let updatedResult =
                            x.Value
                            |> fun (index,searchResult) -> match intForWhichTab with
                                                           | 1 -> index,createSearchResult filter searchResult.SearchResultWeapons searchResult.SearchResultModifications
                                                           | 2 -> index,createSearchResult searchResult.SearchResultChar filter searchResult.SearchResultModifications
                                                           | 3 -> index,createSearchResult searchResult.SearchResultChar searchResult.SearchResultWeapons filter
                                                           | _ -> failwith "unknown case, you should not get this 001"
                        if x.IsNone
                        then failwith "unknown case, you should not get this 002"
                        else x.Value,updatedResult
         let newSearchResultList = heavyCalculation::(List.except [oldValue] currentModel.SearchResultList)
         let newSearchResultListTable =
            let filter =
             match intForWhichTab with
             | 1 -> ((searchForCharacters exmpCharArr searchInput) |> Array.map (fun x -> x.CharacterName,"a nice guy","here will be a button") )
             | 2 -> ((searchForWeapons exmpWeaponArr searchInput) |> Array.map (fun x -> x.Name,x.Description,"here will be a button") )
             | 3 -> ((searchForModifications testArr searchInput) |> Array.map (fun x -> x.Name,x.Description,"here will be a button") )
             | _ -> failwith "unknown case, you should not get this 004"                                   
            filter
            |> Array.map (fun (name,desc,button) -> sprintf "<tr>
                                                                <th>%s</th>
                                                                <th>%s</th>
                                                                <th>%s</th>
                                                             </tr>" name desc button )
            |> Array.fold (fun elem arr -> elem + arr) ""
         let nextModel = {
            currentModel with
                SearchResultList = newSearchResultList
         }
         hugeTest.innerHTML <- (sprintf "%A" newSearchResultListTable)
         nextModel,Cmd.none
    | _, UpdateActiveModifierList (id,intForWhichTab,searchForString) ->
        let activeModifierMatchedID =
            List.tryFind ( fun (index,values) -> index = id) currentModel.ActiveModifierList
            |> fun x -> snd x.Value
        let searchCharArr (str:string)=
            exmpCharArr
            |> Array.tryFind (fun x -> x.CharacterName = str)
            |> fun x -> if x.IsNone then failwith "Error 005" else x.Value
        let searchWeaponArr (str:string)=
            exmpWeaponArr
            |> Array.tryFind (fun x -> x.Name = str)
            |> fun x -> if x.IsNone then failwith "Error 006" else x.Value
        let searchModificationArr (str:string)=
            ModificationArr
            |> Array.tryFind (fun x -> x.Name = str)
            |> fun x -> if x.IsNone then failwith "Error 007" else x.Value //only works for non function Modifications
        let updatedModifiers =
            match intForWhichTab with
            | 1 -> (searchCharArr searchForString)
                   |> fun x -> let outputChar = (Browser.document.getElementById (sprintf "OutputCharacter%i" id))
                               outputChar.innerHTML <- x.CharacterName
                               createActiveModifiers x activeModifierMatchedID.ActiveSize activeModifierMatchedID.ActiveWeapons activeModifierMatchedID.ActiveModifications
            | 2 -> (searchWeaponArr searchForString)
                   |> fun x -> let outputWeapon = (Browser.document.getElementById (sprintf "OutputWeapons%i" id))
                               outputWeapon.innerHTML <- (outputWeapon.innerHTML + ", " + x.Name)
                               createActiveModifiers activeModifierMatchedID.ActiveCharacter activeModifierMatchedID.ActiveSize (x::activeModifierMatchedID.ActiveWeapons) activeModifierMatchedID.ActiveModifications
            | 3 -> (searchModificationArr searchForString)
                   |> fun x -> let outputModifications = (Browser.document.getElementById (sprintf "OutputModifications%i" id))
                               outputModifications.innerHTML <- (outputModifications.innerHTML + ", " + x.Name)
                               createActiveModifiers activeModifierMatchedID.ActiveCharacter activeModifierMatchedID.ActiveSize activeModifierMatchedID.ActiveWeapons (x::activeModifierMatchedID.ActiveModifications)
            | _ -> failwith "Error 008"
        let nextModel = {
            currentModel with
                ActiveModifierList = (id,updatedModifiers)::(List.except [currentModel.ActiveModifierList.Item (List.tryFindIndex (fun (i,y) -> i = id) currentModel.ActiveModifierList).Value] currentModel.ActiveModifierList)
            }
        nextModel,Cmd.none
    
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
            [ Dropdown.content [ Props [ Style [CSSProp.MaxHeight "250px"] ]
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
                                                                    Button.CustomClass (Tooltip.ClassName + " " + Tooltip.IsMultiline ) 
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


let doHideShow (tabId:string) (cardID:int) =
    let x = Browser.document.getElementById(sprintf "%A%i" tabId cardID)
    if x.style.display = "none"
    then x.style.display <- "block"
    else x.style.display <- "none"

let doHide (tabId:string) (cardID:int) =
    let x = Browser.document.getElementById(sprintf "%A%i" tabId cardID)
    x.style.display <- "none"


let searchBarTab (dispatch : Msg -> unit) (id:int) (tabCategory:string)=

    Content.content [ Content.Props [ Props.Id (sprintf "Tab%s%i" tabCategory id); Props.Style [CSSProp.Display "none"]
                                    ]
                    ]
                    [ Columns.columns [ ]
                                      [ Column.column [ Column.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                                            [ Heading.h6 [ ]
                                                         [ str "Searchbar"] 
                                              Level.item [ ]
                                                         [ Field.div [ Field.HasAddons ]
                                                                     [ Control.div [ ]
                                                                                   [ Input.text [ Input.Placeholder (sprintf "Search %s" tabCategory)
                                                                                                  Input.OnChange (fun e -> let x = !!e.target?value
                                                                                                                           let getIntForTabCategory =
                                                                                                                               match tabCategory with
                                                                                                                               | "characters" -> 1
                                                                                                                               | "weapons" -> 2
                                                                                                                               | "modifications" -> 3
                                                                                                                               | _ -> failwith "unknown case, you should not get this 003"
                                                                                                                           dispatch (UpdateSearchBarList (id,getIntForTabCategory,x))
                                                                                                                 ) 
                                                                                                ] 
                                                                                   ]
                                                                       Control.div [ ]
                                                                                   [ Button.button [ Button.OnClick (fun _ -> let getIntForTabCategory =
                                                                                                                                  match tabCategory with
                                                                                                                                  | "characters" -> 1
                                                                                                                                  | "weapons" -> 2
                                                                                                                                  | "modifications" -> 3
                                                                                                                                  | _ -> failwith "unknown case, you should not get this 003"
                                                                                                                              dispatch (UpdateSearchResultList (id, getIntForTabCategory))

                                                                                                                    )
                                                                                                   ]
                                                                                                   [ str "Search" ]
                                                                                   ]
                                                                     ]
                                                         ]
                                            ]
                                        Column.column [ Column.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                                            [ Heading.h6 [ ]
                                                         [ str "Results" ]
                                              Table.table [ Table.IsBordered
                                                            Table.IsNarrow ]
                                                          [ thead [ ]
                                                                  [ tr [ ]
                                                                       [ th [ ] [ str "Name" ]
                                                                         th [ ] [ str "Description" ]
                                                                         th [ ] [ str "add" ] ] ]
                                                            tbody [ Props.Id (sprintf "SearchResultTable%s%i" tabCategory id) ]
                                                                  /// module to display SearchResults
                                                                  [ tr [ ]
                                                                       [ th [ ] [ str "ex1" ]
                                                                         th [ ] [ str "ex2" ]
                                                                         th [ ] [ str "ex3" ] ] ]
                                                          ]
                                            ]
                                      ]
                    ]


let attackCalculatorCard (dispatch : Msg -> unit) (id:int) =

    Card.card [  ]
        [ Card.header [ ]
            [ Card.Header.title [ Card.Header.Title.IsCentered ]
                                [ str "Attack Calculator" ]
              Button.button [ Button.Color IsWhite
                              Button.OnClick (fun _ -> doHideShow "TabMainInfo" id
                                                       doHideShow "TabMainOutput" id
                                                       doHide "Tabcharacters" id
                                                       doHide "Tabsize" id
                                                       doHide "Tabweapons" id
                                                       doHide "Tabmodifications" id
                                             )
                              Button.Props [ Tooltip.dataTooltip "hide/show card" ]
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
          Card.content [ Props [ Props.Id (sprintf "TabMainOutput%A" id); Props.Style [CSSProp.Display "none"] ] ]
                       [ Container.container [ Container.IsFluid ]
                                             [ h1 [ Props.Id (sprintf "OutputCharacter%A" id) ]
                                                  [ str "Here will be your CharacterName" ]
                                               p [ Props.Id (sprintf "OutputSize%A" id) ]
                                                 [ str "Here will be your Size" ]
                                               p [ Props.Id (sprintf "OutputWeapons%A" id) ]
                                                 [ str "Here will be your Weapons" ]
                                               p [ Props.Id (sprintf "OutputModifications%A" id) ]
                                                 [ str "Here will be your Modifications" ] 
                                             ]
                       ]
          Card.content [ Props [ Props.Id (sprintf "TabMainInfo%A" id); Props.Style [CSSProp.Display "none"] ] ]
                       [ Notification.notification [ Notification.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered)] ]
                                                   [ str "Here will be your result! Try it out!" ]
                         Level.level [ ]
                                     [ Level.item [ Level.Item.HasTextCentered ]
                                         [ div [ ]
                                               [ Button.button [ Button.Color IsInfo
                                                                 Button.IsInverted
                                                                 Button.OnClick (fun _ -> doHideShow "Tabcharacters" id
                                                                                          doHide "Tabsize" id
                                                                                          doHide "Tabweapons" id
                                                                                          doHide "Tabmodifications" id
                                                                                )
                                                               ]
                                                               [ span [] [ str "Character" ]
                                                                 Icon.icon [ ] [ i [ ClassName "fa fa-angle-down" ] [ ] ]
                                                               ]
                                               ]
                                         ]
                                       Level.item [ Level.Item.HasTextCentered ]
                                         [ div [ ]
                                               [ Button.button [ Button.Color IsInfo
                                                                 Button.IsInverted
                                                                 Button.OnClick (fun _ -> doHideShow "Tabsize" id
                                                                                          doHide "Tabcharacters" id
                                                                                          doHide "Tabweapons" id
                                                                                          doHide "Tabmodifications" id
                                                                                )
                                                               ]
                                                               [ span [] [ str "Size" ]
                                                                 Icon.icon [ ] [ i [ ClassName "fa fa-angle-down" ] [ ] ]
                                                               ]
                                               ]
                                         ]
                                       Level.item [ Level.Item.HasTextCentered ]
                                         [ div [ ]
                                               [ Button.button [ Button.Color IsInfo
                                                                 Button.IsInverted
                                                                 Button.OnClick (fun _ -> doHideShow "Tabweapons" id
                                                                                          doHide "Tabsize" id
                                                                                          doHide "Tabcharacters" id
                                                                                          doHide "Tabmodifications" id
                                                                                )
                                                               ]
                                                               [ span [] [ str "Weapons" ]
                                                                 Icon.icon [ ] [ i [ ClassName "fa fa-angle-down" ] [ ] ]
                                                               ]
                                               ]
                                         ]
                                       Level.item [ Level.Item.HasTextCentered ]
                                         [ div [ ]
                                               [ Button.button [ Button.Color IsInfo
                                                                 Button.IsInverted
                                                                 Button.OnClick (fun _ -> doHideShow "Tabmodifications" id
                                                                                          doHide "Tabsize" id
                                                                                          doHide "Tabweapons" id
                                                                                          doHide "Tabcharacters" id
                                                                                )
                                                               ]
                                                               [ span [] [ str "Modifications" ]
                                                                 Icon.icon [ ] [ i [ ClassName "fa fa-angle-down" ] [ ] ]
                                                               ]
                                               ]
                                         ]
                                     ]
                       ]
          searchBarTab dispatch id "characters"
          searchBarTab dispatch id "size"
          searchBarTab dispatch id "weapons"
          searchBarTab dispatch id "modifications"
          Card.Footer.div [Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered)
                                       Modifier.TextWeight TextWeight.Bold ]
                          ] 
                          [ str "Here will be a name" ]          
        ]

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
                                [ Heading.p [ ]
                                            [ Button.button [ Button.Color IsWhite
                                                              Button.OnClick (fun _ -> dispatch (AddTabToTabList (attackCalculatorCard dispatch model.IDCounter)
                                                                                                )
                                                                             )
                                                            ]
                                                            [ str "Open new Attack Calculator tab"] 
                                            ]
                                  Content.content []
                                                  [ Content.content [ ]
                                                                    (List.map (fun (index,tab) -> tab) (List.sortByDescending fst model.TabList))
                                                  ]              
                                ]
            //createInsertButton 0 1 "Parrn" dispatch
            //Card.card [ GenericOption.Props [ Props.Id "TestMe" ] ]
            //          [Button.button []
            //                [str "test me"]
            //          ]
            //Button.button [ Button.OnClick (fun _ -> let x = (Browser.document.getElementById "TestMe")
            //                                         x.innerHTML <- "<button onclick=\"createInsertButton(0,1,\"Parr\",dispatch)\">add Parrn</button>
            //                                                         <script>
            //                                                         export function createInsertButton(id$$1, intForWhichTab, searchForName, dispatch) {
            //                                                           var clo1, clo1$$1;
            //                                                           return button$$1(ofArray([new Option(15, \"Props\", new List(new HTMLAttr(59, \"Id\", (clo1 = toText(printf(\"Button%i%i\")), function (arg10) {
            //                                                             const clo2 = clo1(arg10);
            //                                                             return function (arg20) {
            //                                                               return clo2(arg20);
            //                                                             };
            //                                                           })(id$$1)(intForWhichTab)), new List())), new Option(16, \"OnClick\", function (_arg1) {
            //                                                             dispatch(new Msg(9, \"UpdateActiveModifierList\", id$$1, intForWhichTab, searchForName));
            //                                                           }), new Option(0, \"Color\", new Color$0024002EIColor(6, \"is-success\")), new Option(5, \"is-inverted\")]), new List(Helpers$0024$0024$0024str((clo1$$1 = toText(printf(\"add %s\")), function (arg10$$1) {
            //                                                             return clo1$$1(arg10$$1);
            //                                                           })(searchForName)), new List()));
            //                                                         }
            //                                                         </script>"
            //                               )
            //              ]
            //              [ str "click to change above button"]
            //pathfinderCoreChooseModule exmpCharArr exmpWeaponArr model dispatch
            //str (string model.SearchBarList)
            //str "|||||||||||||||||||||||||"
            //str (string model.SearchResultList)
            //pathfinderCoreShowModule model dispatch
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
