module Client


open System
open Elmish
open Elmish.React
open Fable.FontAwesome
open Fable.FontAwesome.Free
open Fable.React
open Fable.React.Props
open Fable.Core.JsInterop
open Fetch.Types
open Thoth.Fetch
open Fulma
open Thoth.Json

open Shared

open Browser
open Fulma.Extensions.Wikiki


open PathfinderAttackSimulator.Library
open AuxLibFunctions
open Modifications
open Server
open PathfinderAttackSimulator.StandardAttackAction
open PathfinderAttackSimulator.FullRoundAttackAction
open Browser.Types
open Fulma
open Fulma

let exmpCharArr = [|Characters.myParrn; Characters.myTumor; Characters.myElemental;|] |> Array.sort
let exmpWeaponArr = [|Weapons.bite;Weapons.butchersAxe;Weapons.claw; Weapons.greatswordParrn; Weapons.enchantedLongswordElemental; Weapons.glaiveGuisarmePlus1FlamingBurst; Weapons.mwkRapier; Weapons.mwkLongbow
                      Weapons.slamElemental |] |> Array.sort
let ModificationArr = [|Charging; DivineFavor; Wrath; Multiattack; Flanking; Haste; FlurryOfBlows; TwoWeaponFighting; TwoWeaponFightingImproved; MutagenStrength; Invisibility; BlessingOfFervorAttackBonus
                        Shaken;WeaponFocus;EnlargePerson; MutagenStrength;WeaponSpecialization;Fatigued;AidAnother;VitalStrike;VitalStrikeImproved;VitalStrikeGreater;BlessingOfFervorAttackBonus |] |> Array.sort
let VarModificationArr = [|PowerAttack;SneakAttack;SneakAttackOnce;PlanarFocusFire|] |> Array.sortBy (fun x -> (x 0).Name)
let CompleteModificationArr = [|
                                Multiattack;SneakAttackOnce 0;TwoWeaponFighting;TwoWeaponFightingImproved;Haste;FlurryOfBlows;Shaken;WeaponFocus;EnlargePerson;MutagenStrength;
                                Invisibility;PlanarFocusFire 0;SneakAttack 0;Wrath;DivineFavor;FuriousFocus 0;PowerAttack 0;Flanking;Charging;WeaponSpecialization;Fatigued;
                                AidAnother;VitalStrike;VitalStrikeImproved;VitalStrikeGreater;InspireCourage 0; ShockingGrasp 0 true; ShockingGraspIntensifiedEmpowered 0 true; PowerAttackURL OffHand 0;
                                BlessingOfFervorAttackBonus; BonusAttackDamage 0 0;
                              |] |> Array.sortBy (fun x -> x.Name)

let hiddenModal = Modal.modal [Modal.IsActive false ] []
/// function for similarity measurements
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

type SubSearchResult = {
    ResultName          : string
    ResultDescription   : string
    }

type SearchResult = {
    SearchResultChar          : SubSearchResult []
    SearchResultWeapons       : SubSearchResult []
    SearchResultModifications : SubSearchResult []
    }

let createSubSearchResult resultName resultDesc= {
    ResultName          = resultName
    ResultDescription   = resultDesc
    }

let createSearchResult searchResultChar  searchResultWea searchResultModi = {
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
    TabList: (int * (SearchResult -> string [] -> ActiveModifiers -> ReactElement)) list
    SearchBarList: (int*SubTabsSearchBars) list
    SearchResultList : (int*SearchResult) list
    ActiveModifierList : (int*ActiveModifiers) list
    CalculationResult : (int*string []) list
    Modal : ReactElement
    ModalInputList : (string * (int * string) []) list
    CharacterArray : CharacterStats []
    IDCounter : int
    }

// The Msg type defines what events/actions can occur while the application is running
// the state of the application changes *only* in reaction to these events
type Msg =
| CalculateStandardAttackAction of int
| AddTabToTabList of (SearchResult -> string [] -> ActiveModifiers -> ReactElement)
| UpdateSearchBarList of int * int * string
| UpdateSearchResultList of int * int
| UpdateActiveModifierList of int * int * string
| UpdateActiveModifierListOnlySize of int*string
| ResetActiveWeapon of int
| ResetActiveModifications of int
| CloseTab of int
| ActivateModal of ReactElement
| CloseModal
| UpdateModalInputList of string * (int*string)

let createInsertButton id intForWhichTab (searchForName:string) (dispatch : Msg -> unit) =
    Button.button [ Button.Props [ Props.Id (sprintf "Button%i%i" id intForWhichTab) ]
                    Button.OnClick (fun _ -> dispatch (UpdateActiveModifierList (id,intForWhichTab,searchForName)
                                                      )
                                   )
                    Button.Color IsSuccess; Button.IsInverted
                  ]
                  [ str (sprintf "add %s" searchForName)]


// defines the initial state and initial command (= side-effect) of the application
let init () : Model * Cmd<Msg> =
    let initialModel = { 
        //ReactElements with AttackCalculator function, all with id
        TabList = []
        //SearchBar input from TabList, all with id
        SearchBarList = []
        //Results after searching library for input in SearchBar, all with id
        SearchResultList = []
        //Selected modifiers which will be used for attack calculator, all with id
        ActiveModifierList = []
        //Calculation results, all with id
        CalculationResult = []
        // Saves all active states and ids for modals 
        Modal = hiddenModal
        // List for all modal input arrays (for character, weapon, modification creation)
        ModalInputList = []
        CharacterArray = exmpCharArr
        // will count one up for each tab created
        IDCounter = 0
        }

    initialModel, Cmd.none

// The update function computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// these commands in turn, can dispatch messages to which the update function will react.
let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    match currentModel, msg with
    | _, AddTabToTabList (tab)->
        let nextModel = {
            currentModel with
                TabList = (currentModel.IDCounter,tab)::currentModel.TabList
                IDCounter = currentModel.IDCounter+1
                SearchBarList = (currentModel.IDCounter,createSubTabsSearchBars "" "" "")::currentModel.SearchBarList
                SearchResultList = (currentModel.IDCounter,createSearchResult [||] [||] [||])::currentModel.SearchResultList
                ActiveModifierList = (currentModel.IDCounter, createActiveModifiers EmptyChar Medium [EmptyWeapon] [EmptyModification])::currentModel.ActiveModifierList
                //ActiveModifierList = (currentModel.IDCounter, createActiveModifiers Characters.myParrn Medium [Weapons.greatswordParrn] [Flanking])::currentModel.ActiveModifierList
                CalculationResult = (currentModel.IDCounter, [||])::currentModel.CalculationResult
            }
        nextModel,Cmd.none
    | _, CloseTab (id) ->
        let nextModel = {
            currentModel with
                TabList = List.filter (fun (index,x) -> index <> id) currentModel.TabList
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
            ///defines searchResult
         let filter =
             match intForWhichTab with
             | 1 -> ((searchForCharacters currentModel.CharacterArray searchInput) |> Array.map (fun x -> createSubSearchResult x.CharacterName x.CharacterDescription) )
             | 2 -> ((searchForWeapons exmpWeaponArr searchInput) |> Array.map (fun x -> createSubSearchResult x.Name x.Description) )
             | 3 -> ((searchForModifications ModificationArr searchInput) |> Array.map (fun x -> createSubSearchResult x.Name x.Description) )
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
         let nextModel = {
            currentModel with
                SearchResultList = newSearchResultList
         }
         nextModel,Cmd.none
    | _, UpdateActiveModifierList (id,intForWhichTab,searchForString) ->
        let filterFillerWeapon (weapList:Weapon list) =
            List.filter (fun (x:Weapon) -> x.Name <> "Here will be your weapons" ) weapList
        let filterFillerModi (modiList:AttackModification list) =
            List.filter (fun (x:AttackModification) -> x.Name <> "Here will be your modifications" ) modiList
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
                   |> fun x -> createActiveModifiers x activeModifierMatchedID.ActiveSize activeModifierMatchedID.ActiveWeapons activeModifierMatchedID.ActiveModifications
            | 2 -> (searchWeaponArr searchForString)
                   |> fun weap -> createActiveModifiers activeModifierMatchedID.ActiveCharacter activeModifierMatchedID.ActiveSize (weap::(filterFillerWeapon activeModifierMatchedID.ActiveWeapons)) activeModifierMatchedID.ActiveModifications
            | 3 -> (searchModificationArr searchForString)
                   |> fun attackModi -> createActiveModifiers activeModifierMatchedID.ActiveCharacter activeModifierMatchedID.ActiveSize activeModifierMatchedID.ActiveWeapons (attackModi::(filterFillerModi activeModifierMatchedID.ActiveModifications))
            | _ -> failwith "Error 008"
        let nextModel = {
            currentModel with
                ActiveModifierList = (id,updatedModifiers)::(List.except [currentModel.ActiveModifierList.Item (List.tryFindIndex (fun (i,y) -> i = id) currentModel.ActiveModifierList).Value
                                                                         ] currentModel.ActiveModifierList)
            }
        nextModel,Cmd.none
    | _, UpdateActiveModifierListOnlySize (id,sizeString) ->
        let activeModifierMatchedID = List.tryFind (fun (index,activeModi) -> index = id) currentModel.ActiveModifierList
                                      |> fun x -> snd x.Value
        let updatedSize = match sizeString with
                          | "Fine"         -> Fine
                          | "Diminuitive"  -> Diminuitive
                          | "Tiny"         -> Tiny
                          | "Small"        -> Small
                          | "Medium"       -> Medium
                          | "Large"        -> Large
                          | "Huge"         -> Huge
                          | "Gargantuan"   -> Gargantuan
                          | "Colossal"     -> Colossal
                          | _              -> failwith "Error 009"
        let updatedModifiers =
            createActiveModifiers activeModifierMatchedID.ActiveCharacter updatedSize activeModifierMatchedID.ActiveWeapons activeModifierMatchedID.ActiveModifications
        let nextModel = { 
            currentModel with
                ActiveModifierList = (id,updatedModifiers)::(List.except [currentModel.ActiveModifierList.Item (List.tryFindIndex (fun (i,y) -> i = id) currentModel.ActiveModifierList).Value
                                                                         ] currentModel.ActiveModifierList)
            }
        nextModel, Cmd.none
    | _, CalculateStandardAttackAction (id) ->
        let activeModifierMatchedID = List.tryFind (fun (index,activeModi) -> index = id) currentModel.ActiveModifierList
                                      |> fun x -> snd x.Value
        let CalculationResultsMatchedID = List.tryFind (fun (index,activeModi) -> index = id) currentModel.CalculationResult
                                          |> fun x -> snd x.Value
        let result = myStandardAttack activeModifierMatchedID.ActiveCharacter
                                      activeModifierMatchedID.ActiveSize
                                      activeModifierMatchedID.ActiveWeapons.Head
                                      (Array.ofList activeModifierMatchedID.ActiveModifications)
                     |> fun x -> "> " + x
        let outputFinalized = CalculationResultsMatchedID
                              |> Array.append [|result|]
                              |> fun x -> if x.Length > 5 then x.[..4] else x
        let nextModel = {
            currentModel with
                CalculationResult = (id,outputFinalized)::currentModel.CalculationResult
            }
        nextModel, Cmd.none
    | _, ResetActiveWeapon (id) ->
        let activeModifierMatchedID = List.tryFind (fun (index,activeModi) -> index = id) currentModel.ActiveModifierList
                                      |> fun x -> snd x.Value
        let resetActiveModifers = {
            activeModifierMatchedID with
                ActiveWeapons = [EmptyWeapon]
            }
        let nextModel = {
            currentModel with
                ActiveModifierList = (id,resetActiveModifers)::(List.except [currentModel.ActiveModifierList.Item (List.tryFindIndex (fun (i,y) -> i = id) currentModel.ActiveModifierList).Value
                                                                            ] currentModel.ActiveModifierList)
            }
        nextModel, Cmd.none
    | _, ResetActiveModifications (id) ->
        let activeModifierMatchedID = List.tryFind (fun (index,activeModi) -> index = id) currentModel.ActiveModifierList
                                      |> fun x -> snd x.Value
        let resetActiveModifers = {
            activeModifierMatchedID with
                ActiveModifications = [EmptyModification]
            }
        let nextModel = {
            currentModel with
                ActiveModifierList = (id,resetActiveModifers)::(List.except [currentModel.ActiveModifierList.Item (List.tryFindIndex (fun (i,y) -> i = id) currentModel.ActiveModifierList).Value
                                                                            ] currentModel.ActiveModifierList)
            }
        nextModel, Cmd.none
    | _, ActivateModal (modal) ->
        let toggledModal = if modal = currentModel.Modal then hiddenModal else modal
        let nextModel = {
            currentModel with
                Modal = toggledModal
            }
        nextModel, Cmd.none
    | _, CloseModal ->
        let nextModel = {
            currentModel with
                Modal = hiddenModal
            }
        nextModel, Cmd.none
    | _ , UpdateModalInputList (modalID,(orderID,input)) ->
        let (modalIDOfInterest,modalInputOfInterest) = List.tryFind (fun (id,activeValues) -> id = modalID) currentModel.ModalInputList
                                                        |> fun x -> if x.IsSome then x.Value else modalID,[||]
        let newInputArr =
            modalInputOfInterest
            |> Array.filter (fun (id,value) -> id <> orderID)
            |> Array.append [|orderID,input|]
        let newModel = {
            currentModel with
                ModalInputList = (modalIDOfInterest,newInputArr)::(List.except [(modalIDOfInterest,modalInputOfInterest)] currentModel.ModalInputList)
            }
        newModel, Cmd.none  
    //| _ -> currentModel, Cmd.none

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
            [ Navbar.Item.a [ Navbar.Item.Props [ Href "https://freymaurer.github.io/PathfinderAttackSimulator/"] ]
                            [ str "Documentation" ]
            ]
        ]

let doHideShow (tabId:string) (cardID:int) =
    let x = Dom.document.getElementById(sprintf "%A%i" tabId cardID)
    if x?style?display = "none"
    then x?style?display <- "block"
    else x?style?display <- "none"

let doHide (tabId:string) (cardID:int) =
    let x = Dom.document.getElementById(sprintf "%A%i" tabId cardID)
    x?style?display <- "none"

let searchBarTab (dispatch : Msg -> unit) (id:int) (tabCategory:string) (specificSearchResults:SubSearchResult []) =
    let getIntForTabCategory =
        match tabCategory with
        | "characters" -> 1
        | "weapons" -> 2
        | "modifications" -> 3
        | _ -> failwith "unknown case, you should not get this 003"
    let searchResultElement =
        specificSearchResults
        |> Array.map (fun subSearch -> subSearch,createInsertButton id getIntForTabCategory subSearch.ResultName dispatch)
        |> Array.map (fun (subSearch,button) -> tr [ ]
                                                   [ th [ ] [ str (sprintf "%s" subSearch.ResultName) ]
                                                     th [ ]
                                                        [ Text.span [ Modifiers [Modifier.TextSize (Screen.All,TextSize.Is7) ] ]
                                                                    [ str (sprintf "%s" subSearch.ResultDescription) ] 
                                                        ]
                                                     th [ ] [ button ]
                                                   ]
                     )

    Content.content [ Content.Props [ Props.Id (sprintf "Tab%s%i" tabCategory id); Props.Style [CSSProp.Display DisplayOptions.None]
                                    ]
                    ]
                    [ Columns.columns [ ]
                                      [ Column.column [ Column.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                                                      [ Heading.h6 [ ]
                                                                   [ str "Searchbar"] 
                                                        Level.item [ ]
                                                                   [ Field.div [ Field.HasAddons ]
                                                                               [ Control.div [ ]
                                                                                             [Button.button [ Button.Props [ Tooltip.dataTooltip "click here to add new entry" ]
                                                                                                              Button.CustomClass (Tooltip.ClassName + " " + Tooltip.IsTooltipBottom)
                                                                                                              Button.OnClick (fun _ -> let getIntForTabCategory =
                                                                                                                                           match tabCategory with
                                                                                                                                           | "characters" -> 1
                                                                                                                                           | "weapons" -> 2
                                                                                                                                           | "modifications" -> 3
                                                                                                                                           | _ -> failwith "unknown case, you should not get this 003"
                                                                                                                                       dispatch (UpdateSearchResultList (id, getIntForTabCategory))

                                                                                                                              )
                                                                                                            ]
                                                                                                            [ Icon.icon [ Icon.Size IsSmall ]
                                                                                                                        [ i [ClassName "fas fa-plus-circle"] [] ] ]
                                                                                             ]
                                                                                 Control.div [ ]
                                                                                             [ Input.text [ Input.Placeholder (sprintf "Search %s" tabCategory)
                                                                                                            Input.OnChange (fun e -> let x = !!e.target?value
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
                                                                      Table.IsFullWidth]
                                                                    [ thead [ ]
                                                                            [ tr [ ]
                                                                                 [ th [ ] [ str "Name" ]
                                                                                   th [ ] [ str "Description" ]
                                                                                   th [ ] [ str "add" ] ] ]
                                                                      tbody [ Props.Id (sprintf "SearchResultTable%s%i" tabCategory id) ]
                                                                            /// module to display SearchResults
                                                                            searchResultElement
                                                                    ]
                                                      ]
                                      ]
                    ]


let attackCalculatorCard (dispatch : Msg -> unit) (id:int) (searchResult:SearchResult) (specificCalculationResults:string []) (relatedActiveModifier:ActiveModifiers) =
    let specificCalculationResultsFinalized =
        (if Array.isEmpty specificCalculationResults
        then [|"Here will be your result! Try it out!"|]
        else specificCalculationResults)
        |> Array.collect (fun x -> [|(str x); br []|])
        |> List.ofArray
    let stringOfActiveWeaponNames =
        relatedActiveModifier.ActiveWeapons
        |> List.fold (fun arr ele -> ele.Name + ", " + arr) ""
        |> fun x -> x.Trim([|',';' '|])
    let stringOfActiveModifications =
        relatedActiveModifier.ActiveModifications
        |> List.fold (fun arr ele -> ele.Name + ", " + arr) ""
        |> fun x -> x.Trim([|',';' '|])
    let dropdownButtonSize (sizeStr:string) =
        Dropdown.Item.a [ Dropdown.Item.Props [ Props.OnClick (fun _ -> dispatch (UpdateActiveModifierListOnlySize (id,sizeStr))) ]
                             ]
                 [ str sizeStr]
    Card.card [  ]
        [ Card.header [ Modifiers [ Modifier.BackgroundColor IsGreyLighter] ]
            [ Card.Header.title [ Card.Header.Title.IsCentered
                                  Card.Header.Title.CustomClass "BiggerSize" //does not work either
                                ]
                                [ str "Attack Calculator" ]
              Card.Header.icon [ Modifiers [Modifier.TextAlignment (Screen.All,TextAlignment.Right)] ]
                               [ Button.button [ Button.Color IsWhite
                                                 Button.OnClick (fun _ -> doHideShow "TabMainInfo" id
                                                                          doHideShow "TabMainOutput" id
                                                                          doHide "Tabweapons" id
                                                                          doHide "Tabcharacters" id
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
            ]
          Card.content [ Props [ Props.Id (sprintf "TabMainOutput%A" id); Props.Style [CSSProp.Display DisplayOptions.None] ]
                          ]
                       [ Container.container [ Container.IsFluid
                                               Container.Modifiers [ Modifier.TextSize (Screen.All,TextSize.Is6)] ]
                                             [ h4 [ Props.Id (sprintf "OutputCharacter%A" id)]
                                                  [ str relatedActiveModifier.ActiveCharacter.CharacterName]
                                               p [ Props.Id (sprintf "OutputSize%A" id) ]
                                                 [ str (string relatedActiveModifier.ActiveSize) ]
                                               p [ Props.Id (sprintf "OutputWeapons%A" id) ]
                                                 [ Level.level [ ]
                                                               [ str stringOfActiveWeaponNames
                                                                 Button.button [ Button.IsOutlined
                                                                                 Button.OnClick (fun _ -> dispatch (ResetActiveWeapon id))
                                                                                 Button.Props [ Tooltip.dataTooltip "click here to reset all selected weapons" ]
                                                                                 Button.CustomClass (Tooltip.ClassName + " " + Tooltip.IsTooltipLeft)
                                                                                 Button.Modifiers [ Modifier.TextSize (Screen.All,TextSize.Is7) ]
                                                                               ]
                                                                               [ str "reset weapons" ]
                                                               ]
                                                 ]
                                               p [ Props.Id (sprintf "OutputModifications%A" id) ]
                                                 [ Level.level [ ]
                                                               [ str stringOfActiveModifications
                                                                 Button.button [ Button.IsOutlined
                                                                                 Button.OnClick (fun _ -> dispatch (ResetActiveModifications id))
                                                                                 Button.Props [ Tooltip.dataTooltip "click here to reset all selected modifications" ]
                                                                                 Button.CustomClass (Tooltip.ClassName + " " + Tooltip.IsTooltipLeft)
                                                                                 Button.Modifiers [ Modifier.TextSize (Screen.All,TextSize.Is7) ]
                                                                               ]
                                                                               [ str "reset weapons" ]
                                                               ]
                                                 ] 
                                             ]
                       ]
          Card.content [ Props [ Props.Id (sprintf "TabMainInfo%A" id); Props.Style [CSSProp.Display DisplayOptions.None] ] ]
                       [ Level.level [ ]
                                     [ Level.item []
                                                  [ Button.button [ Button.Color IsInfo
                                                                    Button.IsInverted
                                                                    Button.OnClick (fun _ -> dispatch (CalculateStandardAttackAction id)
                                                                                   )
                                                                  ]
                                                                  [ Icon.icon [ Icon.Size IsSmall ]
                                                                              [ i [ClassName "fas fa-dice-six"] [] ]
                                                                    span [] [str "Calculate Standard Attack"
                                                                                ]
                                                                  ] 
                                                  ]
                                       //Level.item []
                                       //           [ str "Test2" ]
                                     ]
                         Notification.notification [ Notification.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Left)] ]
                                                   specificCalculationResultsFinalized
                         
                         Level.level [ ]
                                     [ Level.item [ Level.Item.HasTextCentered ]
                                                  [ div [ ]
                                                        [ Button.button [ Button.Color IsInfo
                                                                          Button.IsInverted
                                                                          Button.OnClick (fun _ -> doHideShow "Tabcharacters" id
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
                                                  [ Dropdown.dropdown [ Dropdown.IsHoverable
                                                                        Dropdown.IsUp ]
                                                                      [ div [ ]
                                                                            [ Button.button [ Button.Color IsInfo
                                                                                              Button.IsInverted ]
                                                                                            [ span [] [ str "Size" ]
                                                                                              Icon.icon [ ] [ i [ ClassName "fa fa-angle-down" ] [ ] ]
                                                                                            ]
                                                                            ]
                                                                        Dropdown.menu [ ]
                                                                          [ Dropdown.content [ ]
                                                                              [ dropdownButtonSize "Fine"
                                                                                dropdownButtonSize "Diminuitive"
                                                                                dropdownButtonSize "Tiny"
                                                                                dropdownButtonSize "Small"
                                                                                Dropdown.divider [ ]
                                                                                dropdownButtonSize "Medium"
                                                                                Dropdown.divider [ ]
                                                                                dropdownButtonSize "Large"      
                                                                                dropdownButtonSize "Huge"       
                                                                                dropdownButtonSize "Gargantuan" 
                                                                                dropdownButtonSize "Colossal"   
                                                                              ]
                                                                          ]
                                                                      ]
                                                  ]
                                       Level.item [ Level.Item.HasTextCentered ]
                                         [ div [ ]
                                               [ Button.button [ Button.Color IsInfo
                                                                 Button.IsInverted
                                                                 Button.OnClick (fun _ -> doHideShow "Tabweapons" id
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
          searchBarTab dispatch id "characters" searchResult.SearchResultChar
          searchBarTab dispatch id "weapons" searchResult.SearchResultWeapons
          searchBarTab dispatch id "modifications" searchResult.SearchResultModifications
          Card.Footer.div [Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered)
                                       Modifier.TextWeight TextWeight.Bold
                                       Modifier.BackgroundColor IsGreyLighter]
                          ] 
                          [ str relatedActiveModifier.ActiveCharacter.CharacterName ]          
        ]
// single input panels used for addModals
let inputPanel description placeholder inputID (dispatch : Msg -> unit) =
    Level.level [ ]
                [ Level.left [ ]
                             [ Level.item [] [str description] ]
                  Level.right [ ]
                              [ Control.div [ ]
                                            [ Input.text [ Input.Size IsSmall 
                                                           Input.Placeholder placeholder
                                                           Input.OnChange (fun e -> let x = !!e.target?value
                                                                                    dispatch (UpdateModalInputList ("addCharacter",(inputID,x))
                                                                                             )
                                                                          )
                                                         ]
                                              
                                            ]
                              ]
                ]
// content for add character modal
let addCharacterFormat (dispatch : Msg -> unit) =
    Panel.panel [ ]
                [ inputPanel "Character Name:" ".. best character name" 1 dispatch
                  inputPanel "Base Attack Bonus:"  ".. e.g. 4" 2 dispatch
                  inputPanel "Strength:" ".. ability score, e.g. 18" 3 dispatch
                  inputPanel "Dexterity:" ".. ability score, e.g. 18" 4 dispatch
                  inputPanel "Constitution:" ".. ability score, e.g. 18" 5 dispatch
                  inputPanel "Intelligence:" ".. ability score, e.g. 18" 6 dispatch
                  inputPanel "Wisdom:" ".. ability score, e.g. 18" 7 dispatch
                  inputPanel "Charisma:" ".. ability score, e.g. 18" 8 dispatch
                  inputPanel "Character Description:" ".. description" 9 dispatch]  

// add character modal
let addCharacterModal closeDisplay (dispatch : Msg -> unit)=
    Modal.modal [ Modal.IsActive true
                ]
        [ Modal.background [ Props [ OnClick closeDisplay ] ] [ ]
          Modal.Card.card [ ]
            [ Modal.Card.head [ ]
                [ Modal.Card.title [ ]
                    [ str "Character Creator" ]
                  Delete.delete [ Delete.OnClick closeDisplay ] [ ] ]
              Modal.Card.body [ ]
                              [ addCharacterFormat dispatch]
              Modal.Card.foot [ ]
                [ Button.button [ Button.Color IsSuccess ]
                                [ str "Add Character" ]
                  Button.button [ Button.OnClick closeDisplay ]
                                [ str "Cancel" ] ] ] ]

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
                Hero.IsHalfHeight
                Hero.IsBold ]
              [ Hero.head [ ]
                  [ Navbar.navbar [ ]
                      [ Container.container [ ]
                          [ navBrand
                            navMenu ] ] ]
                Hero.body [ ]
                  [ Container.container [ Container.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                      [ img [ Src "https://steemitimages.com/DQmTFWdqNAFhbxQjJ9HZXKx7BkQx52kpRorsnJxkdD1wagZ/Pathfinder.png"
                              Alt "Logo" ]
                        Heading.p [ ]
                          [ str "Pathfinder Attack Calculator" ]
                        Heading.p [ Heading.IsSubtitle ]
                            [ safeComponents ] ] ]
              ]
            Container.container [ Container.IsFluid ]
                                [ Heading.p [ ]
                                            [ Level.level [ ]
                                                          //creaters button, which creates the attack calculator card and adds empty spaces to all models related to this card.
                                                          [ Button.button [ Button.Color IsWhite
                                                                            Button.OnClick (fun _ -> dispatch (AddTabToTabList (attackCalculatorCard dispatch model.IDCounter)
                                                                                                                               )
                                                                                                              )
                                                                          ]
                                                                          [ str "Open new Attack Calculator tab"]
                                                            Button.button [ ]
                                                                          [str "Add custom Character (soon)"]
                                                            Button.button []
                                                                          [str "Add custom Weapon (soon)"]

                                                          ]
                                            ]
                                  // contains all above mentioned cards
                                  Content.content []
                                                  [ Content.content [ ]
                                                                    (   (List.sortByDescending fst model.TabList)
                                                                        |> List.map (fun (indexTab,tab) -> let relatedSearchResults = snd (List.find (fun (index,searchResults) -> index = indexTab) model.SearchResultList)
                                                                                                           let relatedCalculationResults = snd (List.find (fun (index,searchResults) -> index = indexTab) model.CalculationResult)
                                                                                                           let relatedActiveModifier = snd (List.find (fun (index,searchResults) -> index = indexTab) model.ActiveModifierList)
                                                                                                           tab relatedSearchResults relatedCalculationResults relatedActiveModifier
                                                                                    ) 
                                                                    )   
                                                  ]              
                                ]
            // Control elment of the modal UI
            Card.card [ ]
                      [ div [ ]
                            [ Button.button [ Button.OnClick (fun _ -> dispatch (ActivateModal (addCharacterModal (fun _ -> dispatch CloseModal) dispatch
                                                                                               )
                                                                                )
                                                             )
                                                             
                                            ]
                                            [ str "Show card modal" ] ]
                      ]
            model.Modal
            str (if List.isEmpty model.ModalInputList then "still empty" else (snd (snd (model.ModalInputList |> List.head)).[0]) )

            footer [ ClassName "footer" ]
                   [ footerContainer ]
        ]


#if DEBUG
open Elmish.Debug
open Elmish.HMR
open Browser.Types

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

