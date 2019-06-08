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
open PathfinderAttackSimulator

open ClientAuxFunctions
open ClientWebElements


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
    WeaponArray : Weapon []
    ModificationArray : AttackModification []
    IDCounter : int
    }


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
        WeaponArray = exmpWeaponArr
        ModificationArray = CompleteModificationArr
        // will count one up for each tab created
        IDCounter = 0
        }

    initialModel, Cmd.none

// The update function computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// these commands in turn, can dispatch messages to which the update function will react.
let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    match currentModel, msg with
    | _, AddIDCounterTesting ->
        let nextModel = {
            currentModel with
                IDCounter = currentModel.IDCounter + 10 }
        nextModel,Cmd.none
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
             | 2 -> ((searchForWeapons currentModel.WeaponArray searchInput) |> Array.map (fun x -> createSubSearchResult x.Name x.Description) )
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
            currentModel.CharacterArray
            |> Array.tryFind (fun x -> x.CharacterName = str)
            |> fun x -> if x.IsNone then failwith "Error 005" else x.Value
        let searchWeaponArr (str:string)=
            currentModel.WeaponArray
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
    | _, DeleteSearchResultFromActiveArray (intForWhichTab,searchForString) ->
        let rmCharFromArr (str:string)=
            let newActiveArray =
                currentModel.CharacterArray
                |> Array.filter (fun x -> x.CharacterName <> str)
            let newActiveModifierList =
                currentModel.ActiveModifierList
                |> List.map (fun (x,activeMod) -> let replacedChar = {
                                                    activeMod with
                                                        ActiveCharacter = EmptyChar }
                                                  if activeMod.ActiveCharacter.CharacterName = str
                                                  then x,replacedChar
                                                  else x,activeMod 
                            )
            let newSearchResult =
                currentModel.SearchResultList
                |> List.map (fun (id,result) -> let newSearchResult = {
                                                        result with
                                                            SearchResultChar = Array.filter (fun x -> x.ResultName <> str) result.SearchResultChar
                                                        }
                                                id,newSearchResult
                            )
            newActiveArray,newActiveModifierList,newSearchResult
        let rmWeaponFromArr (str:string)=
            let newActiveArray =
                currentModel.WeaponArray
                |> Array.filter (fun x -> x.Name <> str)
            let newActiveModifierList =
                currentModel.ActiveModifierList
                |> List.map (fun (x,activeMod) -> let rmWeap = {
                                                    activeMod with
                                                        ActiveWeapons = List.filter (fun x -> x.Name <> str) activeMod.ActiveWeapons }
                                                  x,rmWeap
                            )
            let newSearchResult =
                currentModel.SearchResultList
                |> List.map (fun (id,result) -> let newSearchResult = {
                                                    result with
                                                        SearchResultWeapons = Array.filter (fun x -> x.ResultName <> str) result.SearchResultWeapons }
                                                id,newSearchResult
                            )
            newActiveArray,newActiveModifierList,newSearchResult
        let rmModificationFromArr (str:string)=
            let newActiveArray =
                CompleteModificationArr
                |> Array.filter (fun x -> x.Name <> str)
            let newActiveModifierList =
                currentModel.ActiveModifierList
                |> List.map (fun (x,activeMod) -> let rmModification = {
                                                    activeMod with
                                                        ActiveModifications = List.filter (fun x -> x.Name <> str) activeMod.ActiveModifications }
                                                  x,rmModification
                            )
            let newSearchResult =
                currentModel.SearchResultList
                |> List.map (fun (id,result) -> let newSearchResult = {
                                                    result with
                                                        SearchResultModifications = Array.filter (fun x -> x.ResultName <> str) result.SearchResultModifications }
                                                id,newSearchResult
                            )
            newActiveArray,newActiveModifierList,newSearchResult

        let nextModel =
            match intForWhichTab with
            | 1 -> rmCharFromArr searchForString
                   |> fun (charArr,activeModList,searchResultList) -> {currentModel with
                                                                        CharacterArray = charArr
                                                                        ActiveModifierList = activeModList
                                                                        SearchResultList = searchResultList}
            | 2 -> rmWeaponFromArr searchForString
                   |> fun (weapArr,activeModList,searchResultList) -> {currentModel with
                                                                               WeaponArray = weapArr
                                                                               ActiveModifierList = activeModList
                                                                               SearchResultList = searchResultList}
            | 3 -> rmModificationFromArr searchForString
                   |> fun (modArr,activeModList,searchResultList) -> {currentModel with
                                                                        ModificationArray = modArr
                                                                        ActiveModifierList = activeModList
                                                                        SearchResultList = searchResultList}
            | _ -> failwith "Error 010"
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
                ModalInputList = []
            }
        nextModel, Cmd.none
    | _, UpdateModalInputList (modalID,(orderID,input)) ->
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
    | _, AddModalInputToCharacterArray ->
        let (modalIDOfInterest,modalInputOfInterest) =
            currentModel.ModalInputList
            |> List.tryFind (fun (x,y) -> x = "addCharacter")
            |> fun x -> if x.IsSome then x.Value else failwith "Not all input fields are filled for character creation!"
        let characterStat =
            modalInputOfInterest
            |> fun x -> if x.Length <> 9 then failwith "Not all input fields are filled for character creation!" else x
            |> Array.sortBy fst
            |> Array.map snd
            |> fun x -> createCharacterStats x.[0] (int x.[1]) (int x.[2]) (int x.[3]) (int x.[4]) (int x.[5]) (int x.[6]) (int x.[7]) 0 0 x.[8]
        let newModel = {
            currentModel with
                CharacterArray = Array.append currentModel.CharacterArray [|characterStat|]
                Modal = hiddenModal
            }
        newModel, Cmd.none
    | _, AddModalInputToWeaponArray ->
        let (modalIDOfInterest,modalInputOfInterest) =
            currentModel.ModalInputList
            |> List.tryFind (fun (x,y) -> x = "addWeapon")
            |> fun x -> if x.IsSome then x.Value else failwith "Not all input fields are filled for character creation!"
        let newWeapon =
            modalInputOfInterest
            |> fun x -> if x.Length <> 21 then failwith "Not all input fields are filled for weapon creation!" else x
            |> Array.sortBy fst
            |> Array.map snd
            |> fun x -> createWeapon x.[0]
                                    (int x.[1]) (int x.[2]) (matchStringToDamageType x.[3])
                                    (int x.[4])
                                    (int x.[5]) (int x.[6]) (matchStringToDamageType x.[7])
                                    (int x.[8]) (int x.[9]) (matchStringToDamageType x.[10])
                                    (int x.[11])
                                    (int x.[12]) (int x.[13]) (int x.[14])
                                    (matchStringToAbilityScore x.[15])
                                    (matchStringToAbilityScore x.[16])
                                    (matchStringToHandling x.[17]) (float x.[18])
                                    (matchStringToNaturalOrManufactured x.[19])
                                    x.[20]
        let newModel = {
            currentModel with
                WeaponArray = Array.append currentModel.WeaponArray [|newWeapon|]
                Modal = hiddenModal
                ModalInputList = []
            }
        newModel, Cmd.none

    //| _ -> currentModel, Cmd.none
              

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
            Hero.hero []
                      [ Container.container [ Container.IsFluid ]
                                            [ Navbar.navbar [ ] 
                                                        [ Level.level [ ]
                                                                      //creaters button, which creates the attack calculator card and adds empty spaces to all models related to this card.
                                                                      [ Button.button [ Button.Color IsWhite
                                                                                        Button.OnClick (fun _ -> dispatch (AddTabToTabList (attackCalculatorCard dispatch model.IDCounter)
                                                                                                                                           )
                                                                                                                          )
                                                                                      ]
                                                                                      [ Icon.icon [ ] [ i [ ClassName "fas fa-dungeon" ] [ ] ]
                                                                                        span [ ] [ str "Open new Attack Calculator tab" ]
                                                                                      ]
                                                                      ]
                                                        ]
                                              // contains all above mentioned cards
                                              Content.content [ ]
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
                      ]
            //Button.button [ Button.Props [ Tooltip.dataTooltip "click here to add new entry" ]
            //                Button.CustomClass (Tooltip.ClassName + " " + Tooltip.IsTooltipBottom)
            //                Button.OnClick (fun _ -> dispatch (test dispatch))
            //              ]
            //              [ Icon.icon [ Icon.Size IsSmall ]
            //                          [ i [ClassName "fas fa-plus-circle"] [] ] ]
            //str (string model.IDCounter)

            footer [ ClassName "footer" ]
                   [ footerContainer ]
            model.Modal
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

