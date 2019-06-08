module ClientWebElements

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

open ClientAuxFunctions


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
    ActiveWeapons       : (int * Weapon) list
    ActiveModifications : AttackModification list
    }

let createActiveModifiers activeChar activeSize activeWeapons activeModi= {
    ActiveCharacter     = activeChar
    ActiveSize          = activeSize
    ActiveWeapons       = activeWeapons
    ActiveModifications = activeModi
    }

type TabActiveIdCounterCollector = {
    ModificationID : int
    WeaponID : int
    }

let createTabActiveIDcounter modID weapID = {
    ModificationID = modID
    WeaponID = weapID
    }

// The Msg type defines what events/actions can occur while the application is running
// the state of the application changes *only* in reaction to these events
type Msg =
| AddIDCounterTesting
| CalculateStandardAttackAction of int
| AddTabToTabList of (SearchResult -> string [] -> ActiveModifiers -> ReactElement)
| UpdateSearchBarList of int * int * string
| UpdateSearchResultList of int * int
| UpdateActiveModifierList of int * int * string
| DeleteSearchResultFromActiveArray of int * string
| UpdateActiveModifierListOnlySize of int * string
| ResetActiveWeapon of int
| ResetActiveModifications of int
| CloseTab of int
| ActivateModal of ReactElement
| CloseModal
| UpdateModalInputList of string * (int*string)
| AddModalInputToCharacterArray
| AddModalInputToWeaponArray

let createActivateSearchResultButton (searchForName:string) msg dispatch =
    Button.button [ Button.Props [ ]
                    Button.OnClick (fun _ -> dispatch msg
                                   )
                    Button.Color IsSuccess; Button.IsInverted
                  ]
                  [ str (sprintf "add %s" searchForName)]

let deleteSearchResultFromActiveArrayButton msg dispatch =
    Button.button [ Button.Props [ ]
                    Button.OnClick (fun _ -> dispatch msg
                                   )
                    Button.Color IsDanger; Button.IsInverted
                  ]
                  [ Icon.icon [ Icon.Size IsSmall ]
                              [ i [ClassName "fa fa-times-circle"] [] ]
                  ]

let [<Literal>] ENTER_KEY = 13.

let onEnter msg dispatch =
    OnKeyDown (fun ev ->
        if ev.keyCode = ENTER_KEY then
            dispatch msg)


///////////////////////////////////////////////////////////////////////// MODALs //////////////////////////////////////////////////////////////

/////////////////// Add Character Modal ///////////////////////////////////////////////////////////////////////////////////////////////////////

// single input panels used for addModals
let inputPanel modalID description placeholder inputID (dispatch : Msg -> unit) =
    Level.level [ ]
                [ Level.left [ ]
                             [ Level.item [] [str description] ]
                  Level.right [ ]
                              [ Control.div [ ]
                                            [ Input.text [ Input.Size IsSmall 
                                                           Input.Placeholder placeholder
                                                           Input.OnChange (fun e -> let x = !!e.target?value
                                                                                    dispatch (UpdateModalInputList (modalID,(inputID,x))
                                                                                             )
                                                                          )
                                                         ]
                                              
                                            ]
                              ]
                ]

// content for add character modal
let addCharacterModalContent (dispatch : Msg -> unit) =
    Panel.panel [ ]
                [ inputPanel "addCharacter" "Character Name:" ".. best character name" 1 dispatch
                  inputPanel "addCharacter" "Base Attack Bonus:"  ".. e.g. 4" 2 dispatch
                  inputPanel "addCharacter" "Strength:" ".. ability score, e.g. 18" 3 dispatch
                  inputPanel "addCharacter" "Dexterity:" ".. ability score, e.g. 18" 4 dispatch
                  inputPanel "addCharacter" "Constitution:" ".. ability score, e.g. 18" 5 dispatch
                  inputPanel "addCharacter" "Intelligence:" ".. ability score, e.g. 18" 6 dispatch
                  inputPanel "addCharacter" "Wisdom:" ".. ability score, e.g. 18" 7 dispatch
                  inputPanel "addCharacter" "Charisma:" ".. ability score, e.g. 18" 8 dispatch
                  inputPanel "addCharacter" "Character Description:" ".. description" 9 dispatch]


// add character modal
let addCharacterModal closeDisplay (dispatch : Msg -> unit)=
    Modal.modal [ Modal.IsActive true
                  Modal.Props [ (onEnter AddModalInputToCharacterArray dispatch) ]
                ]
        [ Modal.background [ Props [ OnClick closeDisplay ] ] [ ]
          Modal.Card.card [ ]
            [ Modal.Card.head [ ]
                [ Modal.Card.title [ ]
                    [ str "Character Creator" ]
                  Delete.delete [ Delete.OnClick closeDisplay ] [ ] ]
              Modal.Card.body [ ]
                              [ addCharacterModalContent dispatch]
              Modal.Card.foot [ ]
                [ Button.button [ Button.Color IsSuccess
                                  Button.OnClick (fun _ -> dispatch AddModalInputToCharacterArray
                                                  )
                                ]
                                [ str "Add Character" ]
                  Button.button [ Button.OnClick closeDisplay ]
                                [ str "Cancel" ] ] ] ]

let activateAddCharacterDispatch dispatch =
    ActivateModal (addCharacterModal (fun _ -> dispatch CloseModal) dispatch)

/////////////////////////////////////////////////// Show Active Character Modal ///////////////////////////////////////////////////////////////////////////
let charStats (char:CharacterStats) =

    let charStrings = [|char.CharacterName; string char.BAB; string char.Strength; string char.Dexterity; string char.Constitution;
                        string char.Intelligence; string char.Wisdom; string char.Charisma|]
    let descStrings = [|"CharacterName"; "BAB"; "Strength"; "Dexterity"; "Constitution"; "Intelligence"; "Wisdom"; "Charisma"|]
    let combinedArr = Array.zip descStrings charStrings

    Container.container [ Container.IsFluid ]
                        (combinedArr
                        |> Array.map (fun (desc,value) -> Panel.panel [ ]
                                                                      [ Level.level [ ]
                                                                                    [ Level.left [] [ str desc ]
                                                                                      Level.right [] [ str value ]
                                                                                    ] 
                                                                      ]
                                      )
                        |> List.ofArray
                        )


let displayActivechar char (dispatch : Msg -> unit)=
    Modal.modal [ Modal.IsActive true
                ]
        [ Modal.background [ Props [ OnClick (fun _ -> dispatch CloseModal) ] ] [ ]
          Modal.Card.card [ ]
            [ Modal.Card.head [ ]
                [ Modal.Card.title [ ]
                    [ str "Weapon Creator" ]
                  Delete.delete [ Delete.OnClick (fun _ -> dispatch CloseModal)] [ ] ]
              Modal.Card.body [ ]
                              [ charStats char ]
              Modal.Card.foot [ ]
                [ Button.button [ Button.OnClick (fun _ -> dispatch CloseModal) ]
                                [ str "Cancel" ] ] ] ]            

let activatedisplayActivechar char dispatch =
    ActivateModal (displayActivechar char dispatch)

/////////////////// Add Weapon Modal ////////////////////////////////////////////////////////////////////////////////////////////////////
// single input panels used for addModals
let inputPanelWeaponDmg modalID description placeholder placeholder2 inputID inputID2 inputID3 (dispatch : Msg -> unit) =
    Level.level [ ]
                [ Level.left [ ]
                             [ Level.item [] [str description] ]
                  Level.right [ ]
                              [ Level.level [ ]
                                            [ Input.text [ Input.Props [ Style [ CSSProp.Width "40px"] ]
                                                           Input.Size IsSmall 
                                                           Input.Placeholder placeholder
                                                           Input.OnChange (fun e -> let x = !!e.target?value
                                                                                    dispatch (UpdateModalInputList (modalID,(inputID,x))
                                                                                             )
                                                                          )
                                                         ]
                                              str "d"
                                              Input.text [ Input.Props [ Style [ CSSProp.Width "40px"] ]
                                                           Input.Size IsSmall 
                                                           Input.Placeholder placeholder2
                                                           Input.OnChange (fun e -> let x = !!e.target?value
                                                                                    dispatch (UpdateModalInputList (modalID,(inputID2,x))
                                                                                             )
                                                                          )
                                                         ]
                                              Select.select [ Select.Color IsWhite
                                                              Select.Size ISize.IsSmall
                                                              Select.IsInline]
                                                            [ select [ DefaultValue "Untyped" ]
                                                                [ option [ Props.OnClick (fun _ -> dispatch (UpdateModalInputList (modalID,(inputID3,"Untyped"))) )
                                                                           Value "Untyped" ] [ str "Untyped" ]
                                                                  option [ Props.Disabled true] [ str "___________________________" ]
                                                                  option [ Value "Bludgeoning"
                                                                           Props.OnClick (fun _ -> dispatch (UpdateModalInputList (modalID,(inputID3,"Bludgeoning"))) ) ] [ str "Bludgeoning" ]
                                                                  option [ Value "Piercing"
                                                                           Props.OnClick (fun _ -> dispatch (UpdateModalInputList (modalID,(inputID3,"Piercing"))) ) ] [ str "Piercing" ]
                                                                  option [ Value "Slashing"
                                                                           Props.OnClick (fun _ -> dispatch (UpdateModalInputList (modalID,(inputID3,"Slashing"))) ) ] [ str "Slashing" ]
                                                                  option [ Props.Disabled true] [ str "___________________________" ]
                                                                  option [ Value "Bludgeoning & Slashing"
                                                                           Props.OnClick (fun _ -> dispatch (UpdateModalInputList (modalID,(inputID3,"Bludgeoning & Slashing"))) ) ] [ str "Bludgeoning & Slashing" ]
                                                                  option [ Value "Bludgeoning & Piercing"
                                                                           Props.OnClick (fun _ -> dispatch (UpdateModalInputList (modalID,(inputID3,"Bludgeoning & Piercing"))) ) ] [ str "Bludgeoning & Piercing" ]
                                                                  option [ Value "Piercing & Slashing"
                                                                           Props.OnClick (fun _ -> dispatch (UpdateModalInputList (modalID,(inputID3,"Piercing & Slashing"))) ) ] [ str "Piercing & Slashing" ]
                                                                  option [ Props.Disabled true] [ str "___________________________" ]
                                                                  option [ Value "Bludgeoning & Piercing & Slashing"
                                                                           Props.OnClick (fun _ -> dispatch (UpdateModalInputList (modalID,(inputID3,"Bludgeoning & Piercing & Slashing"))) ) ] [ str "Bludgeoning & Piercing & Slashing" ]
                                                                  option [ Props.Disabled true] [ str "___________________________" ]
                                                                  option [ Value "Acid"
                                                                           Props.OnClick (fun _ -> dispatch (UpdateModalInputList (modalID,(inputID3,"Acid"))) ) ] [ str "Acid" ]
                                                                  option [ Value "Fire"
                                                                           Props.OnClick (fun _ -> dispatch (UpdateModalInputList (modalID,(inputID3,"Fire"))) ) ] [ str "Fire" ]
                                                                  option [ Value "Cold"
                                                                           Props.OnClick (fun _ -> dispatch (UpdateModalInputList (modalID,(inputID3,"Cold"))) ) ] [ str "Cold" ]
                                                                  option [ Value "Electricity"
                                                                           Props.OnClick (fun _ -> dispatch (UpdateModalInputList (modalID,(inputID3,"Electricity"))) ) ] [ str "Electricity" ]
                                                                  option [ Value "Precision"
                                                                           Props.OnClick (fun _ -> dispatch (UpdateModalInputList (modalID,(inputID3,"Precision"))) ) ] [ str "Precision" ]
                                                                  option [ Value "Vital Strike Damage"
                                                                           Props.OnClick (fun _ -> dispatch (UpdateModalInputList (modalID,(inputID3,"Vital Strike Damage"))) ) ] [ str "Vital Strike Damage" ]
                                                                ]                           
                                                            ]
                                            ]                           

                              ]
                ]

let inputPanelCrits modalID description placeholder placeholder2 placeholder3 inputID inputID2 inputID3 (dispatch : Msg -> unit) =
    Level.level [ ]
                [ Level.left [ ]
                             [ Level.item [] [str description] ]
                  Level.right [ ]
                              [ Input.text [ Input.Props [ Style [ CSSProp.Width "40px"] ]
                                             Input.Size IsSmall 
                                             Input.Placeholder placeholder
                                             Input.OnChange (fun e -> let x = !!e.target?value
                                                                      dispatch (UpdateModalInputList (modalID,(inputID,x))
                                                                               )
                                                            )
                                           ]
                                str "-"
                                Input.text [ Input.Props [ Style [ CSSProp.Width "40px"] ]
                                             Input.Size IsSmall 
                                             Input.Placeholder placeholder2
                                             Input.OnChange (fun e -> let x = !!e.target?value
                                                                      dispatch (UpdateModalInputList (modalID,(inputID2,x))
                                                                               )
                                                            )
                                           ]
                                str "x"
                                Input.text [ Input.Props [ Style [ CSSProp.Width "40px"] ]
                                             Input.Size IsSmall 
                                             Input.Placeholder placeholder3
                                             Input.OnChange (fun e -> let x = !!e.target?value
                                                                      dispatch (UpdateModalInputList (modalID,(inputID3,x))
                                                                               )
                                                            )
                                           ]
                              ]
                ]

let inputPanelAbilityScores modalID description inputID (dispatch : Msg -> unit) =
    Level.level [ ]
                [ Level.left [ ]
                             [ Level.item [] [str description] ]
                  Level.right [ ]
                              [ Select.select [ Select.Color IsWhite
                                                Select.Size ISize.IsSmall
                                                Select.IsInline]
                                              [ select [ DefaultValue "No ability score" ]
                                                  [ option [ Props.OnClick (fun _ -> dispatch (UpdateModalInputList (modalID,(inputID,"No ability score"))) )
                                                             Value "No ability score" ] [ str "No ability score" ]
                                                    option [ Value "Strength"
                                                             Props.OnClick (fun _ -> dispatch (UpdateModalInputList (modalID,(inputID,"Strength"))) ) ] [ str "Strength" ]
                                                    option [ Value "Dexterity"
                                                             Props.OnClick (fun _ -> dispatch (UpdateModalInputList (modalID,(inputID,"Dexterity"))) ) ] [ str "Dexterity" ]
                                                    option [ Value "Constitution"
                                                             Props.OnClick (fun _ -> dispatch (UpdateModalInputList (modalID,(inputID,"Constitution"))) ) ] [ str "Constitution" ]
                                                    option [ Value "Intelligence"
                                                             Props.OnClick (fun _ -> dispatch (UpdateModalInputList (modalID,(inputID,"Intelligence"))) ) ] [ str "Intelligence" ]
                                                    option [ Value "Wisdom"
                                                             Props.OnClick (fun _ -> dispatch (UpdateModalInputList (modalID,(inputID,"Wisdom"))) ) ] [ str "Wisdom" ]
                                                    option [ Value "Charisma"
                                                             Props.OnClick (fun _ -> dispatch (UpdateModalInputList (modalID,(inputID,"Charisma"))) ) ] [ str "Charisma" ]
                                                  ]                           
                                              ]
                              ]
                ]               

let inputPanelHandlingAndMultiplier modalID description placeholder inputID inputID2 (dispatch : Msg -> unit) =
    Level.level [ ]
                [ Level.left [ ]
                             [ Level.item [] [str description] ]
                  Level.right [ ]
                              [ Select.select [ Select.Color IsWhite
                                                Select.Size ISize.IsSmall
                                                Select.IsInline]
                                              [ select [ DefaultValue "One Handed" ]
                                                  [ option [ Value "Off Handed"
                                                             Props.OnClick (fun _ -> dispatch (UpdateModalInputList (modalID,(inputID,"Off Handed"))) ) ] [ str "Off Handed" ]
                                                    option [ Value "One Handed"
                                                             Props.OnClick (fun _ -> dispatch (UpdateModalInputList (modalID,(inputID,"One Handed"))) ) ] [ str "One Handed" ]
                                                    option [ Value "Two Handed"
                                                             Props.OnClick (fun _ -> dispatch (UpdateModalInputList (modalID,(inputID,"Two Handed"))) ) ] [ str "Two Handed" ]
                                                  ]                           
                                              ]
                                Input.text [ Input.Props [ Style [ CSSProp.Width "40px"] ]
                                             Input.Size IsSmall 
                                             Input.Placeholder placeholder
                                             Input.OnChange (fun e -> let x = !!e.target?value
                                                                      dispatch (UpdateModalInputList (modalID,(inputID2,x))
                                                                               )
                                                            )
                                           ]
                              ]
                ]   

let inputPanelManufacturedNatural modalID description inputID dispatch =
    Level.level [ ]
                [ Level.left [ ]
                             [ Level.item [] [str description] ]
                  Level.right [ ]
                              [ Select.select [ Select.Color IsWhite
                                                Select.Size ISize.IsSmall
                                                Select.IsInline]
                                              [ select [ DefaultValue "default" ]
                                                   [ option [ Value "Manufactured"
                                                              Props.OnClick (fun _ -> dispatch (UpdateModalInputList (modalID,(inputID,"Manufactured"))) ) ] [ str "Manufactured" ]
                                                     option [ Value "Natural"
                                                              Props.OnClick (fun _ -> dispatch (UpdateModalInputList (modalID,(inputID,"Natural"))) ) ] [ str "Natural" ]
                                                     option [ Value "default"
                                                              Props.Hidden true] [ str "Type" ]
                                                   ]
                                              ]                           
                              ]
                ]


// content for add character modal
let addWeaponModalContent (dispatch : Msg -> unit) =
    Panel.panel [ ]
                [ inputPanel "addWeapon" "Weapon Name:" ".. cool weapon name" 1 dispatch
                  inputPanelWeaponDmg "addWeapon" "Weapon Damage:" ".. 1" ".. 6" 2 3 4 dispatch
                  inputPanel "addWeapon" "Bonus Damage:" ".. enhancement boni, e.g. 1" 5 dispatch
                  inputPanelWeaponDmg "addWeapon" "Extra Damage:" ".. 1" ".. 6" 6 7 8 dispatch
                  inputPanelWeaponDmg "addWeapon" "Extra Damage on Crit:" ".. 2" ".. 10" 9 10 11 dispatch
                  inputPanel "addWeapon" "Attack Bonus:" ".. enhancement boni/mwk., e.g. 1" 12 dispatch
                  inputPanelCrits "addWeapon" "Crit Statistics" ".. 19" ".. 20" ".. 2" 13 14 15 dispatch
                  inputPanelAbilityScores "addWeapon" "Modifier to hit:" 16 dispatch
                  inputPanelAbilityScores "addWeapon" "Modifier to Damage:" 17 dispatch
                  inputPanelHandlingAndMultiplier "addWeapon" "Handling and Dmg - Multiplier" ".. 1.5" 18 19 dispatch
                  inputPanelManufacturedNatural "addWeapon" "Manufactured or Natural" 20 dispatch
                  inputPanel "addWeapon" "Description:" ".. cool weapon name" 21 dispatch ]

// add Weapon modal
let addWeaponModal closeDisplay (dispatch : Msg -> unit)=
    Modal.modal [ Modal.IsActive true
                  Modal.Props [ (onEnter AddModalInputToWeaponArray dispatch) ]
                ]
        [ Modal.background [ Props [ OnClick closeDisplay ] ] [ ]
          Modal.Card.card [ ]
            [ Modal.Card.head [ ]
                [ Modal.Card.title [ ]
                    [ str "Weapon Creator" ]
                  Delete.delete [ Delete.OnClick closeDisplay ] [ ] ]
              Modal.Card.body [ ]
                              [ addWeaponModalContent dispatch]
              Modal.Card.foot [ ]
                [ Button.button [ Button.Color IsSuccess
                                  Button.OnClick (fun _ -> dispatch AddModalInputToWeaponArray
                                                  )
                                ]
                                [ str "Add Weapon" ]
                  Button.button [ Button.OnClick closeDisplay ]
                                [ str "Cancel" ] ] ] ]

let activateAddWeaponDispatch dispatch =
    ActivateModal (addWeaponModal (fun _ -> dispatch CloseModal) dispatch)

///////////////////////////////////////////////////////// Show active Weapon modal ///////////////////////////////////////////////////////////

let weaponStats (w:Weapon) =

    let weapStrings = [|w.Name; (sprintf "%id%i %A" w.Damage.NumberOfDie w.Damage.Die w.Damage.DamageType); (string w.DamageBonus);
                        (sprintf "%id%i %A" w.ExtraDamage.OnHit.NumberOfDie w.ExtraDamage.OnHit.Die w.ExtraDamage.OnHit.DamageType);
                        (sprintf "%id%i %A" w.ExtraDamage.OnCrit.NumberOfDie w.ExtraDamage.OnCrit.Die w.ExtraDamage.OnCrit.DamageType);
                        (string w.BonusAttackRolls); (sprintf "%i-%i/x%i" (Array.min w.CriticalRange) (Array.max w.CriticalRange) w.CriticalModifier)
                        (string w.Modifier.ToHit); (string w.Modifier.ToDmg); (sprintf "%A, with %A damage multiplier" w.Modifier.MultiplicatorOnDamage.Hand w.Modifier.MultiplicatorOnDamage.Multiplicator)
                        (string w.ManufacturedOrNatural); w.Description |]

    let descStrings = [|"Weapon Name"; "Weapon Damage"; "Bonus Damage"; "Extra Damage"; "Extra Damage on Crit"; "Attack Bonus";
                        "Crit Statistics"; "Modifier to hit"; "Modifier to Damage"; "Handling and Dmg - Multiplier"; "Manufactured or Natural"; "Description"|]

    let combinedArr = Array.zip descStrings weapStrings

    Container.container [ Container.IsFluid ]
                        (combinedArr
                        |> Array.map (fun (desc,value) -> Panel.panel [ ]
                                                                      [ Columns.columns [ ]
                                                                                        [ Column.column [ Column.Modifiers [ Modifier.IsPulledLeft ] ] [ str desc ]
                                                                                          Column.column [ Column.Modifiers [ Modifier.IsPulledRight ] ] [ str value ]
                                                                                        ] 
                                                                      ]
                                      )
                        |> List.ofArray
                        )


let displayActiveWeapon char (dispatch : Msg -> unit)=
    Modal.modal [ Modal.IsActive true
                  Modal.Props [ (onEnter CloseModal dispatch) ]
                ]
        [ Modal.background [ Props [ OnClick (fun _ -> dispatch CloseModal) ] ] [ ]
          Modal.Card.card [ ]
            [ Modal.Card.head [ ]
                [ Modal.Card.title [ ]
                    [ str "Weapon Creator" ]
                  Delete.delete [ Delete.OnClick (fun _ -> dispatch CloseModal)] [ ] ]
              Modal.Card.body [ ]
                              [ weaponStats char ]
              Modal.Card.foot [ ]
                [ Button.button [ Button.OnClick (fun _ -> dispatch CloseModal) ]
                                [ str "Cancel" ] ] ] ]            

let activatedisplayActiveWeapon weapon dispatch =
    ActivateModal (displayActiveWeapon weapon dispatch)

///////////////////////////////////////////////////////// Attack Calculator Card ////////////////////////////////////////////////////////


let searchBarTab (dispatch : Msg -> unit) (id:int) (tabCategory:string) (specificSearchResults:SubSearchResult []) =
    let getIntForTabCategory =
        match tabCategory with
        | "characters" -> 1
        | "weapons" -> 2
        | "modifications" -> 3
        | _ -> failwith "unknown case, you should not get this 003"
    let searchResultElement = 
        specificSearchResults
        |> Array.map (fun subSearch -> subSearch,createActivateSearchResultButton subSearch.ResultName (UpdateActiveModifierList (id,getIntForTabCategory,subSearch.ResultName)) dispatch)
        |> Array.map (fun (subSearch,button) -> tr [ ]
                                                   [ th [ ] [ str (sprintf "%s" subSearch.ResultName) ]
                                                     th [ ]
                                                        [ Text.span [ Modifiers [Modifier.TextSize (Screen.All,TextSize.Is7) ] ]
                                                                    [ str (sprintf "%s" subSearch.ResultDescription) ] 
                                                        ]
                                                     th [ ] [ button ]
                                                     deleteSearchResultFromActiveArrayButton (DeleteSearchResultFromActiveArray (getIntForTabCategory,subSearch.ResultName)) dispatch
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
                                                                                                              Button.OnClick (fun _ -> match tabCategory with
                                                                                                                                       // Control elment of the modal UI
                                                                                                                                       | "characters" -> dispatch (activateAddCharacterDispatch dispatch)
                                                                                                                                       | "weapons" -> dispatch (activateAddWeaponDispatch dispatch)
                                                                                                                                       | "modifications" -> dispatch (activateAddCharacterDispatch dispatch)
                                                                                                                                       | _ -> failwith "unknown case, you should not get this 003"
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
                                                                                                            Input.Props [ onEnter (UpdateSearchResultList (id, getIntForTabCategory)) dispatch ]
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

    let activeWeaponElement =
        let singleElement activeID weapon=
            Tag.tag [ Tag.Props [ Props.OnClick (fun _ -> dispatch (activatedisplayActiveWeapon weapon dispatch)) ]
                      Tag.Color Color.IsWhiteTer]
                    [str weapon.Name ]
            
        div []
            (relatedActiveModifier.ActiveWeapons
            |> List.map (fun (x,y) -> singleElement x y))

    let stringOfActiveModifications =
        relatedActiveModifier.ActiveModifications
        |> List.fold (fun arr ele -> ele.Name + ", " + arr) ""
        |> fun x -> x.Trim([|',';' '|])
    let dropdownButtonSize (sizeStr:string) =
        Dropdown.Item.a [ Dropdown.Item.Props [ Props.OnClick (fun _ -> dispatch (UpdateActiveModifierListOnlySize (id,sizeStr))) ]
                             ]
                 [ str sizeStr]
    Card.card [ ]
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
          Card.content [ Props [ Props.Id (sprintf "TabMainOutput%A" id);
                                 Props.Style [CSSProp.Display DisplayOptions.None] ]
                         ]
                       [ Container.container [ Container.IsFluid
                                               Container.Modifiers [ Modifier.TextSize (Screen.All,TextSize.Is6)] ]
                                             [ h4 [ Props.Id (sprintf "OutputCharacter%A" id)
                                                    Props.OnClick (fun _ -> let char = relatedActiveModifier.ActiveCharacter
                                                                            dispatch (activatedisplayActivechar char dispatch))
                                                  ]
                                                  [ str relatedActiveModifier.ActiveCharacter.CharacterName]
                                               p [ Props.Id (sprintf "OutputSize%A" id) ]
                                                 [ str (string relatedActiveModifier.ActiveSize) ]
                                               p [ Props.Id (sprintf "OutputWeapons%A" id) ]
                                                 [ Level.level [ ]
                                                               [ activeWeaponElement
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
                                                                               [ str "reset modifications" ]
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
                                                                              [ i [ClassName "fas fa-dice-d20"] [] ]
                                                                    span [] [str "Calculate Standard Attack"
                                                                                ]
                                                                  ] 
                                                  ]
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

////////////////////////////////////////// Original Webelement From SAFE stack ////////////////////////////////////////////////////////////

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

let footerContainer =
    Container.container [ ]
        [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
            [ p [ ]
                [ safeComponents ]
              p [ ]
                [ ] ] ]