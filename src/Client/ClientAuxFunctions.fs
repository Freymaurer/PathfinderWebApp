module ClientAuxFunctions

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

let doHideShow (tabId:string) (cardID:int) =
    let x = Dom.document.getElementById(sprintf "%A%i" tabId cardID)
    if x?style?display = "none"
    then x?style?display <- "block"
    else x?style?display <- "none"

let doHide (tabId:string) (cardID:int) =
    let x = Dom.document.getElementById(sprintf "%A%i" tabId cardID)
    x?style?display <- "none"

let matchStringToAbilityScore string =
    match string with
    | "Strength" -> AbilityScore.Strength
    | "Dexterity" -> AbilityScore.Dexterity
    | "Constitution" -> AbilityScore.Constitution
    | "Intelligence" -> AbilityScore.Intelligence
    | "Wisdom" -> AbilityScore.Wisdom
    | "Charisma" -> AbilityScore.Charisma
    | _ -> AbilityScore.NoAS

let matchStringToDamageType string=
    match string with
    | "Bludgeoning"                       -> DamageTypes.Bludgeoning
    | "Piercing"                          -> DamageTypes.Piercing
    | "Slashing"                          -> DamageTypes.Slashing
    | "Bludgeoning & Slashing"            -> DamageTypes.BludgeoningOrSlashing
    | "Bludgeoning & Piercing"            -> DamageTypes.BludgeoningOrPiercing
    | "Piercing & Slashing"               -> DamageTypes.PiercingOrSlashing
    | "Piercing & Slashing & Bludgeoning" -> DamageTypes.BludgeoningOrPiercingOrSlashing
    | "Acid"                              -> DamageTypes.Acid
    | "Fire"                              -> DamageTypes.Fire
    | "Cold"                              -> DamageTypes.Cold
    | "Electricity"                       -> DamageTypes.Electricity
    | "Precision"                         -> DamageTypes.Precision
    | "Vital Strike Damage"               -> DamageTypes.VitalStrikeDamage
    | _                                   -> DamageTypes.Untyped

let matchStringToHandling string =
    match string with
    | "Off Handed" -> WeaponHanded.OffHand
    | "One Handed" -> WeaponHanded.OneHanded
    | "Two Handed" -> WeaponHanded.TwoHanded
    | _ -> WeaponHanded.OneHanded

let matchStringToNaturalOrManufactured string =
    match string with
    | "Manufactured" -> Manufactured
    | "Natural" -> Natural
    | _ -> Manufactured