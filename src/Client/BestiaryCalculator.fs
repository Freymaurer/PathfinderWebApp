﻿namespace PathfinderAttackSimulator

open System
open Library
open Library.AuxLibFunctions

/// This module contains both bestiary calculator functions for standard attacks "calculateStandardAttack" and full-round attacks "calculateFullAttack"
module BestiaryCalculator =
    
    open BestiaryReader.AuxFunctions

    let private testForNaturalAttack (str:string) =
        let regexNaturalAttack = System.Text.RegularExpressions.Regex("(claw|vine|tentacle|bite|gore|hoof|wing|pincers|tail\sslap|slam|sting|talon|tongue)")
        regexNaturalAttack.IsMatch(str)

    /// This function returns the calculated attack rolls of a d20pfsrd/archives of nethys bestiary entry.
    /// attackinfo = the output of the "getMonsterInformation" function, attackVariant = Melee/Ranged,
    /// attackNumber = the exact attack variant that should be calculated, starting at 1
    /// modifications = array of attackmodifications (StatChanges will not work for this function)
    let calculateStandardAttack (attackInfo: URLMonsterAttacks []) (attackVariant:AttackVariant) (attackNumber:int) (modifications: AttackModification []) =
        
        //check if there is actually the attack wanted by the user
        if attackNumber > attackInfo.Length then failwith "The chosen url does not provide enough different attacks for the attackNumber given. Try giving a smaller number or 1."
        let monsterStats = attackInfo.[0].RelevantMonsterStats
    
        let rollDice count (diceSides:int) =
            let rnd = System.Random()
            if diceSides = 0 
            then [|0|]
            else Array.init count (fun _ -> rnd.Next (1, diceSides+1))
        
        let getRndArrElement =
            let rnd = Random()
            fun (arr : int[]) -> arr.[rnd.Next(arr.Length)]
    
        //filter for either melee or ranged
        let wantedMonsterAttack =
            attackInfo
            |> Array.filter (fun x -> x.AttackType = attackVariant)
            |> fun x -> if x.Length = 0 
                        then failwith "The chosen url does not provide any attacks for the chosen attack type (melee or ranged)."
                        else x.[attackNumber-1]

        let wantedAttack =
            wantedMonsterAttack
            |> fun x -> x.AttackScheme.[0]

    
        //attack roll for this exact attack
        let (attackRoll,critConfirmationRoll) = 
            let getAttackRolls =
                    rollDice 10000 20
            getRndArrElement getAttackRolls,getRndArrElement getAttackRolls
        //
        let startSize =
            match monsterStats.Size with
            | Fine          -> 1
            | Diminuitive   -> 2
            | Tiny          -> 3
            | Small         -> 4
            | Medium        -> 5
            | Large         -> 6
            | Huge          -> 7
            | Gargantuan    -> 8
            | Colossal      -> 9

        //
        let calculatedSize =
    
            let changeSizeBy =
                modifications
                |> Array.filter (fun x -> x.SizeChanges.EffectiveSizeChange = false)
                |> Array.map (fun x -> x.SizeChanges)
                |> Array.groupBy (fun x -> x.SizeChangeBonustype)
                |> Array.map (fun (header,bonusArr) -> if header <> BonusTypes.Flat 
                                                       then bonusArr
                                                            |> Array.sortByDescending (fun x -> x.SizeChangeValue) 
                                                            |> fun x -> Array.head x
                                                            |> fun x -> x.SizeChangeValue
                                                       elif header = BonusTypes.Flat
                                                       then bonusArr
                                                            |> Array.map (fun x -> x.SizeChangeValue)
                                                            |> Array.sum
                                                       else failwith "Unrecognized Pattern of attackBoni in 'addBoniToAttack'" 
                             )
                |> Array.sum
    
            (startSize + changeSizeBy)
            |> fun x -> if x > 9 then 9
                        elif x < 1 then 1
                        else x
    
        //Start adding up attack boni
        let attackBonus =
            wantedAttack
            |> fun x -> Array.max x.AttackBonus

        //
        let attackBoniSize =
            let sizeModifierNew =
                calculatedSize
                |> fun x -> Map.find x findSizes
                |> fun x -> x.SizeModifier
            let sizeModifierOld =
                startSize
                |> fun x -> Map.find x findSizes
                |> fun x -> x.SizeModifier
            sizeModifierNew - sizeModifierOld

        let attackBoniModifications = 
            modifications 
            |> Array.map (fun x -> x.BonusAttackRoll.OnHit)
            |> Array.groupBy (fun x -> x.BonusType)
            |> Array.map (fun (header,bonusArr) -> if header <> BonusTypes.Flat 
                                                   then bonusArr
                                                        |> Array.map (fun x -> x.Value)
                                                        |> Array.max
                                                   elif header = BonusTypes.Flat
                                                   then bonusArr
                                                        |> Array.map (fun x -> x.Value)
                                                        |> Array.sum
                                                   else failwith "Unrecognized Pattern of attackBoni in 'addBoniToAttack'"
                          )
            |> Array.sum
    
        let attackBoniWithoutFullRoundFeats =
            let regexTwoWeaponFighting = System.Text.RegularExpressions.Regex("Two\WWeapon\sFighting")
            let regexRapidShot = System.Text.RegularExpressions.Regex("Rapid\sShot")
            let (|RapidShot|NoRapidShot|) (strArr:string []) = 
                if (Array.contains true (Array.map (fun str -> regexRapidShot.IsMatch(str)
                                                   ) strArr                                        
                                        ) 
                   ) = true
                   && attackVariant = Ranged
                then RapidShot
                else NoRapidShot
            let (|TwoWeaponFighting|NoTwoWeaponFighting|) (strArr:string []) = 
                if (Array.contains true (Array.map (fun str -> regexTwoWeaponFighting.IsMatch(str)
                                                   ) strArr
                                        ) 
                   ) = true
                   && wantedMonsterAttack.AttackScheme.Length > 1
                   && (testForNaturalAttack wantedAttack.WeaponName) = false
                then TwoWeaponFighting
                else NoTwoWeaponFighting
            let balanceTwoWeaponMalus =
                match monsterStats.Feats with
                | TwoWeaponFighting -> 2
                | NoTwoWeaponFighting -> 0
            let balanceRapidShotMalus =
                match monsterStats.Feats with
                | RapidShot -> 2
                | NoRapidShot -> 0
            balanceTwoWeaponMalus + balanceRapidShotMalus

        let combinedAttackBoni =
            attackBonus + attackBoniModifications + attackBoniSize + attackBoniWithoutFullRoundFeats
    
        let totalAttackBonus =
            attackRoll + combinedAttackBoni

        let totalAttackCritBonus =
            let critSpecificBonus =
                modifications
                |> Array.map (fun x -> x.BonusAttackRoll.OnCrit)
                |> Array.groupBy (fun x -> x.BonusType)
                |> Array.map (fun (header,bonusArr) -> if header <> BonusTypes.Flat 
                                                       then bonusArr
                                                            |> Array.sortByDescending (fun x -> x.Value) 
                                                            |> fun x -> Array.head x
                                                            |> fun x -> x.Value
                                                       elif header = BonusTypes.Flat
                                                       then bonusArr
                                                            |> Array.map (fun x -> x.Value)
                                                            |> Array.sum
                                                       else failwith "Unrecognized Pattern of attackBoni in 'addBoniToAttack'"
                              )
                |> Array.sum
            critConfirmationRoll + combinedAttackBoni + critSpecificBonus
    
        /////End attack boni/Start damage boni/////
    
        let sizeAdjustedWeaponDamage =
    
            let effectiveSize =
    
                let changeSizeBy =
                    modifications
                    |> Array.map (fun x -> x.SizeChanges)
                    |> Array.groupBy (fun x -> x.SizeChangeBonustype)
                    |> Array.map (fun (header,bonusArr) -> if header <> BonusTypes.Flat 
                                                           then bonusArr
                                                                |> Array.map (fun x -> x.SizeChangeValue)
                                                                |> Array.max
                                                           elif header = BonusTypes.Flat
                                                           then bonusArr
                                                                |> Array.map (fun x -> x.SizeChangeValue)
                                                                |> Array.sum
                                                           else failwith "Unrecognized Pattern of sizeChangeBoni." 
                                 )
                    |> Array.sum
    
                (startSize + changeSizeBy)
                |> fun x -> if x > 9 then 9
                            elif x < 1 then 1
                            else x
    
            let diceRow = 
                [|(1,1);(1,2);(1,3);(1,4);(1,6);(1,8);(1,10);(2,6);(2,8);(3,6);(3,8);
                (4,6);(4,8);(6,6);(6,8);(8,6);(8,8);(12,6);(12,8);(16,6);(16,8);(24,6);(24,8);(36,6);(36,8)|] 
    
            ///https://paizo.com/paizo/faq/v5748nruor1fm#v5748eaic9t3f
            let getSizeChange reCalcWeapon (startS: int) (modifiedS: int) =
                let snowFlakeIncrease numberofdice (die: int) =
                    match numberofdice with 
                    | 1 -> 2,die
                    | _ -> (numberofdice + int (floor (float numberofdice)*(1./3.))), die
                let snowFlakeDecrease numberofdice (die: int) =
                    match numberofdice with 
                    | 2 -> 1,die
                    | _ -> (numberofdice - int (floor (float numberofdice)*(1./3.))), die
                let isEven x = (x % 2) = 0         
                let isOdd x = (x % 2) = 1
                let sizeDiff = modifiedS - startS
                let decInc = if sizeDiff < 0 then (-1.)
                             elif sizeDiff > 0 then (1.)
                             else 0.
                let adjustedDie = match reCalcWeapon.Die with
                                  | 2 -> reCalcWeapon.NumberOfDie, reCalcWeapon.Die
                                  | 3 -> reCalcWeapon.NumberOfDie, reCalcWeapon.Die
                                  | 4 -> match reCalcWeapon.NumberOfDie with
                                         | 1                                                        -> reCalcWeapon.NumberOfDie, reCalcWeapon.Die
                                         | odd when isOdd reCalcWeapon.NumberOfDie = true    -> int (ceil (float reCalcWeapon.NumberOfDie/2.)), 6
                                         | even when isEven reCalcWeapon.NumberOfDie = true  -> (reCalcWeapon.NumberOfDie/2), 8
                                         | _                                                        -> failwith "unknown combination for reCalcWeapon damage dice calculator accoringly to size; Error4"
                                  | 6 -> reCalcWeapon.NumberOfDie, reCalcWeapon.Die
                                  | 8 -> reCalcWeapon.NumberOfDie, reCalcWeapon.Die
                                  | 10 -> reCalcWeapon.NumberOfDie, reCalcWeapon.Die
                                  | 12 -> (reCalcWeapon.NumberOfDie*2), 6
                                  | 20 -> (reCalcWeapon.NumberOfDie*2), 10
                                  | _ -> if reCalcWeapon.Die % 10 = 0
                                         then ((reCalcWeapon.Die / 10) * reCalcWeapon.NumberOfDie), 10
                                         elif reCalcWeapon.Die % 6 = 0
                                         then ((reCalcWeapon.Die / 6) * reCalcWeapon.NumberOfDie), 6
                                         elif reCalcWeapon.Die % 4 = 0 
                                         then ((reCalcWeapon.Die / 4) * reCalcWeapon.NumberOfDie), 4
                                         else reCalcWeapon.NumberOfDie, reCalcWeapon.Die
                let adjustedDieNum = fst adjustedDie
                let adjustedDietype = snd adjustedDie
    
                let rec loopResizeWeapon (n:int) (nDice:int) (die:int) = 
    
                    let stepIncrease = if startS + (int decInc*n) < 5 || (nDice * die) < 6 
                                       then 1
                                       else 2
                    let stepDecrease = if startS + (int decInc*n) < 6 || (nDice * die) < 8 
                                       then 1
                                       else 2
                    let findRowPosition =
                        Array.tryFindIndex (fun (x,y) -> x = nDice && y = die) diceRow
                    if sizeDiff = 0 || n >= abs sizeDiff
                    then nDice, die
                    else findRowPosition
                         |> fun x -> if (x.IsSome) 
                                     then match decInc with 
                                          | dec when decInc < 0. -> if x.Value < 1 then diceRow.[0] else diceRow.[x.Value - stepDecrease]
                                          | inc when decInc > 0. -> if x.Value > (diceRow.Length-3) then (snowFlakeIncrease nDice die) else diceRow.[x.Value + stepIncrease]
                                          | _ -> failwith "unknown combination for reCalcWeapon damage dice calculator accoringly to size; Error1"
                                     elif x.IsSome = false 
                                     then match decInc with 
                                          | dec when decInc < 0. -> snowFlakeDecrease nDice die
                                          | inc when decInc > 0. -> snowFlakeIncrease nDice die
                                          | _ -> failwith "unknown combination for reCalcWeapon damage dice calculator accoringly to size; Error2"
                                     else failwith "unknown combination for reCalcWeapon damage dice calculator accoringly to size; Error3"
                         |> fun (nDie,die) -> loopResizeWeapon (n+1) nDie die
    
                loopResizeWeapon 0 adjustedDieNum adjustedDietype
                |> fun (n,die) -> createURLDamage n die reCalcWeapon.BonusDamage reCalcWeapon.DamageType
    
            getSizeChange wantedAttack.WeaponDamage startSize effectiveSize
    
        let damageRolls =
            let getDamageRolls =
                rollDice 1000 sizeAdjustedWeaponDamage.Die
            [|for i=1 to sizeAdjustedWeaponDamage.NumberOfDie do
                yield getRndArrElement getDamageRolls|]
            |> Array.sum
    
        let modificationDamageBoni =
            modifications
            |> Array.map (fun x -> x.BonusDamage)
            |> Array.groupBy (fun x -> x.BonusType)
            |> Array.map (fun (x,bonusArr) -> if x <> BonusTypes.Flat 
                                              then bonusArr
                                                   |> Array.map (fun x -> x.Value)
                                                   |> Array.max
                                              else bonusArr
                                                   |> Array.map (fun x -> x.Value)
                                                   |> Array.sum                   
                          )
            |> Array.sum
    
        let totalDamage =
            damageRolls + sizeAdjustedWeaponDamage.BonusDamage + modificationDamageBoni
            |> fun x -> if x <= 0 then 1 else x
    
        let extraDamageOnHit =
            let getDamageRolls numberOfDie die=
                let rolledDice = rollDice 1000 die
                [|for i=1 to numberOfDie do
                    yield getRndArrElement rolledDice|]
                |> Array.sum
            let extraDamageModifications =
                modifications 
                |> Array.map (fun x -> x.ExtraDamage.OnHit,x.Name)
                |> Array.map (fun (extraDmg,str) -> getDamageRolls extraDmg.NumberOfDie extraDmg.Die
                                                    , extraDmg.DamageType, str
                             )
                ///Vital Strike hardcode
                |> fun extraDmg -> if (Array.exists (fun modi -> modi = Modifications.VitalStrike
                                                                 || modi = Modifications.VitalStrikeImproved
                                                                 || modi = Modifications.VitalStrikeGreater) modifications 
                                      ) = true
                                   then Array.filter (fun (x:AttackModification) -> x.ExtraDamage.OnHit.DamageType = VitalStrikeDamage) modifications
                                        |> Array.sortByDescending (fun x -> x.ExtraDamage.OnHit.NumberOfDie)
                                        |> Array.head
                                        |> fun vitalS -> [|for i in 1 .. vitalS.ExtraDamage.OnHit.NumberOfDie do
                                                            yield getDamageRolls sizeAdjustedWeaponDamage.NumberOfDie sizeAdjustedWeaponDamage.Die|], vitalS.Name
                                        |> fun x -> x
                                        |> fun (intList,str) -> Array.sum intList, str
                                        |> fun (bonus,str) -> Array.append [|bonus,VitalStrikeDamage,str|] extraDmg
                                   else extraDmg
                |> Array.map (fun (bonusValue,dmgType,modificationName) -> bonusValue,(string dmgType))
            //add weapon extra dmg (e.g. shocking enchantment) to modification extra dmg
            extraDamageModifications
            |> Array.filter (fun (extraDmgValue,dType) -> extraDmgValue <> 0 )
    
        let extraDamageOnCrit =
            let getDamageRolls numberOfDie die=
                let rolledDice = rollDice 1000 die
                [|for i=1 to numberOfDie do
                    yield getRndArrElement rolledDice|]
                |> Array.sum
            let extraDamageModifications =
                if (Array.contains attackRoll wantedAttack.CriticalRange) = false
                then [||]
                else modifications 
                     |> Array.map (fun x -> x.ExtraDamage.OnCrit,x.Name)
                     |> Array.map (fun (extraDmg,str) -> getDamageRolls extraDmg.NumberOfDie extraDmg.Die
                                                         , extraDmg.DamageType, str
                                  )
                     ///Vital Strike hardcode
                     |> fun extraDmg -> if (Array.exists (fun modi -> modi = Modifications.VitalStrike
                                                                      || modi = Modifications.VitalStrikeImproved
                                                                      || modi = Modifications.VitalStrikeGreater) modifications 
                                           ) = true
                                        then Array.filter (fun (x:AttackModification) -> x.ExtraDamage.OnHit.DamageType = VitalStrikeDamage) modifications
                                             |> Array.sortByDescending (fun x -> x.ExtraDamage.OnHit.NumberOfDie)
                                             |> Array.head
                                             |> fun vitalS -> [|for i in 1 .. vitalS.ExtraDamage.OnHit.NumberOfDie do
                                                                 yield getDamageRolls sizeAdjustedWeaponDamage.NumberOfDie sizeAdjustedWeaponDamage.Die|], vitalS.Name
                                             |> fun x -> x
                                             |> fun (intList,str) -> Array.sum intList, str
                                             |> fun (bonus,str) -> Array.append [|bonus,VitalStrikeDamage,str|] extraDmg
                                        else extraDmg
                     |> Array.map (fun (bonusValue,dmgType,modificationName) -> bonusValue,(string dmgType))
            //add weapon extra dmg (e.g. shocking enchantment) to modification extra dmg
            extraDamageModifications
            |> Array.filter (fun (extraDmgValue,dType) -> extraDmgValue <> 0 )

        let extraDamageCombined =
            let getDamageRolls numberOfDie die=
                let rolledDice = rollDice 1000 die
                [|for i=1 to numberOfDie do
                    yield getRndArrElement rolledDice|]
                |> Array.sum
            if extraDamageOnCrit = [||]
            then extraDamageOnHit
                |> Array.append [|(getDamageRolls wantedAttack.ExtraDamage.NumberOfDie wantedAttack.ExtraDamage.Die, wantedAttack.ExtraDamage.DamageType)|]
                |> Array.filter (fun (extraDmgValue,dType) -> extraDmgValue <> 0 )
            else Array.map2 (fun (onHit:(int*string)) (onCrit:(int*string)) -> (fst onHit) + (fst onCrit), snd onHit) extraDamageOnHit extraDamageOnCrit
                |> Array.append [|(getDamageRolls wantedAttack.ExtraDamage.NumberOfDie wantedAttack.ExtraDamage.Die, wantedAttack.ExtraDamage.DamageType)|]
                |> Array.filter (fun (extraDmgValue,dType) -> extraDmgValue <> 0 ) 

        let extraDamageToString extraDmgArr= 
            extraDmgArr
            |> Array.map (fun (value,dType) -> "+" + (string value) + " " + (string dType) + " " + "damage"  + ", ")
            |> Array.fold (fun strArr x -> strArr + x) "" 
            |> fun x -> x.TrimEnd [|' ';','|]    
    
        let additionalInfoString =
            if wantedAttack.AdditionalEffects = ""
            then ""
            else "plus " + wantedAttack.AdditionalEffects
    
        if (Array.contains attackRoll wantedAttack.CriticalRange) = false && extraDamageCombined = [||]
            then printfn "You attack with a %s and hit with a %i (rolled %i) for %i damage %s!" wantedAttack.WeaponName totalAttackBonus attackRoll totalDamage additionalInfoString
        elif (Array.contains attackRoll wantedAttack.CriticalRange) = true && extraDamageCombined = [||] 
            then printfn "You attack with a %s and (hopefully) critically hit the enemy with a %i (rolled %i) and confirm your crit with a %i (rolled %i) for %i Damage (x %i) %s!" wantedAttack.WeaponName totalAttackBonus attackRoll totalAttackCritBonus critConfirmationRoll totalDamage wantedAttack.CriticalModifier additionalInfoString
        elif (Array.contains attackRoll wantedAttack.CriticalRange) = false && extraDamageCombined <> [||]
            then printfn "You attack with a %s and hit the enemy with a %i (rolled %i) for %i damage %s %s!" wantedAttack.WeaponName totalAttackBonus attackRoll totalDamage (extraDamageToString extraDamageCombined) additionalInfoString
        elif (Array.contains attackRoll wantedAttack.CriticalRange) = true && extraDamageCombined <> [||] 
            then printfn ("You attack with a %s and (hopefully) critically hit the enemy with a %i (rolled %i) and confirm your crit with a %i (rolled %i) for %i damage (x %i) %s (%s on a crit) / (%s when not confirmed) !") wantedAttack.WeaponName totalAttackBonus attackRoll totalAttackCritBonus critConfirmationRoll totalDamage wantedAttack.CriticalModifier additionalInfoString (extraDamageToString extraDamageCombined) (extraDamageToString extraDamageOnHit)  
            else printfn "You should not see this message, please open an issue with your input as a bug report"

    /// This function returns the calculated attack rolls of a d20pfsrd/archives of nethys bestiary entry.
    /// attackinfo = the output of the "getMonsterInformation" function, attackVariant = Melee/Ranged,
    /// attackNumber = the exact attack variant that should be calculated, starting at 1
    /// modifications = array of attackmodifications (StatChanges will not work for this function)
    let calculateFullAttack (attackInfo: URLMonsterAttacks []) (attackVariant:AttackVariant) (attackNumber:int) (modifications: AttackModification []) =
        
        //check if there is actually the attack wanted by the user
        if attackNumber > attackInfo.Length then failwith "The chosen url does not provide enough different attacks for the attackNumber given. Try giving a smaller number or 1."
        let monsterStats = attackInfo.[0].RelevantMonsterStats
    
        let rollDice count (diceSides:int) =
            let rnd = System.Random()
            if diceSides = 0 
            then [|0|]
            else Array.init count (fun _ -> rnd.Next (1, diceSides+1))
        
        let getRndArrElement =
            let rnd = Random()
            fun (arr : int[]) -> arr.[rnd.Next(arr.Length)]
    
        //filter for either melee or ranged
        let wantedAttack =
            attackInfo
            |> Array.filter (fun x -> x.AttackType = attackVariant)
            |> fun x -> if x.Length = 0 
                        then failwith "The chosen url does not provide any attacks for the chosen attack type (melee or ranged)."
                        else x.[attackNumber-1]
    
        let attackArr =
            wantedAttack
            |> fun x -> x.AttackScheme
                        |> Array.collect (fun attack -> [|for i = 0 to (attack.AttackBonus.Length-1) do
                                                            yield attack.AttackBonus.[i], attack|] 
                                         )
    
        let modificationsForAllAttacks =
            modifications
            |> Array.filter (fun x -> snd x.AppliedTo = -20)
    
        let modificationsForNotAllAttacks =
            let attackMaximumLength appliedTo = if appliedTo < attackArr.Length then appliedTo else attackArr.Length
            modifications
            |> Array.filter (fun x -> snd x.AppliedTo <> -20)
            |> fun arr -> if Array.isEmpty arr
                          then Array.create attackArr.Length Modifications.ZeroMod
                               |> Array.mapi (fun i x -> i,x)
                          else Array.collect (fun x -> (Array.create (snd x.AppliedTo) x) 
                                                       |> fun x -> Array.append x (Array.create (attackArr.Length - x.Length) Modifications.ZeroMod)
                                                       |> Array.mapi (fun i x -> i,x)
                                              ) arr
            |> Array.groupBy (fun (x,y) -> x)
            |> Array.map (fun (x,y) -> (Array.map snd y))
            |> fun x -> x
        
        let modificationsCombined =
            modificationsForNotAllAttacks 
            |> Array.map (Array.append modificationsForAllAttacks)
    
        let calculateOneAttack attackBonus (urlAttack: URLAttack) (modificationArr: AttackModification []) =
            
            //attack roll for this exact attack
            let (attackRoll,critConfirmationRoll) = 
                let getAttackRolls =
                        rollDice 10000 20
                getRndArrElement getAttackRolls,getRndArrElement getAttackRolls

            //
            let startSize =
                match monsterStats.Size with
                | Fine          -> 1
                | Diminuitive   -> 2
                | Tiny          -> 3
                | Small         -> 4
                | Medium        -> 5
                | Large         -> 6
                | Huge          -> 7
                | Gargantuan    -> 8
                | Colossal      -> 9

            //
            let calculatedSize =
    
                let changeSizeBy =
                    modificationArr
                    |> Array.filter (fun x -> x.SizeChanges.EffectiveSizeChange = false)
                    |> Array.map (fun x -> x.SizeChanges)
                    |> Array.groupBy (fun x -> x.SizeChangeBonustype)
                    |> Array.map (fun (header,bonusArr) -> if header <> BonusTypes.Flat 
                                                           then bonusArr
                                                                |> Array.sortByDescending (fun x -> x.SizeChangeValue) 
                                                                |> fun x -> Array.head x
                                                                |> fun x -> x.SizeChangeValue
                                                           elif header = BonusTypes.Flat
                                                           then bonusArr
                                                                |> Array.map (fun x -> x.SizeChangeValue)
                                                                |> Array.sum
                                                           else failwith "Unrecognized Pattern of attackBoni in 'addBoniToAttack'" 
                                 )
                    |> Array.sum
    
                (startSize + changeSizeBy)
                |> fun x -> if x > 9 then 9
                            elif x < 1 then 1
                            else x

            ///
            let attackBoniSize =
                let sizeModifierNew =
                    calculatedSize
                    |> fun x -> Map.find x findSizes
                    |> fun x -> x.SizeModifier
                let sizeModifierOld =
                    startSize
                    |> fun x -> Map.find x findSizes
                    |> fun x -> x.SizeModifier
                sizeModifierNew - sizeModifierOld
    
            //Start adding up attack boni
            let AttackBoniModifications = 
                modificationArr 
                |> Array.map (fun x -> x.BonusAttackRoll.OnHit)
                |> Array.groupBy (fun x -> x.BonusType)
                |> Array.map (fun (header,bonusArr) -> if header <> BonusTypes.Flat 
                                                       then bonusArr
                                                            |> Array.map (fun x -> x.Value)
                                                            |> Array.max
                                                       elif header = BonusTypes.Flat
                                                       then bonusArr
                                                            |> Array.map (fun x -> x.Value)
                                                            |> Array.sum
                                                       else failwith "Unrecognized Pattern of attackBoni in 'addBoniToAttack'"
                              )
                |> Array.sum
    
            let combinedAttackBoni =
                attackBonus + AttackBoniModifications + attackBoniSize
    
            let totalAttackBonus =
                attackRoll + combinedAttackBoni

            let totalAttackCritBonus =

                let critSpecificBonus =
                    modifications
                    |> Array.map (fun x -> x.BonusAttackRoll.OnCrit)
                    |> Array.groupBy (fun x -> x.BonusType)
                    |> Array.map (fun (header,bonusArr) -> if header <> BonusTypes.Flat 
                                                           then bonusArr
                                                                |> Array.sortByDescending (fun x -> x.Value) 
                                                                |> fun x -> Array.head x
                                                                |> fun x -> x.Value
                                                           elif header = BonusTypes.Flat
                                                           then bonusArr
                                                                |> Array.map (fun x -> x.Value)
                                                                |> Array.sum
                                                           else failwith "Unrecognized Pattern of attackBoni in 'addBoniToAttack'"
                                  )
                    |> Array.sum
                critConfirmationRoll + combinedAttackBoni + critSpecificBonus
    
            /////End attack boni/Start damage boni/////
    
            let sizeAdjustedWeaponDamage =
               
                let effectiveSize =
    
                    let changeSizeBy =
                        modifications
                        |> Array.map (fun x -> x.SizeChanges)
                        |> Array.groupBy (fun x -> x.SizeChangeBonustype)
                        |> Array.map (fun (header,bonusArr) -> if header <> BonusTypes.Flat 
                                                               then bonusArr
                                                                    |> Array.map (fun x -> x.SizeChangeValue)
                                                                    |> Array.max
                                                               elif header = BonusTypes.Flat
                                                               then bonusArr
                                                                    |> Array.map (fun x -> x.SizeChangeValue)
                                                                    |> Array.sum
                                                               else failwith "Unrecognized Pattern of sizeChangeBoni." 
                                     )
                        |> Array.sum
    
                    (startSize + changeSizeBy)
                    |> fun x -> if x > 9 then 9
                                elif x < 1 then 1
                                else x
    
                let diceRow = 
                    [|(1,1);(1,2);(1,3);(1,4);(1,6);(1,8);(1,10);(2,6);(2,8);(3,6);(3,8);
                    (4,6);(4,8);(6,6);(6,8);(8,6);(8,8);(12,6);(12,8);(16,6);(16,8);(24,6);(24,8);(36,6);(36,8)|] 
    
                ///https://paizo.com/paizo/faq/v5748nruor1fm#v5748eaic9t3f
                let getSizeChange reCalcWeapon (startS: int) (modifiedS: int) =
                    let snowFlakeIncrease numberofdice (die: int) =
                        match numberofdice with 
                        | 1 -> 2,die
                        | _ -> (numberofdice + int (floor (float numberofdice)*(1./3.))), die
                    let snowFlakeDecrease numberofdice (die: int) =
                        match numberofdice with 
                        | 2 -> 1,die
                        | _ -> (numberofdice - int (floor (float numberofdice)*(1./3.))), die
                    let isEven x = (x % 2) = 0         
                    let isOdd x = (x % 2) = 1
                    let sizeDiff = modifiedS - startS
                    let decInc = if sizeDiff < 0 then (-1.)
                                 elif sizeDiff > 0 then (1.)
                                 else 0.
                    let adjustedDie = match reCalcWeapon.Die with
                                      | 2 -> reCalcWeapon.NumberOfDie, reCalcWeapon.Die
                                      | 3 -> reCalcWeapon.NumberOfDie, reCalcWeapon.Die
                                      | 4 -> match reCalcWeapon.NumberOfDie with
                                             | 1                                                        -> reCalcWeapon.NumberOfDie, reCalcWeapon.Die
                                             | odd when isOdd reCalcWeapon.NumberOfDie = true    -> int (ceil (float reCalcWeapon.NumberOfDie/2.)), 6
                                             | even when isEven reCalcWeapon.NumberOfDie = true  -> (reCalcWeapon.NumberOfDie/2), 8
                                             | _                                                        -> failwith "unknown combination for reCalcWeapon damage dice calculator accoringly to size; Error4"
                                      | 6 -> reCalcWeapon.NumberOfDie, reCalcWeapon.Die
                                      | 8 -> reCalcWeapon.NumberOfDie, reCalcWeapon.Die
                                      | 10 -> reCalcWeapon.NumberOfDie, reCalcWeapon.Die
                                      | 12 -> (reCalcWeapon.NumberOfDie*2), 6
                                      | 20 -> (reCalcWeapon.NumberOfDie*2), 10
                                      | _ -> if reCalcWeapon.Die % 10 = 0
                                             then ((reCalcWeapon.Die / 10) * reCalcWeapon.NumberOfDie), 10
                                             elif reCalcWeapon.Die % 6 = 0
                                             then ((reCalcWeapon.Die / 6) * reCalcWeapon.NumberOfDie), 6
                                             elif reCalcWeapon.Die % 4 = 0 
                                             then ((reCalcWeapon.Die / 4) * reCalcWeapon.NumberOfDie), 4
                                             else reCalcWeapon.NumberOfDie, reCalcWeapon.Die
                    let adjustedDieNum = fst adjustedDie
                    let adjustedDietype = snd adjustedDie
    
                    let rec loopResizeWeapon (n:int) (nDice:int) (die:int) = 
    
                        let stepIncrease = if startS + (int decInc*n) < 5 || (nDice * die) < 6 
                                           then 1
                                           else 2
                        let stepDecrease = if startS + (int decInc*n) < 6 || (nDice * die) < 8 
                                           then 1
                                           else 2
                        let findRowPosition =
                            Array.tryFindIndex (fun (x,y) -> x = nDice && y = die) diceRow
                        if sizeDiff = 0 || n >= abs sizeDiff
                        then nDice, die
                        else findRowPosition
                             |> fun x -> if (x.IsSome) 
                                         then match decInc with 
                                              | dec when decInc < 0. -> if x.Value < 1 then diceRow.[0] else diceRow.[x.Value - stepDecrease]
                                              | inc when decInc > 0. -> if x.Value > (diceRow.Length-3) then (snowFlakeIncrease nDice die) else diceRow.[x.Value + stepIncrease]
                                              | _ -> failwith "unknown combination for reCalcWeapon damage dice calculator accoringly to size; Error1"
                                         elif x.IsSome = false 
                                         then match decInc with 
                                              | dec when decInc < 0. -> snowFlakeDecrease nDice die
                                              | inc when decInc > 0. -> snowFlakeIncrease nDice die
                                              | _ -> failwith "unknown combination for reCalcWeapon damage dice calculator accoringly to size; Error2"
                                         else failwith "unknown combination for reCalcWeapon damage dice calculator accoringly to size; Error3"
                             |> fun (nDie,die) -> loopResizeWeapon (n+1) nDie die
    
                    loopResizeWeapon 0 adjustedDieNum adjustedDietype
                    |> fun (n,die) -> createURLDamage n die reCalcWeapon.BonusDamage reCalcWeapon.DamageType
    
                getSizeChange urlAttack.WeaponDamage startSize effectiveSize
    
            let damageRolls =
                let getDamageRolls =
                    rollDice 1000 sizeAdjustedWeaponDamage.Die
                [|for i=1 to sizeAdjustedWeaponDamage.NumberOfDie do
                    yield getRndArrElement getDamageRolls|]
                |> Array.sum
    
            let modificationDamageBoni =
                modificationArr
                |> Array.map (fun x -> x.BonusDamage)
                |> Array.groupBy (fun x -> x.BonusType)
                |> Array.map (fun (x,bonusArr) -> if x <> BonusTypes.Flat 
                                                  then bonusArr
                                                       |> Array.map (fun x -> x.Value)
                                                       |> Array.max
                                                  else bonusArr
                                                       |> Array.map (fun x -> x.Value)
                                                       |> Array.sum                   
                              )
                |> Array.sum
    
            let totalDamage =
                damageRolls + sizeAdjustedWeaponDamage.BonusDamage + modificationDamageBoni
                //the next line sets a minimum dmg of 1 for all attacks. the additional "(urlAttack.WeaponDamage.NumberOfDie <> 0)" circumvents a 1 dmg attack, if the attack is not meant to deal any attack.
                |> fun x -> if (x <= 0) && (urlAttack.WeaponDamage.NumberOfDie <> 0) then 1 else x
    
            let extraDamageOnHit =
                let getDamageRolls numberOfDie die=
                    let rolledDice = rollDice 1000 die
                    [|for i=1 to numberOfDie do
                        yield getRndArrElement rolledDice|]
                    |> Array.sum
                let extraDamageModifications =
                    modificationArr 
                    |> Array.map (fun x -> x.ExtraDamage.OnHit,x.Name)
                    |> Array.map (fun (extraDmg,str) -> getDamageRolls extraDmg.NumberOfDie extraDmg.Die
                                                        , extraDmg.DamageType, str
                                 )
                    ///Vital Strike hardcode
                    |> fun extraDmg -> if (Array.exists (fun modi -> modi = Modifications.VitalStrike
                                                                     || modi = Modifications.VitalStrikeImproved
                                                                     || modi = Modifications.VitalStrikeGreater) modifications 
                                          ) = true
                                       then Array.filter (fun (x:AttackModification) -> x.ExtraDamage.OnHit.DamageType = VitalStrikeDamage) modifications
                                            |> Array.sortByDescending (fun x -> x.ExtraDamage.OnHit.NumberOfDie)
                                            |> Array.head
                                            |> fun vitalS -> [|for i in 1 .. vitalS.ExtraDamage.OnHit.NumberOfDie do
                                                                yield getDamageRolls sizeAdjustedWeaponDamage.NumberOfDie sizeAdjustedWeaponDamage.Die|], vitalS.Name
                                            |> fun x -> x
                                            |> fun (intList,str) -> Array.sum intList, str
                                            |> fun (bonus,str) -> Array.append [|bonus,VitalStrikeDamage,str|] extraDmg
                                       else extraDmg
                    |> Array.map (fun (bonusValue,dmgType,modificationName) -> bonusValue,(string dmgType))
                //add weapon extra dmg (e.g. shocking enchantment) to modification extra dmg
                extraDamageModifications
                |> Array.filter (fun (extraDmgValue,dType) -> extraDmgValue <> 0 )
    
            let extraDamageOnCrit =
                let getDamageRolls numberOfDie die=
                    let rolledDice = rollDice 1000 die
                    [|for i=1 to numberOfDie do
                        yield getRndArrElement rolledDice|]
                    |> Array.sum
                let extraDamageModifications =
                    if (Array.contains attackRoll urlAttack.CriticalRange) = false
                    then [||]
                    else modificationArr 
                         |> Array.map (fun x -> x.ExtraDamage.OnCrit,x.Name)
                         |> Array.map (fun (extraDmg,str) -> getDamageRolls extraDmg.NumberOfDie extraDmg.Die
                                                             , extraDmg.DamageType, str
                                      )
                         ///Vital Strike hardcode
                         |> fun extraDmg -> if (Array.exists (fun modi -> modi = Modifications.VitalStrike
                                                                          || modi = Modifications.VitalStrikeImproved
                                                                          || modi = Modifications.VitalStrikeGreater) modifications 
                                               ) = true
                                            then Array.filter (fun (x:AttackModification) -> x.ExtraDamage.OnHit.DamageType = VitalStrikeDamage) modifications
                                                 |> Array.sortByDescending (fun x -> x.ExtraDamage.OnHit.NumberOfDie)
                                                 |> Array.head
                                                 |> fun vitalS -> [|for i in 1 .. vitalS.ExtraDamage.OnHit.NumberOfDie do
                                                                     yield getDamageRolls sizeAdjustedWeaponDamage.NumberOfDie sizeAdjustedWeaponDamage.Die|], vitalS.Name
                                                 |> fun x -> x
                                                 |> fun (intList,str) -> Array.sum intList, str
                                                 |> fun (bonus,str) -> Array.append [|bonus,VitalStrikeDamage,str|] extraDmg
                                            else extraDmg
                         |> Array.map (fun (bonusValue,dmgType,modificationName) -> bonusValue,(string dmgType))
                //add weapon extra dmg (e.g. shocking enchantment) to modification extra dmg
                extraDamageModifications
                |> Array.filter (fun (extraDmgValue,dType) -> extraDmgValue <> 0 )

            let extraDamageCombined =
                let getDamageRolls numberOfDie die=
                    let rolledDice = rollDice 1000 die
                    [|for i=1 to numberOfDie do
                        yield getRndArrElement rolledDice|]
                    |> Array.sum
                if extraDamageOnCrit = [||]
                then extraDamageOnHit
                    |> Array.append [|(getDamageRolls urlAttack.ExtraDamage.NumberOfDie urlAttack.ExtraDamage.Die, urlAttack.ExtraDamage.DamageType)|]
                    |> Array.filter (fun (extraDmgValue,dType) -> extraDmgValue <> 0 )
                else Array.map2 (fun (onHit:(int*string)) (onCrit:(int*string)) -> (fst onHit) + (fst onCrit), snd onHit) extraDamageOnHit extraDamageOnCrit
                    |> Array.append [|(getDamageRolls urlAttack.ExtraDamage.NumberOfDie urlAttack.ExtraDamage.Die, urlAttack.ExtraDamage.DamageType)|]
                    |> Array.filter (fun (extraDmgValue,dType) -> extraDmgValue <> 0 ) 

            let extraDamageToString extraDmgArr= 
                extraDmgArr
                |> Array.map (fun (value,dType) -> "+" + (string value) + " " + (string dType) + " " + "damage"  + ", ")
                |> Array.fold (fun strArr x -> strArr + x) "" 
                |> fun x -> x.TrimEnd [|' ';','|]    
    
            let additionalInfoString =
                if urlAttack.AdditionalEffects = ""
                then ""
                else "plus " + urlAttack.AdditionalEffects
    
            ////
            if (Array.contains attackRoll urlAttack.CriticalRange) = false && extraDamageCombined = [||]
                then printfn "You attack with a %s and hit with a %i (rolled %i) for %i damage %s!" urlAttack.WeaponName totalAttackBonus attackRoll totalDamage additionalInfoString
            elif (Array.contains attackRoll urlAttack.CriticalRange) = true && extraDamageCombined = [||] 
                then printfn "You attack with a %s and (hopefully) critically hit the enemy with a %i (rolled %i) and confirm your crit with a %i (rolled %i) for %i Damage (x %i) %s!" urlAttack.WeaponName totalAttackBonus attackRoll totalAttackCritBonus critConfirmationRoll totalDamage urlAttack.CriticalModifier additionalInfoString
            elif (Array.contains attackRoll urlAttack.CriticalRange) = false && extraDamageCombined <> [||]
                then printfn "You attack with a %s and hit the enemy with a %i (rolled %i) for %i damage %s %s!" urlAttack.WeaponName totalAttackBonus attackRoll totalDamage (extraDamageToString extraDamageCombined) additionalInfoString
            elif (Array.contains attackRoll urlAttack.CriticalRange) = true && extraDamageCombined <> [||] 
                then printfn ("You attack with a %s and (hopefully) critically hit the enemy with a %i (rolled %i) and confirm your crit with a %i (rolled %i) for %i damage (x %i) %s (%s on a crit) / (%s when not confirmed) !") urlAttack.WeaponName totalAttackBonus attackRoll totalAttackCritBonus critConfirmationRoll totalDamage urlAttack.CriticalModifier additionalInfoString (extraDamageToString extraDamageCombined) (extraDamageToString extraDamageOnHit)  
                else printfn "You should not see this message, please open an issue with your input as a bug report"
        attackArr
        |> Array.mapi (fun i (attackBonus,attack) -> calculateOneAttack attackBonus attack modificationsCombined.[i])
