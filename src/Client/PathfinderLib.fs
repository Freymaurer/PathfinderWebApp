namespace PathfinderAttackSimulator

open System

/// This module is made from several sub moduls containing necessary types to create characters, weapons and modifications, as well as librarys for each of those classes.
module Library =

    /// This module contains all necessary types and create functions to operate the StandardAttack and Full-RoundAttack module.
    module AuxLibFunctions =

        type SizeType =
            | Fine
            | Diminuitive
            | Tiny
            | Small
            | Medium
            | Large
            | Huge
            | Gargantuan
            | Colossal
        
        type AbilityScore =
            | Strength
            | Dexterity
            | Constitution
            | Intelligence
            | Wisdom
            | Charisma
            | NoAS

        type DamageTypes =
            | Fire
            | Slashing
            | Bludgeoning
            | Piercing
            | Cold
            | Acid
            | Electricity
            | Untyped
            | BludgeoningOrPiercing
            | BludgeoningOrSlashing
            | BludgeoningOrPiercingOrSlashing
            | PiercingOrSlashing
            | Precision
            | VitalStrikeDamage

        type BonusTypes =
            | Insight
            | Moral
            | Luck
            | Alchemical
            | Profane
            | Sacred
            | Circumstance
            | Flat
            | Size
            | TwoWeaponFightingMalus
            | Polymorph
            | Competence

        type BonusAttacksType =
            | HasteLike
            | TWFLike
            | FlurryOfBlowsLike
            | FlatBA
            | NoBA

        type WeaponType =
            | PrimaryMain
            | Primary
            | Secondary
            | All

        type NaturalManufactured =
            | Natural
            | Manufactured

        type WeaponHanded =
            | OneHanded
            | TwoHanded
            | OffHand
            | NoValidHandling

        type SizeAttributes = {
            SizeModifier : int
            SizeId : int
            Size : SizeType
            }

        type Damage = {
            NumberOfDie : int
            Die : int
            DamageType : DamageTypes
            }

        type DamageHitAndCrit = {
            OnHit : Damage
            OnCrit : Damage
            }

        type WeaponDamageMultiplicator = {
            Hand : WeaponHanded
            Multiplicator : float     
            }

        type UsedModifier = {
            ToHit : AbilityScore
            ToDmg : AbilityScore
            MultiplicatorOnDamage : WeaponDamageMultiplicator
            }

        type BonusAttacks = {
            NumberOfBonusAttacks : int
            TypeOfBonusAttacks : BonusAttacksType
            WeaponTypeWithBonusAttacks : WeaponType
            }

        type StatChange = {
            Attribute : AbilityScore
            AttributeChange : int
            Bonustype : BonusTypes
            }

        type Bonus = {
            Value : int
            BonusType : BonusTypes
            }

        type SizeChange = {
            SizeChangeValue : int
            SizeChangeBonustype : BonusTypes
            EffectiveSizeChange : bool
            }

        type AttackBonusHitAndCrit = {
            OnHit   : Bonus
            OnCrit  : Bonus
            }

        type InputParameter = {
            InfoText : string
            Placeholdertext : string
            }

        /// NoAS 0 Flat if no StatChange, or leave array empty
        let createStatChange att attChange bType = {
            Attribute = att
            AttributeChange = attChange
            Bonustype = bType
            }

        let createWeaponDamageMultiplicator handling multiplicator = {
            Hand = handling
            Multiplicator = multiplicator
            }

        /// 0 Flat if no Bonus
        let createBonus value bType = {
            Value = value
            BonusType = bType
            }

        /// 0 NoBA All if no BonusAttacks. num = number of bonus attacks; bonusAttackType = is meant for calculation of non-stacking effects like magus spell combat and twf
        /// in that case both are TWFLike; appliedToWeaponType = the Weapons that get bonus attacks e.g. haste goes to primaryMain, twf goes to primary, multiattack goes to secondary
        let createBonusAttacks num bonusAttackType appliedToWeaponType= {
            BonusAttacks.NumberOfBonusAttacks = num
            BonusAttacks.TypeOfBonusAttacks = bonusAttackType
            BonusAttacks.WeaponTypeWithBonusAttacks = appliedToWeaponType
            }

        /// 0 0 Untyped if no Damage. num = number of damage die; die is how many sides the damage die has; dType is e.g. Bludgeoning
        let createDamage num die dType = {
            Damage.NumberOfDie = num
            Damage.Die = die
            Damage.DamageType = dType
            }

        /// hitting = Modifier used for hitting; damage = Modifier used for damage calculation; handling referrs to onehanded twohanded etc. necessary to calculate power attack; multiplicator = how often is damage modifier added
        let createUsedModifier hitting damage handling multiplicator = {
            ToHit = hitting
            ToDmg = damage
            MultiplicatorOnDamage = createWeaponDamageMultiplicator handling multiplicator
            }

        let createSizechange value bonusType effectiveSizeChange = {
            SizeChangeValue = value
            SizeChangeBonustype = bonusType
            EffectiveSizeChange = effectiveSizeChange
            }

        /// Turns all letters in uppercase, which makes matchingfunctions more failproof.
        let createStringForLib inputString =
            inputString
            |> String.map (fun x -> Char.ToUpper x)


        let createSizeAttributes modifier id sizeType = {
            SizeModifier = modifier
            SizeId = id
            Size = sizeType
            }

        // the OnHit bonus is applied to all attacks crit or not, whereas the OnCrit bonus is applied to crits IN ADDITION to the OnHit Bonus.
        let createAttackBoniHitAndCrit hitValue hitValueType critValue critValueType = {
            OnHit = createBonus hitValue hitValueType
            OnCrit = createBonus critValue critValueType
            }

        /// first 3 variables are number of for extra damage on normal hits, (1d6 Fire damage = 1 6 Fire).
        /// The latter 3 are used for dmg only(!) applied on crits! (Flaming burst = createDamageHitAndCrit 1 6 Fire 2 10 Fire)
        let createDamageHitAndCrit numberOfDieoOnHit dieOnHit dmgTypeOnHit numberOfDieOnCrit dieOnCrit dmgTypeOnCrit = {
            DamageHitAndCrit.OnHit = createDamage numberOfDieoOnHit dieOnHit dmgTypeOnHit
            DamageHitAndCrit.OnCrit = createDamage numberOfDieOnCrit dieOnCrit dmgTypeOnCrit
            }

        let createInputParameter infotext placeholder = {
            InfoText = infotext
            Placeholdertext = placeholder
            }

        let findSizes = [1,createSizeAttributes 8 1 Fine;
                        2,createSizeAttributes 4 2 Diminuitive;
                        3,createSizeAttributes 2 3 Tiny;
                        4,createSizeAttributes 1 4 Small;
                        5,createSizeAttributes 0 5 Medium;
                        6,createSizeAttributes -1 6 Large;
                        7,createSizeAttributes -2 7 Huge;
                        8,createSizeAttributes -4 8 Gargantuan;
                        9,createSizeAttributes -8 9 Colossal
                        ] |> Map.ofSeq

        let checkEmpty (inputArr:string []) (baseInputArr:string []) =
            if Array.isEmpty inputArr then baseInputArr else inputArr

        type CharacterStats = {
            CharacterName : string
            BAB : int
            Strength : int
            Dexterity : int
            Constitution: int
            Intelligence: int
            Wisdom: int
            Charisma: int
            CasterLevel1 : int
            CasterLevel2 : int
            CharacterDescription : string
            }

        type Weapon = {
            Name                    : string
            Damage                  : Damage
            DamageBonus             : int
            ExtraDamage             : DamageHitAndCrit
            AttackBonus        : int
            CriticalRange           : int []
            CriticalModifier        : int
            Modifier                : UsedModifier
            ManufacturedOrNatural   : NaturalManufactured
            Description             : string
            }

        type AttackModification = {
            Name                : string
            BonusAttacks        : BonusAttacks
            AttackBonus    : AttackBonusHitAndCrit
            BonusDamage         : Bonus
            ExtraDamage         : DamageHitAndCrit
            AppliedTo           : WeaponType [] * int
            StatChanges         : StatChange []
            SizeChanges         : SizeChange
            Description         : string
            WebInputParameter   : InputParameter []
            }

        let createCharacterStats name bab str dex con int wis cha cs1 cs2 charDesc= {
            CharacterName        = name
            BAB                  = bab
            Strength             = str
            Dexterity            = dex
            Constitution         = con
            Intelligence         = int
            Wisdom               = wis
            Charisma             = cha
            CasterLevel1         = cs1
            CasterLevel2         = cs2
            CharacterDescription = charDesc
            }

        let createWeapon name dmgNOfDice dmgDie dmgType dmgBonus extradmgNOfDice extradmgDie extradmgType extraCritdmgNOfDice extraCritdmgDie extraCritdmgType attackBonus critStart critEnd critMod toHit toDmg handling dmgMod manuNat desc = {
            Name                = name
            Damage              = createDamage dmgNOfDice dmgDie dmgType
            DamageBonus         = dmgBonus
            ExtraDamage         = createDamageHitAndCrit extradmgNOfDice extradmgDie extradmgType extraCritdmgNOfDice extraCritdmgDie extraCritdmgType
            AttackBonus    = attackBonus
            CriticalRange       = [|critStart .. critEnd|]
            CriticalModifier    = critMod
            Modifier            = createUsedModifier toHit toDmg handling dmgMod
            ManufacturedOrNatural = manuNat
            Description         = desc
            }

        //let createModification name baN baType baApplied barOnHit barOnHitType barOnCrit barOnCritType bd bdTpe
        //                       extradmgNOfDice extradmgDie extradmgType extraCritdmgNOfDice extraCritdmgDie extraCritdmgType
        //                       appliedtoArr nOfApplied = {
        //    Name = "Multiattack"
        //    BonusAttacks = createBonusAttacks 0 NoBA All
        //    AttackBonus = createAttackBoniHitAndCrit 3 Flat 0 Flat
        //    BonusDamage = createBonus 0 BonusTypes.Flat
        //    ExtraDamage = createDamageHitAndCrit 0 0 Untyped 0 0 Untyped
        //    AppliedTo = [|Secondary|], -20
        //    StatChanges = [||]
        //    SizeChanges = createSizechange 0 Flat false
        //    Description = "Gives all secondary attacks +3 on attack rolls, effectively reducing the attack penality for secondary attacks to -2"
        //    WebInputParameter = [||]
        //    }

    /// library for all prebuild characters; this is mostly for personal use or functions as an example
    module Characters =

        open AuxLibFunctions

        ///Stats for Character
        let myParrn = {
            CharacterName = "Parrn"
            BAB = 6
            Strength = 22
            Dexterity = 10
            Constitution = 10
            Intelligence = 10
            Wisdom = 10
            Charisma = 10
            CasterLevel1 = 0
            CasterLevel2 = 0
            CharacterDescription = "a nice guy"
            }


        let myTumor = {
            CharacterName = "Stephano"
            BAB = 6
            Strength = 6
            Dexterity = 12
            Constitution = 10
            Intelligence = 10
            Wisdom = 10
            Charisma = 10
            CasterLevel1 = 0
            CasterLevel2 = 0
            CharacterDescription = "a nice tumor"
            }

        let myElemental = {
            CharacterName = "Michelangelo"
            BAB = 6
            Strength = 12
            Dexterity = 14
            Constitution = 12
            Intelligence = 10
            Wisdom = 10
            Charisma = 10
            CasterLevel1 = 0
            CasterLevel2 = 0
            CharacterDescription = "a nice elemental"
            }

    /// Library for all pre-written weapons; this is mostly for personal use or meant as an example
    module Weapons =

        open AuxLibFunctions

        let glaiveGuisarmePlus1FlamingBurst =  {
            Name                = "Glaive-Guisarme +1 flaming"
            Damage              = createDamage 1 10 Slashing
            DamageBonus         = 1
            ExtraDamage         = createDamageHitAndCrit 1 6 Fire 2 10 Fire
            AttackBonus    = 1
            CriticalRange       = [|20|]
            CriticalModifier    = 3
            Modifier            = createUsedModifier Strength Strength TwoHanded 1.5
            ManufacturedOrNatural = Manufactured
            Description         = ""
            }

        let greatswordParrn = {
            Name                = "Large +1 Keen Greatsword"
            Damage              = createDamage 3 6 Slashing
            DamageBonus         = 1
            ExtraDamage         = createDamageHitAndCrit 0 0 Untyped 0 0 Untyped
            AttackBonus    = 1
            CriticalRange       = [|17;18;19;20|]
            CriticalModifier    = 2
            Modifier            = createUsedModifier Strength Strength TwoHanded 1.5
            ManufacturedOrNatural = Manufactured
            Description         = ""
            }

        let mwkSapLarge = {
            Name                = "Masterwork Sap"
            Damage              = createDamage 1 8 Bludgeoning
            DamageBonus         = 0
            ExtraDamage         = createDamageHitAndCrit 0 0 Untyped 0 0 Untyped
            AttackBonus    = 1
            CriticalRange       = [|20|]
            CriticalModifier    = 2
            Modifier            = createUsedModifier Strength Strength OneHanded 1.
            ManufacturedOrNatural = Manufactured
            Description         = ""
            }

        let mwkSapHuge = {
            Name                = "Masterwork Sap, huge"
            Damage              = createDamage 2 6 Bludgeoning
            DamageBonus         = 0
            ExtraDamage         = createDamageHitAndCrit 0 0 Untyped 0 0 Untyped
            AttackBonus    = 1
            CriticalRange       = [|20|]
            CriticalModifier    = 2
            Modifier            = createUsedModifier Strength Strength OneHanded 1.
            ManufacturedOrNatural = Manufactured
            Description         = ""
            }

        let butchersAxe = {
            Name                = "Butchers Axe"
            Damage              = createDamage 3 6 Slashing
            DamageBonus         = 0
            ExtraDamage         = createDamageHitAndCrit 0 0 Untyped 0 0 Untyped
            AttackBonus    = 0
            CriticalRange       = [|20|]
            CriticalModifier    = 3
            Modifier            = createUsedModifier Strength Strength TwoHanded 1.5
            ManufacturedOrNatural = Manufactured
            Description         = ""
            }

        let mwkRapier = {
            Name                = "Mwk Rapier"
            Damage              = createDamage 1 6 Piercing
            DamageBonus         = 0
            ExtraDamage         = createDamageHitAndCrit 0 0 Untyped 0 0 Untyped
            AttackBonus    = 1
            CriticalRange       = [|18;19;20|]
            CriticalModifier    = 2
            Modifier            = createUsedModifier Dexterity Strength OneHanded 1.
            ManufacturedOrNatural = Manufactured
            Description         = ""
            }

        let enchantedLongswordElemental = {
            Name                = "+1 Longsword"
            Damage              = createDamage 1 6 Slashing
            DamageBonus         = 1
            ExtraDamage         = createDamageHitAndCrit 0 0 Untyped 0 0 Untyped
            AttackBonus    = 1
            CriticalRange       = [|19;20|]
            CriticalModifier    = 2
            Modifier            = createUsedModifier Strength Strength OneHanded 1.
            ManufacturedOrNatural = Manufactured
            Description         = ""
            }

        let talonsTumor = {
            Name                = "Talons"
            Damage              = createDamage 1 3 Piercing
            DamageBonus         = 0
            ExtraDamage         = createDamageHitAndCrit 0 0 Untyped 0 0 Untyped
            AttackBonus    = 0
            CriticalRange       = [|20|]
            CriticalModifier    = 2
            Modifier            = createUsedModifier Dexterity Strength OneHanded 1.
            ManufacturedOrNatural = Natural
            Description         = ""
            }

        let greatswordParrnHuge = {
            Name                = "Huge +1 Keen Greatsword"
            Damage              = createDamage 4 6 Slashing
            DamageBonus         = 1
            ExtraDamage         = createDamageHitAndCrit 0 0 Untyped 0 0 Untyped
            AttackBonus    = 1
            CriticalRange       = [|17;18;19;20|]
            CriticalModifier    = 2
            Modifier            = createUsedModifier Strength Strength TwoHanded 1.5
            ManufacturedOrNatural = Manufactured
            Description         = ""
            }

        let mwkLongbow = {
            Name                = "Mwk Longbow"
            Damage              = createDamage 1 8 Piercing
            DamageBonus         = 0
            ExtraDamage         = createDamageHitAndCrit 0 0 Untyped 0 0 Untyped
            AttackBonus    = 1
            CriticalRange       = [|20|]
            CriticalModifier    = 3
            Modifier            = createUsedModifier Dexterity NoAS OneHanded 1.
            ManufacturedOrNatural = Manufactured
            Description         = ""
            }

        let bite = {
            Name                = "Bite"
            Damage              = createDamage 1 6 BludgeoningOrPiercingOrSlashing
            DamageBonus         = 0
            ExtraDamage         = createDamageHitAndCrit 0 0 Untyped 0 0 Untyped
            AttackBonus    = 0
            CriticalRange       = [|20|]
            CriticalModifier    = 2
            Modifier            = createUsedModifier Strength Strength OneHanded 1.
            ManufacturedOrNatural = Natural
            Description         = ""
            }

        let slamElemental = {
            Name                = "Slam"
            Damage              = createDamage 1 4 Bludgeoning
            DamageBonus         = 0
            ExtraDamage         = createDamageHitAndCrit 0 0 Untyped 0 0 Untyped
            AttackBonus    = 0
            CriticalRange       = [|20|]
            CriticalModifier    = 2
            Modifier            = createUsedModifier Strength Strength OneHanded 1.
            ManufacturedOrNatural = Natural
            Description         = ""
            }

        let claw = {
            Name                = "Claw"
            Damage              = createDamage 1 6 Slashing
            DamageBonus         = 0
            ExtraDamage         = createDamageHitAndCrit 0 0 Untyped 0 0 Untyped
            AttackBonus    = 0
            CriticalRange       = [|20|]
            CriticalModifier    = 2
            Modifier            = createUsedModifier Strength Strength OneHanded 1.
            ManufacturedOrNatural = Natural
            Description         = ""
            }

    /// Library for all pre-written modifications
    module Modifications =
        
        open AuxLibFunctions

        let AidAnother = {
            Name = "Aid Another"
            BonusAttacks = createBonusAttacks 0 NoBA All
            AttackBonus = createAttackBoniHitAndCrit 0 Flat 0 Flat
            BonusDamage = createBonus 0 Flat
            ExtraDamage = createDamageHitAndCrit 0 0 Untyped 0 0 Untyped
            AppliedTo = [|All|], 1
            StatChanges = [||]
            SizeChanges = createSizechange 0 Flat false
            Description = ""
            WebInputParameter = [||]
            }

        let BlessingOfFervorAttackBonus = {
            Name = "Blessing of Fervor (Attack Bonus)"
            BonusAttacks = createBonusAttacks 0 NoBA All
            AttackBonus = createAttackBoniHitAndCrit 2 Flat 0 Flat
            BonusDamage = createBonus 2 Flat
            ExtraDamage = createDamageHitAndCrit 0 0 Untyped 0 0 Untyped
            AppliedTo = [|All|], -20
            StatChanges = [||]
            SizeChanges = createSizechange 0 Flat false
            Description = "Blessing of Fervor with the +2 attack bonus as choice"
            WebInputParameter = [||]
            }
        
        /// use this modification to add fast and easy flat boni to attack rolls or to damage.
        let BonusAttackDamage (inputArr:string []) =
            let arr = checkEmpty inputArr [|"0";"0"|]
            let (input1,input2) =
                arr
                |> fun x -> (int x.[0]),(int x.[1])
            {
            Name = "Variable Bonus For Attack And Damage"
            BonusAttacks = createBonusAttacks 0 NoBA All
            AttackBonus = createAttackBoniHitAndCrit input1 Flat 0 Flat
            BonusDamage = createBonus input2 Flat
            ExtraDamage = createDamageHitAndCrit 0 0 Untyped 0 0 Untyped
            AppliedTo = [|All|], -20
            StatChanges = [||]
            SizeChanges = createSizechange 0 Flat false
            Description = "Use this modification to add fast and easy flat boni to attack rolls or to damage"
            WebInputParameter = [|createInputParameter "Bonus to attack rolls" ".. 2"; createInputParameter "Bonus to damage rolls" ".. 2"|]
            }

        let Charging = {
            Name = "Charge"
            BonusAttacks = createBonusAttacks 0 NoBA All
            AttackBonus = createAttackBoniHitAndCrit 2 Flat 0 Flat
            BonusDamage = createBonus 0 Flat
            ExtraDamage = createDamageHitAndCrit 0 0 Untyped 0 0 Untyped
            AppliedTo = [|All|], -20
            StatChanges = [||]
            SizeChanges = createSizechange 0 Flat false
            Description = "+2 on attack rolls"
            WebInputParameter = [||]
            }

        let CriticalFocus = {
            Name = "Critical Focus"
            BonusAttacks = createBonusAttacks 0 NoBA All
            AttackBonus = createAttackBoniHitAndCrit 0 Flat 4 Flat
            BonusDamage = createBonus 0 Flat
            ExtraDamage = createDamageHitAndCrit 0 0 Untyped 0 0 Untyped
            AppliedTo = [|All|], -20
            StatChanges = [||]
            SizeChanges = createSizechange 0 Flat false
            Description = "Applies +4 to all crits not, as its not able to separate weapons"
            WebInputParameter = [||]
            }

        let DivineFavor = {
            Name = "Divine Favor"
            BonusAttacks = createBonusAttacks 0 NoBA All
            AttackBonus = createAttackBoniHitAndCrit 1 Luck 0 Flat
            BonusDamage = createBonus 1 Luck
            ExtraDamage = createDamageHitAndCrit 0 0 Untyped 0 0 Untyped
            AppliedTo = [|All|], -20
            StatChanges = [||]
            SizeChanges = createSizechange 0 Flat false
            Description = ""
            WebInputParameter = [||]
            }

        let EnlargePerson = {
            Name = "Enlarge Person"
            BonusAttacks = createBonusAttacks 0 NoBA All
            AttackBonus = createAttackBoniHitAndCrit 0 Flat 0 Flat
            BonusDamage = createBonus 0 Flat
            ExtraDamage = createDamageHitAndCrit 0 0 Untyped 0 0 Untyped
            AppliedTo = [|All|], -20
            StatChanges = [|(createStatChange Strength 2 Size);(createStatChange Dexterity -2 Size)|]
            SizeChanges = createSizechange 1 Polymorph false
            Description = ""
            WebInputParameter = [||]
            }

        let Fatigued = {
            Name = "Fatigued"
            BonusAttacks = createBonusAttacks 0 NoBA All
            AttackBonus = createAttackBoniHitAndCrit 0 Flat 0 Flat
            BonusDamage = createBonus 0 Flat
            ExtraDamage = createDamageHitAndCrit 0 0 Untyped 0 0 Untyped
            AppliedTo = [|All|], -20
            StatChanges = [|(createStatChange Strength -2 Flat); (createStatChange Dexterity -2 Size)|]
            SizeChanges = createSizechange 0 Flat false
            Description = ""
            WebInputParameter = [||]
            }

        let Flanking = {
            Name = "Flanking"
            BonusAttacks = createBonusAttacks 0 NoBA All
            AttackBonus = createAttackBoniHitAndCrit 2 Flat 0 Flat
            BonusDamage = createBonus 0 BonusTypes.Flat
            ExtraDamage = createDamageHitAndCrit 0 0 Untyped 0 0 Untyped
            AppliedTo = [|All|], -20
            StatChanges = [||]
            SizeChanges = createSizechange 0 Flat false
            Description = ""
            WebInputParameter = [||]
            }

        let FlurryOfBlows = {
            Name = "Flurry Of Blows"
            BonusAttacks = createBonusAttacks 1 NoBA PrimaryMain
            AttackBonus = createAttackBoniHitAndCrit 0 Flat 0 Flat
            BonusDamage = createBonus 0 BonusTypes.Flat
            ExtraDamage = createDamageHitAndCrit 0 0 Untyped 0 0 Untyped
            AppliedTo = [|All|], -20
            StatChanges = [||]
            SizeChanges = createSizechange 0 Flat false
            Description = ""
            WebInputParameter = [||]
            }

        let FuriousFocus (inputArr:string []) =
            let arr = checkEmpty inputArr [|"0"|]
            let bab =
                arr.[0]
            {
            Name = "Furious Focus"
            BonusAttacks = createBonusAttacks 0 NoBA All
            AttackBonus = createAttackBoniHitAndCrit (int(floor (float bab/4. + 1.))) BonusTypes.Flat 0 Flat
            BonusDamage = createBonus 0 BonusTypes.Flat
            ExtraDamage = createDamageHitAndCrit 0 0 Untyped 0 0 Untyped
            AppliedTo = [|PrimaryMain|], 1
            StatChanges = [||]
            SizeChanges = createSizechange 0 Flat false
            Description = ""
            WebInputParameter = [|createInputParameter "Base Attack Bonus:" ".. 3"|]
            }

        let Haste = {
            Name = "Haste"
            BonusAttacks = createBonusAttacks 1 HasteLike PrimaryMain
            AttackBonus = createAttackBoniHitAndCrit 1 Flat 0 Flat
            BonusDamage = createBonus 0 BonusTypes.Flat
            ExtraDamage = createDamageHitAndCrit 0 0 Untyped 0 0 Untyped
            AppliedTo = [|All|], -20
            StatChanges = [||]
            SizeChanges = createSizechange 0 Flat false
            Description = ""
            WebInputParameter = [||]
            }

        let InspireCourage (inputArr:string []) =
            let arr = checkEmpty inputArr [|"0"|]
            let bardLevel = (int arr.[0])
            let (bonusValue:int) = match bardLevel with
                                   | x when bardLevel >= 17 -> 4
                                   | x when bardLevel >= 11 -> 3
                                   | x when bardLevel >= 5 -> 2
                                   | _ -> 1
            {
            Name = "Inspire Courage"
            BonusAttacks = createBonusAttacks 0 NoBA All
            AttackBonus = createAttackBoniHitAndCrit bonusValue Competence 0 Flat
            BonusDamage = createBonus bonusValue Competence
            ExtraDamage = createDamageHitAndCrit 0 0 Untyped 0 0 Untyped
            AppliedTo = [|All|], -20
            StatChanges = [||]
            SizeChanges = createSizechange 0 Flat false
            Description = "For a set level, because of several IC increasing items"
            WebInputParameter = [|createInputParameter "Bard Level:" ".. 2"|]
            }

        let Invisibility = {
            Name = "Invisibility"
            BonusAttacks = createBonusAttacks 0 NoBA All
            AttackBonus = createAttackBoniHitAndCrit 2 Flat 0 Flat
            BonusDamage = createBonus 0 Flat
            ExtraDamage = createDamageHitAndCrit 0 0 Untyped 0 0 Untyped
            AppliedTo = [|All|], -20
            StatChanges = [||]
            SizeChanges = createSizechange 0 Flat false
            Description = ""
            WebInputParameter = [||]
            }

        let Multiattack =  {
            Name = "Multiattack"
            BonusAttacks = createBonusAttacks 0 NoBA All
            AttackBonus = createAttackBoniHitAndCrit 3 Flat 0 Flat
            BonusDamage = createBonus 0 BonusTypes.Flat
            ExtraDamage = createDamageHitAndCrit 0 0 Untyped 0 0 Untyped
            AppliedTo = [|Secondary|], -20
            StatChanges = [||]
            SizeChanges = createSizechange 0 Flat false
            Description = "Gives all secondary attacks +3 on attack rolls, effectively reducing the attack penality for secondary attacks to -2"
            WebInputParameter = [||]
            }

        let MutagenStrength = {
            Name = "Mutagen, Alchemist (Strength)"
            BonusAttacks = createBonusAttacks 0 NoBA All
            AttackBonus = createAttackBoniHitAndCrit 0 Flat 0 Flat
            BonusDamage = createBonus 0 Flat
            ExtraDamage = createDamageHitAndCrit 0 0 Untyped 0 0 Untyped
            AppliedTo = [|All|], -20
            StatChanges = [|(createStatChange Strength 4 Alchemical); (createStatChange Intelligence -2 Alchemical)|]
            SizeChanges = createSizechange 0 Flat false
            Description = ""
            WebInputParameter = [||]
            }

        let PlanarFocusFire (inputArr:string []) =
            let arr = checkEmpty inputArr [|"0"|]
            let lvl = int arr.[0]
            let NumberOfExtraDie = int (lvl/4) + 1 
            {
            Name = "Planar Focus (Fire)"
            BonusAttacks = createBonusAttacks 0 NoBA All
            AttackBonus = createAttackBoniHitAndCrit 0 Flat 0 Flat
            BonusDamage = createBonus 0 Flat
            ExtraDamage = createDamageHitAndCrit NumberOfExtraDie 6 Fire 0 0 Untyped
            AppliedTo = [|All|], -20
            StatChanges = [||]
            SizeChanges = createSizechange 0 Flat false
            Description = ""
            WebInputParameter = [|createInputParameter "Animal Focus Class lvl:" ".. 2"|]
            }

        let PowerAttack (inputArr:string []) =
            let arr = checkEmpty inputArr [|"0"|]
            let bab = int arr.[0]
            {
            Name = "Power Attack"
            BonusAttacks = createBonusAttacks 0 NoBA All
            AttackBonus = createAttackBoniHitAndCrit (int( - (floor (float bab/4. + 1.)) )) Flat 0 Flat
            BonusDamage = createBonus (int( (floor (float bab/4.) * 2.) + 2. )) BonusTypes.Flat
            ExtraDamage = createDamageHitAndCrit 0 0 Untyped 0 0 Untyped
            AppliedTo = [|All|], -20
            StatChanges = [||]
            SizeChanges = createSizechange 0 Flat false
            Description = "Takes BAB as input."
            WebInputParameter = [|createInputParameter "Base Attack Bonus" ".. 2"|]
            }
        
        let PowerAttackURL (inputArr: string [])=
            let arr = checkEmpty inputArr [|"OneHanded";"0"|]
            let ((handed:WeaponHanded),bab) =
                match arr.[0] with
                | "OneHanded" -> OneHanded
                | "OffHand" -> OffHand
                | "TwoHanded" -> TwoHanded
                | _ ->  NoValidHandling
                , (int arr.[1])
            {
            Name = "Power Attack (For Bestiary Calculator)"
            BonusAttacks = createBonusAttacks 0 NoBA All
            AttackBonus = createAttackBoniHitAndCrit (int( - (floor (float bab/4. + 1.)) )) Flat 0 Flat
            BonusDamage = (floor (float bab/4.) * 2.) + 2. 
                          |> fun x -> match handed with
                                      | TwoHanded -> createBonus (int (x * 1.5)) Flat
                                      | OneHanded -> createBonus (int x) Flat
                                      | OffHand -> createBonus (int (x * 0.5)) Flat
            ExtraDamage = createDamageHitAndCrit 0 0 Untyped 0 0 Untyped
            AppliedTo = [|All|], -20
            StatChanges = [||]
            SizeChanges = createSizechange 0 Flat false
            Description = "Takes OneHanded/TwoHanded/OffHand and BAB as input. Use this only for the calculateURLAttack function."
            WebInputParameter = [|createInputParameter "Weapon Handling:" "OneHanded/TwoHanded/OffHand"; createInputParameter "Base Attack Bonus:" ".. 2"|]
            }        
    
        let Shaken = {
            Name = "Shaken"
            BonusAttacks = createBonusAttacks 0 NoBA All
            AttackBonus = createAttackBoniHitAndCrit -2 Flat 0 Flat
            BonusDamage = createBonus 0 Flat
            ExtraDamage = createDamageHitAndCrit 0 0 Untyped 0 0 Untyped
            AppliedTo = [|All|], -20
            StatChanges = [||]
            SizeChanges = createSizechange 0 Flat false
            Description = ""
            WebInputParameter = [||]
            }

        let ShockingGrasp (inputArr:string [])=
            let arr = checkEmpty inputArr [|"0";"false"|]
            let (casterLevel,metalTF) =
                (int arr.[0]),
                match arr.[1] with
                | "true" | "True" -> true
                | "false" | "False" -> false
                | _ -> failwith "This is not a regocnized true/false input"
            {
            Name = "Shocking Grasp"
            BonusAttacks = createBonusAttacks 0 NoBA All
            AttackBonus = createAttackBoniHitAndCrit (if metalTF = true then 3 else 0) Flat 0 Flat
            BonusDamage = createBonus 0 Flat
            ExtraDamage = createDamageHitAndCrit (if casterLevel > 5 then 5 else casterLevel) 6 Electricity (if casterLevel > 5 then 5 else casterLevel) 6 Electricity
            AppliedTo = [|All|], 1
            StatChanges = [||]
            SizeChanges = createSizechange 0 Flat false
            Description = "Takes casterLevel and true/false for attack metal. Shocking Grasp deals 1d6 / level electricity damage up to a maximum of 5d6."
            WebInputParameter = [|createInputParameter "Caster Level:" ".. 2";createInputParameter "Attacking Metal?" "true=yes/false=no"|]
            }

        let ShockingGraspIntensifiedEmpowered (inputArr:string [])=
            let arr = checkEmpty inputArr [|"0";"false"|]
            let (casterLevel,metalTF) =
                (int arr.[0]),
                match arr.[1] with
                | "true" | "True" -> true
                | "false" | "False" -> false
                | _ -> failwith "This is not a regocnized true/false input"
            {
            Name = "Shocking Grasp Intensified Empowered"
            BonusAttacks = createBonusAttacks 0 NoBA All
            AttackBonus = createAttackBoniHitAndCrit (if metalTF = true then 3 else 0) Flat 0 Flat
            BonusDamage = createBonus 0 Flat
            ExtraDamage = createDamageHitAndCrit ((if casterLevel > 10 then 10 else casterLevel) 
                                                 |> fun x -> x + int (float x * 0.5) ) 6 Electricity
                                                 ((if casterLevel > 10 then 10 else casterLevel) 
                                                 |> fun x -> x + int (float x * 0.5) ) 6 Electricity
            AppliedTo = [|All|], 1
            StatChanges = [||]
            SizeChanges = createSizechange 0 Flat false
            Description = "Takes casterLevel and true/false for attack metal. Shocking Grasp deals 1d6 / level electricity damage up to a maximum of 10d6 for this intensified version. Empowered increases the number of all rolled dice by 50%"
            WebInputParameter = [|createInputParameter "Caster Level:" ".. 2";createInputParameter "Attacking Metal?" "true=yes/false=no"|]
            }

        let SneakAttack (inputArr:string []) =
            let arr = checkEmpty inputArr [|"0"|]
            let rogueLevel =
                arr.[0]
            {
            Name = "Sneak Attack"
            BonusAttacks = createBonusAttacks 0 NoBA All
            AttackBonus = createAttackBoniHitAndCrit 0 Flat 0 Flat
            BonusDamage = createBonus 0 Flat
            ExtraDamage = createDamageHitAndCrit (int (ceil (float rogueLevel/2.))) 6 Precision 0 0 Untyped
            AppliedTo = [|All|], -20
            StatChanges = [||]
            SizeChanges = createSizechange 0 Flat false
            Description = "This assumes full Sneak Attack damage dice progression"
            WebInputParameter = [|createInputParameter "Rogue Level" ".. 2"|]
            }

        let SneakAttackOnce (inputArr:string []) =
            let arr = checkEmpty inputArr [|"0"|]
            let rogueLevel =
                arr.[0]
            {
            Name = "Sneak Attack (Once)"
            BonusAttacks = createBonusAttacks 0 NoBA All
            AttackBonus = createAttackBoniHitAndCrit 0 Flat 0 Flat
            BonusDamage = createBonus 0 BonusTypes.Flat
            ExtraDamage = createDamageHitAndCrit (int (ceil (float rogueLevel/2.))) 6 Precision 0 0 Untyped
            AppliedTo = [|All|], 1        
            StatChanges = [||]
            SizeChanges = createSizechange 0 Flat false
            Description = "Sneak Attack on first attack. This can happen due to a stealth attack or an full-round attack action from invisibility. This assumes full Sneak Attack damage dice progression."
            WebInputParameter = [|createInputParameter "Rogue Level" ".. 2"|]
            }

        ///mit allen als Primary gelisteten Waffen; bisher nur mit -2 auf Treffen
        let TwoWeaponFighting = {
            Name = "Two-Weapon Fighting"
            BonusAttacks = createBonusAttacks 1 TWFLike Primary
            AttackBonus = createAttackBoniHitAndCrit -2 TwoWeaponFightingMalus 0 Flat
            BonusDamage = createBonus 0 BonusTypes.Flat
            ExtraDamage = createDamageHitAndCrit 0 0 Untyped 0 0 Untyped
            AppliedTo = [|Primary; PrimaryMain|], -20
            StatChanges = [||]
            SizeChanges = createSizechange 0 Flat false
            Description = "Gives -2 to all attacks."
            WebInputParameter = [||]
            }    

        /// mit allen als Primary gelisteten Waffen; bisher nur mit -2 auf Treffen
        let TwoWeaponFightingImproved = {
            Name = "Two-Weapon Fighting, Improved"
            BonusAttacks = createBonusAttacks 2 TWFLike Primary
            AttackBonus = createAttackBoniHitAndCrit -2 TwoWeaponFightingMalus 0 Flat
            BonusDamage = createBonus 0 BonusTypes.Flat
            ExtraDamage = createDamageHitAndCrit 0 0 Untyped 0 0 Untyped
            AppliedTo = [|Primary; PrimaryMain|], -20
            StatChanges = [||]
            SizeChanges = createSizechange 0 Flat false
            Description = "Gives -2 to all attacks."
            WebInputParameter = [||]
            }

        /// This modification is hardcoded, so it does not follow normal modification rules
        let VitalStrike = {
            Name = "Vital Strike"
            BonusAttacks = createBonusAttacks 0 NoBA All
            AttackBonus = createAttackBoniHitAndCrit 0 Flat 0 Flat
            BonusDamage = createBonus 0 Flat
            ExtraDamage = createDamageHitAndCrit 1 0 VitalStrikeDamage 0 0 Untyped
            AppliedTo = [|All|], -20
            StatChanges = [||]
            SizeChanges = createSizechange 0 Flat false
            Description = "These extra weapon damage dice are not multiplied on a critical hit, but are added to the total"
            WebInputParameter = [||]
            }
            
        /// This modification is hardcoded, so it does not follow normal modification rules
        let VitalStrikeImproved = {
            Name = "Vital Strike, Improved"
            BonusAttacks = createBonusAttacks 0 NoBA All
            AttackBonus = createAttackBoniHitAndCrit 0 Flat 0 Flat
            BonusDamage = createBonus 0 Flat
            ExtraDamage = createDamageHitAndCrit 2 0 VitalStrikeDamage 0 0 Untyped
            AppliedTo = [|All|], -20
            StatChanges = [||]
            SizeChanges = createSizechange 0 Flat false
            Description = "These extra weapon damage dice are not multiplied on a critical hit, but are added to the total"
            WebInputParameter = [||]
            }

        /// This modification is hardcoded, so it does not follow normal modification rules
        let VitalStrikeGreater = {
            Name = "Vital Strike, Greater"
            BonusAttacks = createBonusAttacks 0 NoBA All
            AttackBonus = createAttackBoniHitAndCrit 0 Flat 0 Flat
            BonusDamage = createBonus 0 Flat
            ExtraDamage = createDamageHitAndCrit 3 0 VitalStrikeDamage 0 0 Untyped
            AppliedTo = [|All|], -20
            StatChanges = [||]
            SizeChanges = createSizechange 0 Flat false
            Description = "These extra weapon damage dice are not multiplied on a critical hit, but are added to the total"
            WebInputParameter = [||]
            }

        let WeaponFocus = {
            Name = "Weapon Focus"
            BonusAttacks = createBonusAttacks 0 NoBA All
            AttackBonus = createAttackBoniHitAndCrit 1 Flat 0 Flat
            BonusDamage = createBonus 0 Flat
            ExtraDamage = createDamageHitAndCrit 0 0 Untyped 0 0 Untyped
            AppliedTo = [|All|], -20
            StatChanges = [||]
            SizeChanges = createSizechange 0 Flat false
            Description = "Only use this if you only use one weapon."
            WebInputParameter = [||]
            }

        let WeaponSpecialization ={
            Name = "WeaponSpecialization"
            BonusAttacks = createBonusAttacks 0 NoBA All
            AttackBonus = createAttackBoniHitAndCrit 0 Flat 0 Flat
            BonusDamage = createBonus 2 Flat
            ExtraDamage = createDamageHitAndCrit 0 0 Untyped 0 0 Untyped
            AppliedTo = [|All|], -20
            StatChanges = [||]
            SizeChanges = createSizechange 0 Flat false
            Description = "Only use this if you only use one weapon."
            WebInputParameter = [||]
            }

        let Wrath = {
            Name = "Wrath"
            BonusAttacks = createBonusAttacks 0 NoBA All
            AttackBonus = createAttackBoniHitAndCrit 1 Moral 0 Flat
            BonusDamage = createBonus 1 Moral
            ExtraDamage = createDamageHitAndCrit 0 0 Untyped 0 0 Untyped
            AppliedTo = [|All|], -20
            StatChanges = [||]
            SizeChanges = createSizechange 0 Flat false
            Description = ""
            WebInputParameter = [||]
            }

        /// Never delete this!! This is 100% necessary for FullRoundAttackAction to function, as it works as a filler for the modificationArrays.
        /// It also functions as a example for a completly empty modification, as this could be added multiple times without changing anything.
        let ZeroMod = {
            Name = ""
            BonusAttacks = createBonusAttacks 0 NoBA All
            AttackBonus = createAttackBoniHitAndCrit 0 Flat 0 Flat
            BonusDamage = createBonus 0 Flat
            ExtraDamage = createDamageHitAndCrit 0 0 Untyped 0 0 Untyped
            AppliedTo = [|All|], -20
            StatChanges = [||]
            SizeChanges = createSizechange 0 Flat false
            Description = ""
            WebInputParameter = [||]
            }
    
    /// This part is still under construction, come back later.
    module Server = 

        open Modifications
        open Weapons
        open Characters
        open AuxLibFunctions

        ///not updated
        let showAll str =  
            let rdyStr = createStringForLib str
            match rdyStr with
            | rdyStr when rdyStr = "MODIFICATIONS" -> [|
                                                        Multiattack;SneakAttackOnce [|"0"|];TwoWeaponFighting;TwoWeaponFightingImproved;Haste;FlurryOfBlows;Shaken;WeaponFocus;EnlargePerson;MutagenStrength;
                                                        Invisibility;PlanarFocusFire [|"0"|];SneakAttack [|"0"|];Wrath;DivineFavor;FuriousFocus [|"0"|];PowerAttack [|"0"|];Flanking;Charging;WeaponSpecialization;Fatigued;
                                                        AidAnother;VitalStrike;VitalStrikeImproved;VitalStrikeGreater;InspireCourage [|"0"|]; ShockingGrasp [|"0";"true"|]; ShockingGraspIntensifiedEmpowered [|"0";"true"|]; PowerAttackURL [|"OneHanded";"0"|];
                                                        BlessingOfFervorAttackBonus; BonusAttackDamage [|"0";"0"|];
                                                      |]
                                                      |> Array.map (fun x -> x.Name)
                                                      |> Array.sortBy (fun x -> x)
            | rdyStr when rdyStr = "WEAPONS" -> [|
                                                    claw;slamElemental;bite;mwkLongbow;greatswordParrnHuge;talonsTumor;enchantedLongswordElemental;
                                                    mwkRapier;butchersAxe;mwkSapHuge;mwkSapLarge;greatswordParrn;glaiveGuisarmePlus1FlamingBurst
                                                |]
                                                |> Array.map (fun x -> x.Name)
                                                |> Array.sortBy (fun x -> x)
            | rdyStr when rdyStr = "CHARACTERS" -> [|
                                                        myElemental;myTumor;myParrn
                                                   |]
                                                   |> Array.map (fun x -> x.CharacterName)
                                                   |> Array.sortBy (fun x -> x)
            | _ -> failwith "Unknown Input. Type *Modifications*, *Weapons* or *Characters* (without the *) to see all related objects in the library."


        let EmptyWeapon = {
                Name                = "Here will be your weapons"
                Damage              = createDamage 1 6 Slashing
                DamageBonus         = 0
                ExtraDamage         = createDamageHitAndCrit 0 0 Untyped 0 0 Untyped
                AttackBonus    = 0
                CriticalRange       = [|20|]
                CriticalModifier    = 2
                Modifier            = createUsedModifier Strength Strength OneHanded 1.
                ManufacturedOrNatural = Manufactured
                Description         = "Psst! The WIZARD told me this is a placeholder!"
                }
    
        let EmptyModification = {
            Name = "Here will be your modifications"
            BonusAttacks = createBonusAttacks 0 NoBA All
            AttackBonus = createAttackBoniHitAndCrit 0 Flat 0 Flat
            BonusDamage = createBonus 0 Flat
            ExtraDamage = createDamageHitAndCrit 0 0 Untyped 0 0 Untyped
            AppliedTo = [|All|], -20
            StatChanges = [||]
            SizeChanges = createSizechange 0 Flat false
            Description = "Program: Modification does not show anything. - User: Alright then, keep your secrets!"
            WebInputParameter = [||]
            }

        /// use this one as empty char for web application as he has an empty string as name
        let EmptyChar = { 
                CharacterName = "Here will be your character"
                BAB = 0
                Strength = 9
                Dexterity = 8
                Constitution = 12
                Intelligence = 6
                Wisdom = 6
                Charisma = 7
                CasterLevel1 = 0
                CasterLevel2 = 0
                CharacterDescription = "The WIZARD does not want you to see his might, so he disguises as commoner!"
                }
    
        //
