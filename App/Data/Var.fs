module Var

open System
open System.ComponentModel
open System.Collections.ObjectModel

open PropertyChanged

open DataModel


let private mk'code'lst(x : (string*int) list )  =
    let f1 = 
        let x = Map.ofList x
        fun k -> Map.tryFind k x
    let f2 = 
        let x = x |> List.map( fun (x,y) -> y,x) |> Map.ofList 
        fun k -> Map.tryFind k x
    (List.map fst x), f1, f2

let gases, codeOfGas, gasOfCode = 
    [   "CO", 0x11
        "H2S", 0x22
        "NH3", 0x33
        "Cl2", 0x44
        "SO2", 0x55
        "NO2", 0x66
        "O2", 0x88
        "NO", 0x99
        "HCl", 0xAA ]
    |> mk'code'lst

let units, codeOfUnits, unitsOfCode  = 
    [   "мг/м3", 2
        "ppm", 3
        "об. дол. %", 7 ]
    |> mk'code'lst




[<AddINotifyPropertyChangedInterface>]
type ProductType =
    {   mutable Name : string
        mutable Gas : string
        mutable Units : string
        mutable Scale : decimal
        mutable NobleMetalContent : decimal
        mutable LifetimeWarrianty : int 
        mutable Is64 : bool
        mutable CalculateTermoMethod : CalculateTermo.Method 
        mutable Ifon_max : decimal option
        mutable DeltaIfon_max : decimal option
        mutable Ksns_min : decimal option
        mutable Ksns_max : decimal option
        mutable Delta_t_min : decimal option
        mutable Delta_t_max : decimal option
        mutable Ks40_min : decimal option
        mutable Ks40_max : decimal option
        mutable Delta_nei_max : decimal option  
        mutable TermoPoints : CalculateTermo.Item list }

    member x.Name1 = "ИБЯЛ.418425." + x.Name

    member x.Range = 
        sprintf "0-%M" x.Scale

    member x.LifetimeWarrianty1 = x.LifetimeWarrianty + 6

    member x.What = sprintf "%A" x

    member x.CalculateTermoMethod1 
        with get () = CalculateTermo.Method.toString x.CalculateTermoMethod
        and set v = x.CalculateTermoMethod <- CalculateTermo.Method.parse1 v
    
    static member create() = 
        {   Name="035"
            Gas="CO"
            Units="мг/м3"
            Scale=200m
            NobleMetalContent = 0.1626m
            LifetimeWarrianty = 18 
            Is64=false
            CalculateTermoMethod = AppSets.Pt3
            Ifon_max = None
            DeltaIfon_max = None
            Ksns_min = None
            Ksns_max = None
            Delta_t_min = None
            Delta_t_max = None
            Ks40_min = None
            Ks40_max = None
            Delta_nei_max = None  
            TermoPoints = []}

let productTypes, saveProductTypes = TextConfig.jsonConfig "productTypes" <| fun () -> 
    let Pt3 = CalculateTermo.Method.Pt3
    let Pt2 = CalculateTermo.Method.Pt2
    [   {   Name = "035"
            Gas = "CO"
            Units = "мг/м3"
            Scale = 200M
            NobleMetalContent = 0.1626M
            LifetimeWarrianty = 18
            Is64 = false
            CalculateTermoMethod = Pt3
            TermoPoints = []
            Ifon_max = None
            DeltaIfon_max = None
            Ksns_min = None
            Ksns_max = None
            Delta_t_min = None
            Delta_t_max = None
            Ks40_min = None
            Ks40_max = None
            Delta_nei_max = None }         
        

        {   Name = "035(2)"
            Gas = "CO"
            Units = "мг/м3"
            Scale = 200M
            NobleMetalContent = 0.1456M
            LifetimeWarrianty = 12
            Is64 = false
            CalculateTermoMethod = Pt3
            TermoPoints = []
            Ifon_max = None
            DeltaIfon_max = None
            Ksns_min = None
            Ksns_max = None
            Delta_t_min = None
            Delta_t_max = None
            Ks40_min = None
            Ks40_max = None
            Delta_nei_max = None }         
        

        {   Name = "035-59"
            Gas = "CO"
            Units = "об. дол. %"
            Scale = 0.5M
            NobleMetalContent = 0.1891M
            LifetimeWarrianty = 12
            Is64 = false
            CalculateTermoMethod = Pt3
            TermoPoints = []
            Ifon_max = None
            DeltaIfon_max = None
            Ksns_min = None
            Ksns_max = None
            Delta_t_min = None
            Delta_t_max = None
            Ks40_min = None
            Ks40_max = None
            Delta_nei_max = None }         
        

        {   Name = "035-60"
            Gas = "CO"
            Units = "мг/м3"
            Scale = 200M
            NobleMetalContent = 0.1891M
            LifetimeWarrianty = 12
            Is64 = false
            CalculateTermoMethod = Pt3
            TermoPoints = []
            Ifon_max = None
            DeltaIfon_max = None
            Ksns_min = None
            Ksns_max = None
            Delta_t_min = None
            Delta_t_max = None
            Ks40_min = None
            Ks40_max = None
            Delta_nei_max = None }         
        

        {   Name = "035-61"
            Gas = "CO"
            Units = "ppm"
            Scale = 2000M
            NobleMetalContent = 0.1891M
            LifetimeWarrianty = 12
            Is64 = false
            CalculateTermoMethod = Pt3
            TermoPoints = []
            Ifon_max = None
            DeltaIfon_max = None
            Ksns_min = None
            Ksns_max = None
            Delta_t_min = None
            Delta_t_max = None
            Ks40_min = None
            Ks40_max = None
            Delta_nei_max = None }         
        

        {   Name = "035-80"
            Gas = "CO"
            Units = "мг/м3"
            Scale = 200M
            NobleMetalContent = 0.1456M
            LifetimeWarrianty = 12
            Is64 = false
            CalculateTermoMethod = Pt3
            TermoPoints = []
            Ifon_max = Some 1.01M
            DeltaIfon_max = Some 3M
            Ksns_min = Some 0.08M
            Ksns_max = Some 0.175M
            Delta_t_min = Some 0M
            Delta_t_max = Some 2M
            Ks40_min = Some 100M
            Ks40_max = Some 135M
            Delta_nei_max = Some 5M }         
        

        {   Name = "035-81"
            Gas = "CO"
            Units = "мг/м3"
            Scale = 1500M
            NobleMetalContent = 0.1456M
            LifetimeWarrianty = 12
            Is64 = false
            CalculateTermoMethod = Pt3
            TermoPoints = []
            Ifon_max = None
            DeltaIfon_max = None
            Ksns_min = None
            Ksns_max = None
            Delta_t_min = None
            Delta_t_max = None
            Ks40_min = None
            Ks40_max = None
            Delta_nei_max = None }         
        

        {   Name = "035-92"
            Gas = "CO"
            Units = "об. дол. %"
            Scale = 0.5M
            NobleMetalContent = 0.1891M
            LifetimeWarrianty = 12
            Is64 = false
            CalculateTermoMethod = Pt3
            TermoPoints = []
            Ifon_max = None
            DeltaIfon_max = None
            Ksns_min = None
            Ksns_max = None
            Delta_t_min = None
            Delta_t_max = None
            Ks40_min = None
            Ks40_max = None
            Delta_nei_max = None }         
        

        {   Name = "035-93"
            Gas = "CO"
            Units = "млн-1"
            Scale = 200M
            NobleMetalContent = 0.1891M
            LifetimeWarrianty = 12
            Is64 = false
            CalculateTermoMethod = Pt3
            TermoPoints = []
            Ifon_max = None
            DeltaIfon_max = None
            Ksns_min = None
            Ksns_max = None
            Delta_t_min = None
            Delta_t_max = None
            Ks40_min = None
            Ks40_max = None
            Delta_nei_max = None }         
        

        {   Name = "035-94"
            Gas = "CO"
            Units = "млн-1"
            Scale = 2000M
            NobleMetalContent = 0.1891M
            LifetimeWarrianty = 12
            Is64 = false
            CalculateTermoMethod = Pt3
            TermoPoints = []
            Ifon_max = None
            DeltaIfon_max = None
            Ksns_min = None
            Ksns_max = None
            Delta_t_min = None
            Delta_t_max = None
            Ks40_min = None
            Ks40_max = None
            Delta_nei_max = None }         
        

        {   Name = "035-105"
            Gas = "CO"
            Units = "мг/м3"
            Scale = 200M
            NobleMetalContent = 0.1456M
            LifetimeWarrianty = 12
            Is64 = false
            CalculateTermoMethod = Pt3
            TermoPoints = []
            Ifon_max = Some 1.51M
            DeltaIfon_max = Some 3M
            Ksns_min = Some 0.08M
            Ksns_max = Some 0.335M
            Delta_t_min = Some 0M
            Delta_t_max = Some 3M
            Ks40_min = Some 110M
            Ks40_max = Some 145.01M
            Delta_nei_max = None }         
        

        {   Name = "100"
            Gas = "CO"
            Units = "мг/м3"
            Scale = 200M
            NobleMetalContent = 0.0816M
            LifetimeWarrianty = 12
            Is64 = true
            CalculateTermoMethod = Pt3
            TermoPoints = []
            Ifon_max = Some 1M
            DeltaIfon_max = Some 3M
            Ksns_min = Some 0.08M
            Ksns_max = Some 0.175M
            Delta_t_min = Some 0M
            Delta_t_max = Some 3M
            Ks40_min = Some 100M
            Ks40_max = Some 135M
            Delta_nei_max = None }         
        

        {   Name = "100-05"
            Gas = "CO"
            Units = "мг/м3"
            Scale = 50M
            NobleMetalContent = 0.0816M
            LifetimeWarrianty = 12
            Is64 = true
            CalculateTermoMethod = Pt3
            TermoPoints = []
            Ifon_max = None
            DeltaIfon_max = None
            Ksns_min = None
            Ksns_max = None
            Delta_t_min = None
            Delta_t_max = None
            Ks40_min = None
            Ks40_max = None
            Delta_nei_max = None }         
        

        {   Name = "100-10"
            Gas = "CO"
            Units = "мг/м3"
            Scale = 200M
            NobleMetalContent = 0.0816M
            LifetimeWarrianty = 12
            Is64 = true
            CalculateTermoMethod = Pt3
            TermoPoints = []
            Ifon_max = Some 1M
            DeltaIfon_max = Some 3M
            Ksns_min = Some 0.08M
            Ksns_max = Some 0.175M
            Delta_t_min = Some 0M
            Delta_t_max = Some 3M
            Ks40_min = Some 100M
            Ks40_max = Some 135M
            Delta_nei_max = None }         
        

        {   Name = "100-15"
            Gas = "CO"
            Units = "мг/м3"
            Scale = 50M
            NobleMetalContent = 0.0816M
            LifetimeWarrianty = 12
            Is64 = true
            CalculateTermoMethod = Pt3
            TermoPoints = []
            Ifon_max = None
            DeltaIfon_max = None
            Ksns_min = None
            Ksns_max = None
            Delta_t_min = None
            Delta_t_max = None
            Ks40_min = None
            Ks40_max = None
            Delta_nei_max = None }         
        

        {   Name = "035-40"
            Gas = "CO"
            Units = "мг/м3"
            Scale = 200M
            NobleMetalContent = 0.1456M
            LifetimeWarrianty = 12
            Is64 = false
            CalculateTermoMethod = Pt2
            TermoPoints = []
            Ifon_max = Some 1.51M
            DeltaIfon_max = Some 3M
            Ksns_min = Some 0.08M
            Ksns_max = Some 0.335M
            Delta_t_min = Some 0M
            Delta_t_max = Some 3M
            Ks40_min = Some 110M
            Ks40_max = Some 145.1M
            Delta_nei_max = None }         
        

        {   Name = "035-21"
            Gas = "CO"
            Units = "мг/м3"
            Scale = 200M
            NobleMetalContent = 0.1456M
            LifetimeWarrianty = 12
            Is64 = false
            CalculateTermoMethod = Pt2
            TermoPoints = []
            Ifon_max = Some 1.51M
            DeltaIfon_max = Some 3M
            Ksns_min = Some 0.08M
            Ksns_max = Some 0.335M
            Delta_t_min = Some 0M
            Delta_t_max = Some 3M
            Ks40_min = Some 110M
            Ks40_max = Some 145.01M
            Delta_nei_max = None }         
        

        {   Name = "130-01"
            Gas = "CO"
            Units = "мг/м3"
            Scale = 200M
            NobleMetalContent = 0.1626M
            LifetimeWarrianty = 12
            Is64 = false
            CalculateTermoMethod = Pt3
            TermoPoints = []
            Ifon_max = Some 1.01M
            DeltaIfon_max = Some 3M
            Ksns_min = Some 0.08M
            Ksns_max = Some 0.18M
            Delta_t_min = Some 0M
            Delta_t_max = Some 2M
            Ks40_min = Some 100M
            Ks40_max = Some 135M
            Delta_nei_max = Some 5M }         
        

        {   Name = "035-70"
            Gas = "CO"
            Units = "мг/м3"
            Scale = 200M
            NobleMetalContent = 0.1626M
            LifetimeWarrianty = 12
            Is64 = false
            CalculateTermoMethod = Pt2
            TermoPoints = []
            Ifon_max = Some 1.51M
            DeltaIfon_max = Some 3M
            Ksns_min = Some 0.08M
            Ksns_max = Some 0.33M
            Delta_t_min = Some 0M
            Delta_t_max = Some 3M
            Ks40_min = Some 110M
            Ks40_max = Some 146M
            Delta_nei_max = None }         
        

        {   Name = "130-08"
            Gas = "CO"
            Units = "ppm"
            Scale = 100M
            NobleMetalContent = 0.1162M
            LifetimeWarrianty = 12
            Is64 = false
            CalculateTermoMethod = Pt3
            TermoPoints = []
            Ifon_max = Some 1M
            DeltaIfon_max = Some 3M
            Ksns_min = Some 0.08M
            Ksns_max = Some 0.18M
            Delta_t_min = Some 0M
            Delta_t_max = Some 2M
            Ks40_min = Some 100M
            Ks40_max = Some 135M
            Delta_nei_max = Some 5M }         
        

        {   Name = "035-117"
            Gas = "NO2"
            Units = "мг/м3"
            Scale = 200M
            NobleMetalContent = 0.1626M
            LifetimeWarrianty = 18
            Is64 = true
            CalculateTermoMethod = Pt3
            TermoPoints = []
            Ifon_max = None
            DeltaIfon_max = None
            Ksns_min = None
            Ksns_max = None
            Delta_t_min = None
            Delta_t_max = None
            Ks40_min = None
            Ks40_max = None
            Delta_nei_max = None }         
        

        {   Name = "010-18"
            Gas = "O2"
            Units = "об. дол. %"
            Scale = 21M
            NobleMetalContent = 0M
            LifetimeWarrianty = 12
            Is64 = true
            CalculateTermoMethod = Pt3
            TermoPoints = []
            Ifon_max = None
            DeltaIfon_max = None
            Ksns_min = None
            Ksns_max = None
            Delta_t_min = None
            Delta_t_max = None
            Ks40_min = None
            Ks40_max = None
            Delta_nei_max = None }         
        

        {   Name = "010-18"
            Gas = "O2"
            Units = "об. дол. %"
            Scale = 21M
            NobleMetalContent = 0M
            LifetimeWarrianty = 12
            Is64 = true
            CalculateTermoMethod = Pt3
            TermoPoints = []
            Ifon_max = None
            DeltaIfon_max = None
            Ksns_min = None
            Ksns_max = None
            Delta_t_min = None
            Delta_t_max = None
            Ks40_min = None
            Ks40_max = None
            Delta_nei_max = None }         
        

        {   Name = "035-111"
            Gas = "CO"
            Units = "мг/м3"
            Scale = 200M
            NobleMetalContent = 0.1626M
            LifetimeWarrianty = 12
            Is64 = true
            CalculateTermoMethod = Pt3
            TermoPoints = []
            Ifon_max = Some 1M
            DeltaIfon_max = Some 3M
            Ksns_min = Some 0.08M
            Ksns_max = Some 0.175M
            Delta_t_min = Some 0M
            Delta_t_max = Some 3M
            Ks40_min = Some 100M
            Ks40_max = Some 135M
            Delta_nei_max = Some 5M }         
        

        {   Name = "035-111-2"
            Gas = "CO"
            Units = "мг/м3"
            Scale = 200M
            NobleMetalContent = 0.1626M
            LifetimeWarrianty = 18
            Is64 = false
            CalculateTermoMethod = Pt3
            TermoPoints = []
            Ifon_max = None
            DeltaIfon_max = None
            Ksns_min = None
            Ksns_max = None
            Delta_t_min = None
            Delta_t_max = None
            Ks40_min = None
            Ks40_max = None
            Delta_nei_max = None }  
    ] |> ResizeArray<ProductType>

let productTypeOfBatch (x:Batch) = 
    let y = productTypes |> Seq.tryFind(fun prodType -> prodType.Name=x.ProductType)
    match y with
    | None -> ProductType.create()
    | Some y -> y

let productTypeOfProduct x (p:Product) = 
    if p.ProductType="" then productTypeOfBatch x else
    match productTypes |> Seq.tryFind(fun prodType -> prodType.Name=p.ProductType) with
    | Some x -> x
    |  _ -> productTypeOfBatch x

let getSensitive pgs1 pgs3 p =
    let d = abs(pgs3 - pgs1)
    match p.Ifon, p.Isns with
    | Some ifon, Some isns when d>0m -> abs(isns-ifon)/d |> Some
    | _ -> None


let getIfon p = 
    [   -20m,   p.If_20
        20m,    p.Ifon
        50m,    p.If50 ]
    |> List.choose( function t, Some x -> Some(t,x) | _ -> None )

let getK20 b p  = 
    let dConc = abs (b.PGS3 - b.PGS1)
    if dConc=0m then None else
    match p.Ifon, p.Isns with
    | Some i_fon20, Some i_sns20 when i_fon20 <> i_sns20 -> 
        Some <| (i_sns20-i_fon20)/dConc
    | _ -> None

let convertCurrentToKsnsPercent1 i_sns20 i_fon20 i_sns i_fon = 
    100m * abs ( (i_sns-i_fon)/(i_sns20 - i_fon20) )

let convertCurrentToKsnsPercent p  = 
    let none _ _ = None
    match p.Ifon, p.Isns with
    | Some i_fon20, Some i_sns20 when i_fon20 <> i_sns20 -> 
        fun i_sns i_fon -> 
            convertCurrentToKsnsPercent1 i_sns20 i_fon20 i_sns i_fon
            |> Some           
        | _ -> none
    | _ -> none


let getKsnsPoints p =     
    [  -20m,    p.Is_20,    p.If_20
       20m,    p.Isns,     p.Ifon
       50m,    p.Is50,     p.If50 ]
    |> List.choose( function 
        | t, Some(i1), Some(i2) ->
            match convertCurrentToKsnsPercent p i1 i2 with
            | Some k -> Some( t, k )
            | None -> None
        | _ -> None )




