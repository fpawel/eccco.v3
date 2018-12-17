#r @"bin\Release\App.exe"

let ctmStr = function
    | AppSets.Pt3 -> "Pt3"
    | AppSets.Pt2 -> "Pt2"

let ff = function
    | Some x -> sprintf "Some %MM" x
    | _ -> "None"

for t in Var.productTypes do
    printfn """
    {   Name = %A
        Gas = %A
        Units = %A
        Scale = %MM
        NobleMetalContent = %MM
        LifetimeWarrianty = %d
        Is64 = %b
        CalculateTermoMethod = %s
        TermoPoints = []
        Ifon_max = %s
        DeltaIfon_max = %s
        Ksns_min = %s
        Ksns_max = %s
        Delta_t_min = %s
        Delta_t_max = %s
        Ks40_min = %s
        Ks40_max = %s
        Delta_nei_max = %s }         
        """ 
        t.Name t.Gas t.Units t.Scale t.NobleMetalContent t.LifetimeWarrianty t.Is64
        (ctmStr t.CalculateTermoMethod)
        (ff t.Ifon_max)
        (ff t.DeltaIfon_max)
        (ff t.Ksns_min)
        (ff t.Ksns_max)
        (ff t.Delta_t_min)
        (ff t.Delta_t_max)
        (ff t.Ks40_min)
        (ff t.Ks40_max)
        (ff t.Delta_nei_max)