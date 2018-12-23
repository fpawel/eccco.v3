module db



open System
open System.IO
open System.Data.SQLite
open System.Data
open System.Diagnostics
open System.Data.SqlClient
open System.Windows.Forms

//let fileName = @"E:\Program Data\Аналитприбор\elco\elco.sqlite"
//let repository_path = @"E:\User\Projects\VS2018\EccCO.v2\App\bin\Release" 
// @"C:\Users\fpawel\Documents\Visual Studio 2015\Projects\Analit\EccCO.v2\App\bin\Release" 

module old =     
    type PartyInfo = DataModel.BatchInfo
    type Product = DataModel.Product
    type Party = DataModel.Batch

type private env = Environment

type Context = 
    {   conn : SQLiteConnection
        importedOldParties:Set<string>
        oldParties : old.PartyInfo list
    }

type private dbt = DbType

type SQLiteCommand with
    member cmd.AddValue name valueType (value:obj) = 
        cmd.Parameters.Add(name, valueType).Value <- value

    member cmd.AddParam (t:dbt) prm (v:'T option) =
        match v with
        | Some a -> 
            cmd.Parameters.Add(prm, t).Value <- a
        | _ -> 
            cmd.Parameters.Add(prm, DbType.Object).Value <- null

    member cmd.AddDecimalParam =
        cmd.AddParam DbType.Decimal
        

let exportProduct x (party_id:int64) (place:int) (old_product:old.Product) (old_party:old.Party) =         
    match old_product.Serial with
    | None -> ()
    | Some serial ->

    let ctm = Alchemy.getCalcTermoMethod old_party old_product
    let ctm2 = 
        match Var.productTypes |> Seq.tryFind(fun x -> x.Name = old_party.ProductType) with
        | Some x -> x.CalculateTermoMethod
        | _ -> Var.productTypes.[0].CalculateTermoMethod
        
    let cmd = new SQLiteCommand(x.conn)
    cmd.CommandText <- """
            INSERT INTO product
                (party_id, serial, place, product_type_name,  
                    i13, i24, i35, i26, i17, not_measured, 

                    i_f_minus20, i_f_plus20, i_f_plus50, i_s_minus20, i_s_plus20, i_s_plus50,

                    firmware, production, old_product_id, old_serial, points_method)
            VALUES        
                (@party_id, @serial, @place, @product_type_name,  
                @i13, @i24, @i35, @i26, @i17, @not_measured, 
                @i_f_minus20, @i_f_plus20, @i_f_plus50, @i_s_minus20, @i_s_plus20, @i_s_plus50,
                @firmware, @production, @old_product_id, @old_serial, @points_method);
                SELECT last_insert_rowid();"""

    cmd.Parameters.Add("@party_id", dbt.Int64).Value <- party_id
    if old_party.Products |> List.exists ( fun x -> x.N < old_product.N && x.Serial = Some serial) then
        cmd.AddValue "@serial" dbt.Object null        
    else
        cmd.AddValue "@serial" dbt.Int32 serial
    cmd.AddValue "@place" dbt.Int32 place    
    cmd.AddParam dbt.String "@product_type_name" 
        (   if old_product.ProductType = "" then 
                None 
            else Some old_product.ProductType
        )

    [   "@i13", old_product.I13
        "@i24", old_product.I24
        "@i35", old_product.I35
        "@i26", old_product.I26
        "@i17", old_product.I17
        "@not_measured", old_product.In

        "@i_f_minus20", old_product.If_20
        "@i_f_plus20", old_product.Ifon
        "@i_f_plus50", old_product.If50
        "@i_s_minus20", old_product.Is_20
        "@i_s_plus20", old_product.Isns
        "@i_s_plus50", old_product.Is50 ] |> List.iter(fun (k,v) -> cmd.AddDecimalParam k v)

    if old_product.Flash |> Array.exists( (<>) 0xffuy) then 
        cmd.Parameters.AddWithValue("@firmware", old_product.Flash) |> ignore   
    else
        cmd.Parameters.AddWithValue("@firmware", null) |> ignore   
    cmd.Parameters.AddWithValue("@production", old_product.IsReportIncluded) |> ignore
    cmd.Parameters.AddWithValue("@old_product_id", old_product.Id) |> ignore
    cmd.Parameters.AddWithValue("@old_serial", serial) |> ignore

    if ctm <> ctm2 then
        cmd.Parameters.Add("@points_method", DbType.Int64).Value <- ctm.number
    else
        cmd.Parameters.Add("@points_method", DbType.Object).Value <- null

        
    cmd.ExecuteNonQuery() |> ignore

let exportProductType x (t : Var.ProductType) = 
    let cmd = new SQLiteCommand(x.conn)

    let convGasName = function
        | "H2S" -> "H₂S"
        | "NH3" -> "NH₃"
        | "NO2" -> "NO₂"
        | "Cl2" -> "Cl₂"
        | "SO2" -> "SO₂"
        | "O2" -> "O₂"
        | "НCl" -> "HCl"
        | x -> x
    
    cmd.CommandText <- """            
        INSERT OR
        REPLACE INTO
          product_type (product_type_name,                        
                        gas_name,
                        units_name,
                        scale,
                        noble_metal_content,
                        lifetime_months)
        VALUES (@product_type_name, @gas_name, @units_name, @scale, @noble_metal_content, @lifetime_months);"""

    cmd.Parameters.Add("@product_type_name", DbType.String).Value <- t.Name
    cmd.Parameters.Add("@gas_name", DbType.String).Value <- convGasName t.Gas
    cmd.Parameters.Add("@units_name", DbType.String).Value <- t.Units
    cmd.Parameters.Add("@scale", DbType.Decimal).Value <- t.Scale
    cmd.Parameters.Add("@noble_metal_content", DbType.Decimal).Value <- t.NobleMetalContent
    cmd.Parameters.Add("@lifetime_months", DbType.Int64).Value <- t.LifetimeWarrianty

    cmd.ExecuteNonQuery() |> ignore

let exportParty x old_party_id = 

    let old_party = 
        match Repository.get old_party_id  with
        | Error err -> failwith err 
        | Ok x -> x
        
    let cmd = new SQLiteCommand(x.conn)
    
    let t = 
        match Var.productTypes |> Seq.tryFind(fun x -> x.Name = old_party.ProductType) with
        | Some x -> x
        | _ -> Var.productTypes.[0]

    cmd.CommandText <- """
            
        INSERT INTO party (created_at, old_party_id, product_type_name, concentration1, concentration2, concentration3, note,
        min_fon, max_fon, max_d_fon, min_k_sens20, max_k_sens20,
min_d_temp, max_d_temp, min_k_sens50, max_k_sens50, max_d_not_measured, points_method) 
            VALUES
                (@created_at, @old_party_id, @product_type_name, @concentration1, @concentration2, @concentration3, @note,
                @min_fon, @max_fon, @max_d_fon, @min_k_sens20, @max_k_sens20,
                @min_d_temp, @max_d_temp, @min_k_sens50, @max_k_sens50, @max_d_not_measured, @points_method
                );
        SELECT last_insert_rowid();"""
    cmd.Parameters.Add("@min_fon", DbType.Object).Value <- null 
        
    [   "@max_fon", t.Ifon_max
        "@max_d_fon", t.DeltaIfon_max
        "@min_k_sens20", t.Ksns_min
        "@max_k_sens20", t.Ksns_max
        "@min_k_sens50", t.Ks40_min
        "@max_k_sens50", t.Ks40_max
        "@min_d_temp", t.Delta_t_min
        "@max_d_temp", t.Delta_t_max
        "@max_d_not_measured", t.Delta_nei_max] 
    |> List.iter(fun (k,v) -> cmd.AddDecimalParam k v)
                    
    cmd.Parameters.Add("@created_at", DbType.DateTime).Value <- old_party.Date
    cmd.Parameters.Add("@old_party_id", DbType.String).Value <- old_party.Id
    cmd.Parameters.Add("@product_type_name", DbType.String).Value <- old_party.ProductType        
    cmd.Parameters.Add("@concentration1", DbType.Decimal).Value <- old_party.PGS1
    cmd.Parameters.Add("@concentration2", DbType.Decimal).Value <- old_party.PGS2
    cmd.Parameters.Add("@concentration3", DbType.Decimal).Value <- old_party.PGS3
    if old_party.Name <> "" then 
        cmd.Parameters.Add("@note", DbType.String).Value <- old_party.Name
    else
        cmd.Parameters.Add("@note", DbType.Object).Value <- null      
               
    cmd.Parameters.Add("@points_method", DbType.Int64).Value <- t.CalculateTermoMethod.number
        
    try
        let party_id =cmd.ExecuteScalar() :?> int64
        for place = 0 to old_party.Products.Length - 1 do
            let old_product = old_party.Products.[place]
            exportProduct x party_id place  old_product  old_party 
    with _ ->
        printfn "%A" old_party.ProductType
        reraise()
    
        

let exportParties x = 

    let cmd = new SQLiteCommand(x.conn, CommandText = "BEGIN TRANSACTION;")
    cmd.ExecuteNonQuery() |> ignore

    let startTime = DateTime.Now
    
    let percent  = 
        let count = x.oldParties.Length
        fun i ->                
            ((float i / float count) * 100.) |> round |> int

    let resetCursor =             
        let conx, cony = Console.CursorLeft, Console.CursorTop
        fun () ->
            let conx2 = Console.CursorLeft
            Console.SetCursorPosition(conx, cony)
            printf "%s" (String(' ', conx2 - conx + 1))
            Console.SetCursorPosition(conx, cony)            

    x.oldParties |> List.iteri (fun i old_party ->            
        let s = sprintf "%d %d%s %A  " (i+1) ( percent i) "%" old_party.Date
        if Set.contains old_party.Id x.importedOldParties then
            resetCursor()
            printf "%s: skip" s
        else
            exportParty x old_party.Id 
            resetCursor()
            printf "%s" s 
        )  
    
    cmd.CommandText <- "COMMIT;"
    cmd.ExecuteNonQuery() |> ignore
    printfn ""
    printfn "total time: %A" (DateTime.Now - startTime)

let dbNull = box System.DBNull.Value

let readExportProduct (r:SQLiteDataReader) = 
    let (~%%) (s:string) : decimal option = 
        if r.[s] = box System.DBNull.Value then None else Some (r.[s] :?> float |> decimal)        
    let p = old.Product.createNew (r.["place"] :?> int64 |> int)        
    p.Serial <- 
        if r.["serial"] = dbNull then None else Some (r.["serial"] :?> int64 |> int)
    p.ProductType <- 
        if r.["product_type_name"] = dbNull then "" else r.["product_type_name"] :?> string
    p.If_20 <- %% "i_f_minus20"
    p.Ifon <- %% "i_f_plus20"
    p.If50 <- %% "i_f_plus50"
    p.Is_20 <- %% "i_s_minus20"
    p.Isns <- %% "i_s_plus20"
    p.Is50 <- %% "i_s_plus50"
    p.I13 <- %% "i13"
    p.I24 <- %% "i24"
    p.I35 <- %% "i35"
    p.I26 <- %% "i26"
    p.I17 <- %% "i17"
    p.In <- %% "not_measured"
    p.IsReportIncluded <-  r.["production"] :?> bool
    p.Flash <- 
        if r.["flash"] = dbNull then [||] else r.["flash"] :?> byte[]    
    p



let getExportPartyProducts x party_id = 
    let products = Array.init 96 (fun n -> old.Product.createNew(n) ) 
    let cmd = new SQLiteCommand(x.conn)
    cmd.CommandText <- "SELECT * FROM product WHERE party_id = @party_id;"
    cmd.AddValue "party_id" dbt.Int64 party_id
    let r = cmd.ExecuteReader()
    while r.Read() do            
        let p = readExportProduct r
        if p.N >= 0 && p.N < 96 then
            products.[p.N] <- p
        else 
            failwithf "%A" p 
        let product_id = r.["product_id"] :?> int64
        let serial = r.["serial"] :?> int64
                    
        let cmd = new SQLiteCommand(x.conn)
        cmd.CommandText <- "UPDATE product SET old_product_id = @old_product_id, old_serial = @old_serial WHERE product_id = @product_id;"
        cmd.AddValue "old_product_id" dbt.String (string(p.Id))
        cmd.AddValue "old_serial" dbt.Int64 serial
        cmd.AddValue "product_id" dbt.Int64 product_id
        cmd.ExecuteNonQuery() |> ignore
    r.Close()
    products

let readExportParty x (r:SQLiteDataReader)  =    
    let p = old.Party.createNew ()
    p.Name <- 
        if r.["note"] = dbNull then "" else r.["note"] :?> string
    p.ProductType <- r.["product_type_name"] :?> string
    p.PGS1 <- r.["concentration1"] :?> float |> decimal
    p.PGS2 <- r.["concentration2"] :?> float |> decimal
    p.PGS3 <- r.["concentration3"] :?> float |> decimal
    
    let party_id = r.["party_id"] :?> int64
    p.Products <- 
        getExportPartyProducts x party_id
        |> Array.toList
    p.ProductsSerials <- old.Party.getProductsSerials p.Products

    let cmd = new SQLiteCommand(x.conn)
    cmd.CommandText <- "UPDATE party SET old_party_id = @old_party_id WHERE party_id = @party_id;"
    cmd.AddValue "old_party_id" dbt.String (string(p.Id))
    cmd.AddValue "party_id" dbt.Int64 party_id
    cmd.ExecuteNonQuery() |> ignore
    p


let export () = 

    let appName = "elco"
    
    let dbPath = 
        let mutable s = Environment.GetEnvironmentVariable("MYAPPDATA") 
        if s = "" then
            s <- Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData)
        Path.Combine(s, "Аналитприбор", appName)

    let fileName = Path.Combine(dbPath, appName + ".sqlite" )    

    let fileExitst = File.Exists fileName

    printfn "database: %s" fileName  
    if not fileExitst then
        printfn "create sqlite database file"
        if not <| Directory.Exists dbPath then
            let x = Directory.CreateDirectory dbPath
            assert x.Exists
        SQLiteConnection.CreateFile fileName
        
    let conn = new SQLiteConnection( sprintf "Data Source=%s; Version=3;" fileName)
    try 
        conn.Open() 
    with e ->
        failwith e.Message        
    
    printfn "%A: initialize sqlite database" DateTime.Now
    let cmd = new SQLiteCommand(conn, CommandText = initdb.sql)
    cmd.ExecuteNonQuery() |> ignore
        
    printfn "%A: read exported parties" DateTime.Now
    cmd.CommandText <- "SELECT old_party_id FROM party WHERE old_party_id NOT NULL;"

    let r = cmd.ExecuteReader()    
    let importedParties =
        seq{
            while r.Read() do         
                yield r.["old_party_id"] :?> string            
        } |> Set.ofSeq
    r.Close()

    
    printfn "exported parties: %d" importedParties.Count    

    printfn "%A: read old parties: %A" DateTime.Now appDataDir

    
    let oldParties = 
        Repository.getInfoList ()
        |> List.sortBy(fun x -> x.Date.Ticks * (-1L))

    printfn "%A: old parties: %d" DateTime.Now oldParties.Length 

    let x = {   
        conn = conn
        importedOldParties = importedParties
        oldParties = oldParties
    }

    printfn "%A: start export" DateTime.Now

    
    printfn "%s" "export products types..."
    for t in Var.productTypes do
        exportProductType x t

    exportParties x
    
    //let cmd = new SQLiteCommand(conn)
    //cmd.CommandText <- "SELECT * FROM party WHERE old_party_id ISNULL;"
    //let r = cmd.ExecuteReader()    
    //while r.Read() do         
    //    readExportParty x r        
    //    |> EccCO.v2.Repository.save env.CurrentDirectory 
    //r.Close()

    //let cmd = new SQLiteCommand(conn)
    //cmd.CommandText <- "SELECT * FROM last_party;"
    //conn.Close()

    Process.Start(dbPath) |> ignore