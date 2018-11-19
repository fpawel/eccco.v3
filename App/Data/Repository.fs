﻿module Repository

open System
open System.IO
open DataModel

type Id = string

[<AutoOpen>]
 module private Helpers = 
    let usefile<'a> (path:string) (x:FileMode) (f : FileStream -> 'a)= 
        try
            use file = new FileStream( path, x ) 
            f file |> Ok
        with e ->             
            log.Debug ( sprintf "Ошибка обращения к файлу %s, %A, %A" path x e )
            Error e.Message

    type FileName = { FileName : string }

    let readFile<'T> filename (func : BinaryReader -> 'T)  =        
        usefile filename.FileName FileMode.Open <| fun file ->
            use reader = new BinaryReader(file)
            func reader 

    let writeFile<'T> filename (func : BinaryWriter -> unit)  =         
        usefile filename.FileName FileMode.Create <| fun file ->        
            use writer = new BinaryWriter(file)
            func writer 
        |> Either.leftSome

    module Path = 
        let data = 
            let x = Path.Combine( exepath, "Batches" ) 
            if Directory.Exists x |> not then 
                Directory.CreateDirectory x |> ignore
            x
        let baddata = lazy (                
            let x = Path.Combine( exepath, "__CORRUPTED__" ) 
            if Directory.Exists x |> not then 
                Directory.CreateDirectory x |> ignore
            x )
            

        let batchFolder canCreate id (dt:DateTime)  = 
            let (~%%) = string
            let month = dt.ToString("MMM", System.Globalization.CultureInfo.InvariantCulture)
            let path = Path.Combine(data, %% dt.Year, sprintf "%d-%s" dt.Month month, %% dt.Day, id )
            if canCreate then
                createDirectory path
            { FileName = path }

        let batchFileName canCreate id dt  =             
            { FileName = Path.Combine( (batchFolder canCreate id dt).FileName, sprintf "%s.batch"  id ) }
            
    let moveCorrupted (src:string) = 
        let filename = Path.GetFileName src
        log.Warn ( sprintf "Файл %s повреждён!" filename )
        try 
            let dest = ( Path.Combine(Path.baddata.Force(), filename) )
            if File.Exists dest then
                File.Delete dest |> ignore
            File.Move( src, dest ) 
        with e ->
            log.Error( sprintf  "Ошибка переноса файла %s, %A" filename e.Message )
            log.Debug( sprintf  "%A" e)

    let getBatchesInfo() =
        let files = Directory.GetFiles( Path.data, "*.batch", SearchOption.AllDirectories)
        let r = 
            [   for filename in files do
                    match   readFile {FileName=filename} FSharpBin.deserialize<BatchInfo>  with
                    | Error x ->  moveCorrupted filename                    
                    | Ok x -> yield x ]   
        r


module Path =
    let year year = 
        let x = Path.batchFolder false "-" (DateTime(year,1,1) )
        x.FileName 
        |> Path.GetDirectoryName
        |> Path.GetDirectoryName
        |> Path.GetDirectoryName
    let month year month = 
        let x = Path.batchFolder false "-" (DateTime(year,month,1) )
        x.FileName 
        |> Path.GetDirectoryName
        |> Path.GetDirectoryName

    let day year month day = 
        let x = Path.batchFolder false "-" (DateTime(year,month,day) )
        x.FileName 
        |> Path.GetDirectoryName

    let batch id dateTime = 
        (Path.batchFolder false id dateTime).FileName
  
[<AutoOpen>]
module private Helpers1 = 
    let batchesInfo = ref ( getBatchesInfo() )

    let load id dt =
        let fileName = Path.batchFileName false id dt
        if File.Exists fileName.FileName |> not then
            batchesInfo := !batchesInfo |> List.filter( fun y -> y.Id <> id)
            Error ("не найден файл " + fileName.FileName)
        else
            readFile fileName <| fun reader ->
                let x = FSharpBin.deserialize<Batch> reader
                {   x with 
                        Products = 
                            [   for p in x.Products -> 
                                    if p.Flash.Length = EEPROM_SIZE then p else
                                    { p with Flash = Array.create EEPROM_SIZE 0xffuy}] }
                
                

    let save batch = 
        let batchInfo = BatchInfo.creteNew batch
        writeFile (Path.batchFileName true batchInfo.Id batchInfo.Date) <| fun writer ->
            FSharpBin.serialize writer batch
            batchesInfo := !batchesInfo |> List.map( fun x -> if x.Id=batch.Id then batchInfo else x)

    let tryGetBatchInfo id = 
        match !batchesInfo |> List.tryFind( fun x -> x.Id=id) with
        | None -> None
        | Some batchInfo -> Some batchInfo

    let updateBatchInfo batch = 
        batchesInfo := (DataModel.BatchInfo.creteNew batch) :: ( !batchesInfo |> List.filter( fun y -> y.Id <> batch.Id) )


    
let removeByIds removeIds =
    let xs1 = !batchesInfo |> List.map( fun x -> x.Id, x)
    let existed = Map.ofList xs1
    let existedIds = xs1 |> List.map fst |> Set.ofList
    batchesInfo :=
        Set.difference existedIds removeIds
        |> Seq.map( fun id -> existed.[id])
        |> Seq.toList
            
let save batch =              
    match save batch with
    | None -> updateBatchInfo batch                
    | Some e -> 
        failwithf "Не удалось сохраниться!\n\n%A\n\n%A" ( BatchInfo.creteNew batch) e



let createNew() =  
    let b = DataModel.Batch.createNew()
    save b
    b

let remove id =
    match tryGetBatchInfo id with
    | None -> Some ("удаляемая партия не найдена - " + id)
    | Some batchInfo ->
        let filename = Path.batchFileName false id batchInfo.Date
        if File.Exists filename.FileName |>  not then Some ("удаляемая партия не найдена - " + filename.FileName) else
        try            
            Directory.Delete( (Path.batchFolder false id batchInfo.Date).FileName, true )
            batchesInfo := !batchesInfo |> List.filter( fun y -> y.Id <> id)
            None
        with e ->
            Some e.Message

let getInfoList () = 
    List.rev !batchesInfo


let get id =
    match tryGetBatchInfo id with
    | None -> Error "партия не найдена"
    | Some batchInfo ->
        load id batchInfo.Date 

let getLast () =
    if !batchesInfo |> List.isEmpty then Error "список партий пуст" else
    let batchInfo = !batchesInfo |> List.maxBy ( fun x -> x.Date ) 
    load batchInfo.Id batchInfo.Date 


   