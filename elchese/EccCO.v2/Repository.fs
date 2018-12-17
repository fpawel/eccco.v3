namespace EccCO.v2 

module Repository =

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
                printfn "Ошибка обращения к файлу %s, %A, %A" path x e 
                Err e.Message

        type FileName = { FileName : string }

        let readFile<'T> filename (func : BinaryReader -> 'T)  =        
            usefile filename.FileName FileMode.Open <| fun file ->
                use reader = new BinaryReader(file)
                func reader 

        let writeFile<'T> filename (func : BinaryWriter -> unit)  =         
            usefile filename.FileName FileMode.Create <| fun file ->        
                use writer = new BinaryWriter(file)
                func writer 

        module Path = 
        
            let appDataDir = 
                let appDataDir = 
                    Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData)
                let dir = Path.Combine(appDataDir, "eccco.v3")
                if not <| Directory.Exists dir then
                    let x = Directory.CreateDirectory dir
                    assert x.Exists
                dir

            let batchFolder canCreate id (dt:DateTime)  = 
                let (~%%) = string
                let month = dt.ToString("MMM", System.Globalization.CultureInfo.InvariantCulture)
                let path = Path.Combine(appDataDir, %% dt.Year, sprintf "%d-%s" dt.Month month, %% dt.Day, id )
                if canCreate then
                    createDirectory path
                { FileName = path }

            let batchFileName canCreate id dt  =             
                { FileName = Path.Combine( (batchFolder canCreate id dt).FileName, sprintf "%s.batch"  id ) }
            
        let getBatchesInfo() =
            let files = Directory.GetFiles( Path.appDataDir, "*.batch", SearchOption.AllDirectories)
            let r = 
                [   for filename in files do
                        match   readFile {FileName=filename} FSharpBin.deserialize<BatchInfo>  with
                        | Err x ->  ()                  
                        | Ok x -> yield x ]   
            r


    
  
    [<AutoOpen>]
    module private Helpers1 = 
        let load id dt =
            let fileName = Path.batchFileName false id dt
            if File.Exists fileName.FileName |> not then
                Err ("не найден файл " + fileName.FileName)
            else
                readFile fileName <| fun reader ->
                    let x = FSharpBin.deserialize<Batch> reader
                    {   x with 
                            Products = 
                                [   for p in x.Products -> 
                                        if p.Flash.Length = EEPROM_SIZE then p else
                                        { p with Flash = Array.create EEPROM_SIZE 0xffuy}] }
                
                

        let tryGetBatchInfo id = 
            match getBatchesInfo() |> List.tryFind( fun x -> x.Id=id) with
            | None -> None
            | Some batchInfo -> Some batchInfo

    let getInfoList rootPath = 
        List.rev ( getBatchesInfo rootPath)

    let loadParty id date = load id date

    let get id =
        match tryGetBatchInfo id with
        | None -> Err "партия не найдена"
        | Some batchInfo ->
            load id batchInfo.Date 

    let save batch = 
        let batchInfo = BatchInfo.creteNew batch
        let fileName = Path.batchFileName true batchInfo.Id batchInfo.Date
        try
            let fileStream = new FileStream( fileName.FileName, FileMode.Create ) 
            let writer = new BinaryWriter(fileStream)
            FSharpBin.serialize writer batch
            writer.Close()
            fileStream.Close()

        with e ->             
            failwithf "write file %A: %s" fileName.FileName e.Message 
            




   