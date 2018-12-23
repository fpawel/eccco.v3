module TextConfig

open System
open System.IO

let readFromFile fileName foo = 
    if File.Exists fileName |> not then None else
    try
        match foo (File.ReadAllText(fileName)) with
        | Error x -> sprintf "Ошибка данных в файле %A\n%s" (Path.GetFileName fileName) x |> log.Error; None
        | Ok x -> Some x
    with e ->
        sprintf "Ошибка считывания файла %A\n%A" (Path.GetFileName fileName) e |> log.Error
        None

let writeToFile fileName x = 
    try
        File.WriteAllText(fileName, x)
    with e ->
        sprintf "Ошибка записи файла %A\n%A" (Path.GetFileName fileName) e |> log.Error

let jsonConfig<'a> configName dummy = 
    let dummy() = 
        printfn "%s- по умолчанию" configName
        dummy()

    let path =  IO.Path.Combine( appDataDir, sprintf "%s.config.json" configName)
    let config = 
    
        if IO.File.Exists path then 
            try
                match IO.File.ReadAllText(path) |> Json.parse<'a> with
                | Ok x -> x
                | Error x -> 
                    sprintf "ошибла файла конфигурации %s\n%s" ( Path.GetFileName path) x |> log.Error
                    dummy()
            with e ->
                sprintf "ошибла файла конфигурации %s\n%A" path e |> log.Error
                dummy()
        else
            dummy()
    let save() = config |> Json.stringify |> writeToFile path
    config, save

