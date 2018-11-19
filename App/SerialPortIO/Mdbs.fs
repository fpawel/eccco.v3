﻿module Mdbs

open AppSets

let private req addy cmd data = 
    [| yield addy; yield cmd; yield! data |] 
    |> CRC16.add
        
let private convRxToAnswer addy cmd (rxd:byte [])   = 
    let len = Array.length rxd 
    if len=0 then Error "не отвечает"
    elif len<4 then Error ( sprintf "несоответствие дины ответа %d" len ) else
    let crc16 = CRC16.get rxd
    if crc16>0us then Error ( sprintf "ненулевая crc16 %x"  crc16 ) else
    let rxd = rxd.[0..(len-3)] |> Array.toList     
    match rxd with     
    | b::_ when b<>addy -> Error ( sprintf "несовпадение адреса %d" addy )
    | _::b::errorCode::[] when b=(cmd|||0x80uy) -> Error ( sprintf "код ошибки %d" errorCode )
    | _::b::_ when b<>cmd -> Error ( sprintf "несовпадение кода команды %d" cmd )
    | _::_::data -> Ok data
    | _ -> Error "неизвестный формат ответа"
    

let doTransfer<'a> sets cmd data parseAnswerData : Result<'a,string> =
    let request = req sets.Addy cmd data    
    match SerialPorts.sndrecv sets request with
    | Error error ->  Error error
    | Ok response ->        
        match convRxToAnswer sets.Addy cmd response  with 
        | Error error ->  
            log.Debug (sprintf "Ошибка связи %A, запрос %s, %s" sets.Description error (SerialPorts.showBytes request))
            Error ( sprintf "Ошибка связи, %A, %s" sets.Description error)
        | Ok data -> parseAnswerData data
        
        


