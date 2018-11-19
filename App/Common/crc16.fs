﻿module CRC16

let private auchCRCHi = 
    [|  0x00uy; 0xC1uy; 0x81uy; 0x40uy; 0x01uy; 0xC0uy; 0x80uy; 0x41uy; 0x01uy; 0xC0uy; 0x80uy; 0x41uy; 0x00uy; 0xC1uy; 0x81uy;
        0x40uy; 0x01uy; 0xC0uy; 0x80uy; 0x41uy; 0x00uy; 0xC1uy; 0x81uy; 0x40uy; 0x00uy; 0xC1uy; 0x81uy; 0x40uy; 0x01uy; 0xC0uy;
        0x80uy; 0x41uy; 0x01uy; 0xC0uy; 0x80uy; 0x41uy; 0x00uy; 0xC1uy; 0x81uy; 0x40uy; 0x00uy; 0xC1uy; 0x81uy; 0x40uy; 0x01uy;
        0xC0uy; 0x80uy; 0x41uy; 0x00uy; 0xC1uy; 0x81uy; 0x40uy; 0x01uy; 0xC0uy; 0x80uy; 0x41uy; 0x01uy; 0xC0uy; 0x80uy; 0x41uy;
        0x00uy; 0xC1uy; 0x81uy; 0x40uy; 0x01uy; 0xC0uy; 0x80uy; 0x41uy; 0x00uy; 0xC1uy; 0x81uy; 0x40uy; 0x00uy; 0xC1uy; 0x81uy;
        0x40uy; 0x01uy; 0xC0uy; 0x80uy; 0x41uy; 0x00uy; 0xC1uy; 0x81uy; 0x40uy; 0x01uy; 0xC0uy; 0x80uy; 0x41uy; 0x01uy; 0xC0uy;
        0x80uy; 0x41uy; 0x00uy; 0xC1uy; 0x81uy; 0x40uy; 0x00uy; 0xC1uy; 0x81uy; 0x40uy; 0x01uy; 0xC0uy; 0x80uy; 0x41uy; 0x01uy;
        0xC0uy; 0x80uy; 0x41uy; 0x00uy; 0xC1uy; 0x81uy; 0x40uy; 0x01uy; 0xC0uy; 0x80uy; 0x41uy; 0x00uy; 0xC1uy; 0x81uy; 0x40uy;
        0x00uy; 0xC1uy; 0x81uy; 0x40uy; 0x01uy; 0xC0uy; 0x80uy; 0x41uy; 0x01uy; 0xC0uy; 0x80uy; 0x41uy; 0x00uy; 0xC1uy; 0x81uy;
        0x40uy; 0x00uy; 0xC1uy; 0x81uy; 0x40uy; 0x01uy; 0xC0uy; 0x80uy; 0x41uy; 0x00uy; 0xC1uy; 0x81uy; 0x40uy; 0x01uy; 0xC0uy;
        0x80uy; 0x41uy; 0x01uy; 0xC0uy; 0x80uy; 0x41uy; 0x00uy; 0xC1uy; 0x81uy; 0x40uy; 0x00uy; 0xC1uy; 0x81uy; 0x40uy; 0x01uy;
        0xC0uy; 0x80uy; 0x41uy; 0x01uy; 0xC0uy; 0x80uy; 0x41uy; 0x00uy; 0xC1uy; 0x81uy; 0x40uy; 0x01uy; 0xC0uy; 0x80uy; 0x41uy;
        0x00uy; 0xC1uy; 0x81uy; 0x40uy; 0x00uy; 0xC1uy; 0x81uy; 0x40uy; 0x01uy; 0xC0uy; 0x80uy; 0x41uy; 0x00uy; 0xC1uy; 0x81uy;
        0x40uy; 0x01uy; 0xC0uy; 0x80uy; 0x41uy; 0x01uy; 0xC0uy; 0x80uy; 0x41uy; 0x00uy; 0xC1uy; 0x81uy; 0x40uy; 0x01uy; 0xC0uy;
        0x80uy; 0x41uy; 0x00uy; 0xC1uy; 0x81uy; 0x40uy; 0x00uy; 0xC1uy; 0x81uy; 0x40uy; 0x01uy; 0xC0uy; 0x80uy; 0x41uy; 0x01uy;
        0xC0uy; 0x80uy; 0x41uy; 0x00uy; 0xC1uy; 0x81uy; 0x40uy; 0x00uy; 0xC1uy; 0x81uy; 0x40uy; 0x01uy; 0xC0uy; 0x80uy; 0x41uy;
        0x00uy; 0xC1uy; 0x81uy; 0x40uy; 0x01uy; 0xC0uy; 0x80uy; 0x41uy; 0x01uy; 0xC0uy; 0x80uy; 0x41uy; 0x00uy; 0xC1uy; 0x81uy;
        0x40uy |]

let private auchCRCLo = 
    [|  0x00uy; 0xC0uy; 0xC1uy; 0x01uy; 0xC3uy; 0x03uy; 0x02uy; 0xC2uy; 0xC6uy; 0x06uy; 0x07uy; 0xC7uy; 0x05uy; 0xC5uy; 0xC4uy;
        0x04uy; 0xCCuy; 0x0Cuy; 0x0Duy; 0xCDuy; 0x0Fuy; 0xCFuy; 0xCEuy; 0x0Euy; 0x0Auy; 0xCAuy; 0xCBuy; 0x0Buy; 0xC9uy; 0x09uy;
        0x08uy; 0xC8uy; 0xD8uy; 0x18uy; 0x19uy; 0xD9uy; 0x1Buy; 0xDBuy; 0xDAuy; 0x1Auy; 0x1Euy; 0xDEuy; 0xDFuy; 0x1Fuy; 0xDDuy;
        0x1Duy; 0x1Cuy; 0xDCuy; 0x14uy; 0xD4uy; 0xD5uy; 0x15uy; 0xD7uy; 0x17uy; 0x16uy; 0xD6uy; 0xD2uy; 0x12uy; 0x13uy; 0xD3uy;
        0x11uy; 0xD1uy; 0xD0uy; 0x10uy; 0xF0uy; 0x30uy; 0x31uy; 0xF1uy; 0x33uy; 0xF3uy; 0xF2uy; 0x32uy; 0x36uy; 0xF6uy; 0xF7uy;
        0x37uy; 0xF5uy; 0x35uy; 0x34uy; 0xF4uy; 0x3Cuy; 0xFCuy; 0xFDuy; 0x3Duy; 0xFFuy; 0x3Fuy; 0x3Euy; 0xFEuy; 0xFAuy; 0x3Auy;
        0x3Buy; 0xFBuy; 0x39uy; 0xF9uy; 0xF8uy; 0x38uy; 0x28uy; 0xE8uy; 0xE9uy; 0x29uy; 0xEBuy; 0x2Buy; 0x2Auy; 0xEAuy; 0xEEuy;
        0x2Euy; 0x2Fuy; 0xEFuy; 0x2Duy; 0xEDuy; 0xECuy; 0x2Cuy; 0xE4uy; 0x24uy; 0x25uy; 0xE5uy; 0x27uy; 0xE7uy; 0xE6uy; 0x26uy;
        0x22uy; 0xE2uy; 0xE3uy; 0x23uy; 0xE1uy; 0x21uy; 0x20uy; 0xE0uy; 0xA0uy; 0x60uy; 0x61uy; 0xA1uy; 0x63uy; 0xA3uy; 0xA2uy;
        0x62uy; 0x66uy; 0xA6uy; 0xA7uy; 0x67uy; 0xA5uy; 0x65uy; 0x64uy; 0xA4uy; 0x6Cuy; 0xACuy; 0xADuy; 0x6Duy; 0xAFuy; 0x6Fuy;
        0x6Euy; 0xAEuy; 0xAAuy; 0x6Auy; 0x6Buy; 0xABuy; 0x69uy; 0xA9uy; 0xA8uy; 0x68uy; 0x78uy; 0xB8uy; 0xB9uy; 0x79uy; 0xBBuy;
        0x7Buy; 0x7Auy; 0xBAuy; 0xBEuy; 0x7Euy; 0x7Fuy; 0xBFuy; 0x7Duy; 0xBDuy; 0xBCuy; 0x7Cuy; 0xB4uy; 0x74uy; 0x75uy; 0xB5uy;
        0x77uy; 0xB7uy; 0xB6uy; 0x76uy; 0x72uy; 0xB2uy; 0xB3uy; 0x73uy; 0xB1uy; 0x71uy; 0x70uy; 0xB0uy; 0x50uy; 0x90uy; 0x91uy;
        0x51uy; 0x93uy; 0x53uy; 0x52uy; 0x92uy; 0x96uy; 0x56uy; 0x57uy; 0x97uy; 0x55uy; 0x95uy; 0x94uy; 0x54uy; 0x9Cuy; 0x5Cuy;
        0x5Duy; 0x9Duy; 0x5Fuy; 0x9Fuy; 0x9Euy; 0x5Euy; 0x5Auy; 0x9Auy; 0x9Buy; 0x5Buy; 0x99uy; 0x59uy; 0x58uy; 0x98uy; 0x88uy;
        0x48uy; 0x49uy; 0x89uy; 0x4Buy; 0x8Buy; 0x8Auy; 0x4Auy; 0x4Euy; 0x8Euy; 0x8Fuy; 0x4Fuy; 0x8Duy; 0x4Duy; 0x4Cuy; 0x8Cuy;
        0x44uy; 0x84uy; 0x85uy; 0x45uy; 0x87uy; 0x47uy; 0x46uy; 0x86uy; 0x82uy; 0x42uy; 0x43uy; 0x83uy; 0x41uy; 0x81uy; 0x80uy;
        0x40uy |]

let get bytes = 
    let fcrc (hi,uchCRCLo) (b:byte) =
        let i = int (hi ^^^ b)
        let hi = uchCRCLo ^^^ auchCRCHi.[i]
        let lo = auchCRCLo.[i]
        (hi,lo)
    let hi,lo = Array.fold  fcrc (0xFFuy,0xFFuy)  bytes 
    (uint16 hi<<<8)+(uint16 lo)

let add bytes = 
    let u = get bytes
    [|  yield! bytes 
        yield byte(u >>> 8)
        yield  byte u |]