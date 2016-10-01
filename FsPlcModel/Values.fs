namespace FsPlcModel

module Values = 
    type word = uint16
    type dword = uint32
    type lword = uint64
    type sint = int8
    type dint = int32
    type lint = int64
    type uint = uint16
    type usint = uint8
    type udint = uint32
    type ulint = uint64
    type real = float
    type lreal = double
    type TimeUnit = 
        | Day
        | Hour
        | Minute
        | Second
        | Millisecond
    let timeunit_ms = function
        | Day -> 24*60*60*1000
        | Hour -> 60*60*1000
        | Minute -> 60*1000
        | Second -> 1000
        | Millisecond -> 1
    type time = uint64
    let time_ms t = 
        List.map (fun (v,u) -> v * (double)(timeunit_ms u)) t
        |> List.sum
        |> System.Math.Round
        |> fun t -> (uint64)t
    type date = int * int * int
    type wstring = string
    type time_of_day = int * int * int
    type date_and_time = date * time_of_day
    
    type Basic = 
        | BOOL of bool
        | BYTE of byte
        | WORD of word
        | DWORD of dword
        | LWORD of lword
        | INT of int
        | SINT of sint
        | DINT of dint
        | LINT of lint
        | UINT of uint
        | USINT of usint
        | UDINT of udint
        | ULINT of ulint
        | REAL of real
        | LREAL of lreal
        | TIME of time
        | DATE of date
        | TIME_OF_DAY of time_of_day
        | DATE_AND_TIME of date_and_time
        | STRING of string
        | WSTRING of wstring
    type Array = Value list
    and Struct = (Common.Identifier*Value) list
    and Value = 
        | BASIC of Basic
        | ARRAY of Array
        | STRUCT of Struct
        | ENUM of string
    
    let TIME v = Basic.TIME (time_ms v)