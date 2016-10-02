namespace FsPlcModel

module TypeConversions = 

    module Query =
        open Types
        let rec generic_subtype super sub = 
            let subtypes xs x = List.map (fun y -> generic_subtype y x) xs |> List.fold (||) false
            if super = sub then true
            else 
                match super with
                | ANY_ELEMENTARY -> subtypes [ ANY_MAGNITUDE; ANY_BIT; ANY_STRING; ANY_DATE ] sub
                | ANY_MAGNITUDE -> subtypes [ ANY_NUM ] sub
                | ANY_NUM -> subtypes [ ANY_REAL; ANY_INT ] sub
                | ANY -> true
                | _ -> false
    
        let is_derived = 
            function 
            | STRUCT _ | ARRAY _ | ENUM _ | TYPE_REFERENCE _ -> true
            | _ -> false
    
        let rec generic_instance gtyp styp = 
            let basic xs x = Option.isSome (List.tryFind (BASIC >> (=) x) xs)
            let generic xs t = List.map (fun g -> generic_instance g t) xs |> List.fold (||) false
            match gtyp with
            | ANY -> true
            | ANY_DERIVED -> is_derived styp
            | ANY_ELEMENTARY -> generic [ ANY_MAGNITUDE; ANY_BIT; ANY_STRING; ANY_DATE ] styp
            | ANY_MAGNITUDE -> generic [ ANY_NUM ] styp || basic [ TIME ] styp
            | ANY_NUM -> generic [ ANY_REAL; ANY_INT ] styp
            | ANY_REAL -> basic [ REAL; LREAL ] styp
            | ANY_INT -> basic [ LINT; DINT; INT; SINT; ULINT; UDINT; UINT; USINT ] styp
            | ANY_BIT -> basic [ LWORD; DWORD; WORD; BYTE; BOOL ] styp
            | ANY_STRING -> basic [ STRING; WSTRING ] styp
            | ANY_DATE -> basic [ DATE_AND_TIME; DATE; TIME_OF_DAY ] styp


    module Basic =
        let to_real = 
            function 
            | Values.BOOL true -> Values.REAL((float) 1)
            | Values.BOOL false -> Values.REAL((float) 0)
            | Values.REAL v -> Values.REAL((float) v)
            | Values.UDINT v -> Values.REAL((float) v)
            | Values.USINT v -> Values.REAL((float) v)
            | Values.ULINT v -> Values.REAL((float) v)
            | Values.LWORD v -> Values.REAL((float) v)
            | Values.DINT v -> Values.REAL((float) v)
            | Values.UINT v -> Values.REAL((float) v)
            | Values.LINT v -> Values.REAL((float) v)
            | Values.BYTE v -> Values.REAL((float) v)
            | Values.WORD v -> Values.REAL((float) v)
            | Values.LREAL v -> Values.REAL((float) v)
            | Values.DWORD v -> Values.REAL((float) v)
            | Values.SINT v -> Values.REAL((float) v)
    
        let to_udint = 
            function 
            | Values.BOOL true -> Values.UDINT((uint32) 1)
            | Values.BOOL false -> Values.UDINT((uint32) 0)
            | Values.REAL v -> Values.UDINT((uint32) v)
            | Values.UDINT v -> Values.UDINT((uint32) v)
            | Values.USINT v -> Values.UDINT((uint32) v)
            | Values.ULINT v -> Values.UDINT((uint32) v)
            | Values.LWORD v -> Values.UDINT((uint32) v)
            | Values.DINT v -> Values.UDINT((uint32) v)
            | Values.UINT v -> Values.UDINT((uint32) v)
            | Values.LINT v -> Values.UDINT((uint32) v)
            | Values.BYTE v -> Values.UDINT((uint32) v)
            | Values.WORD v -> Values.UDINT((uint32) v)
            | Values.LREAL v -> Values.UDINT((uint32) v)
            | Values.DWORD v -> Values.UDINT((uint32) v)
            | Values.SINT v -> Values.UDINT((uint32) v)
    
        let to_usint = 
            function 
            | Values.BOOL true -> Values.USINT((uint8) 1)
            | Values.BOOL false -> Values.USINT((uint8) 0)
            | Values.REAL v -> Values.USINT((uint8) v)
            | Values.UDINT v -> Values.USINT((uint8) v)
            | Values.USINT v -> Values.USINT((uint8) v)
            | Values.ULINT v -> Values.USINT((uint8) v)
            | Values.LWORD v -> Values.USINT((uint8) v)
            | Values.DINT v -> Values.USINT((uint8) v)
            | Values.UINT v -> Values.USINT((uint8) v)
            | Values.LINT v -> Values.USINT((uint8) v)
            | Values.BYTE v -> Values.USINT((uint8) v)
            | Values.WORD v -> Values.USINT((uint8) v)
            | Values.LREAL v -> Values.USINT((uint8) v)
            | Values.DWORD v -> Values.USINT((uint8) v)
            | Values.SINT v -> Values.USINT((uint8) v)
    
        let to_ulint = 
            function 
            | Values.BOOL true -> Values.ULINT((uint64) 1)
            | Values.BOOL false -> Values.ULINT((uint64) 0)
            | Values.REAL v -> Values.ULINT((uint64) v)
            | Values.UDINT v -> Values.ULINT((uint64) v)
            | Values.USINT v -> Values.ULINT((uint64) v)
            | Values.ULINT v -> Values.ULINT((uint64) v)
            | Values.LWORD v -> Values.ULINT((uint64) v)
            | Values.DINT v -> Values.ULINT((uint64) v)
            | Values.UINT v -> Values.ULINT((uint64) v)
            | Values.LINT v -> Values.ULINT((uint64) v)
            | Values.BYTE v -> Values.ULINT((uint64) v)
            | Values.WORD v -> Values.ULINT((uint64) v)
            | Values.LREAL v -> Values.ULINT((uint64) v)
            | Values.DWORD v -> Values.ULINT((uint64) v)
            | Values.SINT v -> Values.ULINT((uint64) v)
    
        let to_lword = 
            function 
            | Values.BOOL true -> Values.LWORD((uint64) 1)
            | Values.BOOL false -> Values.LWORD((uint64) 0)
            | Values.REAL v -> Values.LWORD((uint64) v)
            | Values.UDINT v -> Values.LWORD((uint64) v)
            | Values.USINT v -> Values.LWORD((uint64) v)
            | Values.ULINT v -> Values.LWORD((uint64) v)
            | Values.LWORD v -> Values.LWORD((uint64) v)
            | Values.DINT v -> Values.LWORD((uint64) v)
            | Values.UINT v -> Values.LWORD((uint64) v)
            | Values.LINT v -> Values.LWORD((uint64) v)
            | Values.BYTE v -> Values.LWORD((uint64) v)
            | Values.WORD v -> Values.LWORD((uint64) v)
            | Values.LREAL v -> Values.LWORD((uint64) v)
            | Values.DWORD v -> Values.LWORD((uint64) v)
            | Values.SINT v -> Values.LWORD((uint64) v)
    
        let to_dint = 
            function 
            | Values.BOOL true -> Values.DINT((int32) 1)
            | Values.BOOL false -> Values.DINT((int32) 0)
            | Values.REAL v -> Values.DINT((int32) v)
            | Values.UDINT v -> Values.DINT((int32) v)
            | Values.USINT v -> Values.DINT((int32) v)
            | Values.ULINT v -> Values.DINT((int32) v)
            | Values.LWORD v -> Values.DINT((int32) v)
            | Values.DINT v -> Values.DINT((int32) v)
            | Values.UINT v -> Values.DINT((int32) v)
            | Values.LINT v -> Values.DINT((int32) v)
            | Values.BYTE v -> Values.DINT((int32) v)
            | Values.WORD v -> Values.DINT((int32) v)
            | Values.LREAL v -> Values.DINT((int32) v)
            | Values.DWORD v -> Values.DINT((int32) v)
            | Values.SINT v -> Values.DINT((int32) v)
    
        let to_uint = 
            function 
            | Values.BOOL true -> Values.UINT((uint16) 1)
            | Values.BOOL false -> Values.UINT((uint16) 0)
            | Values.REAL v -> Values.UINT((uint16) v)
            | Values.UDINT v -> Values.UINT((uint16) v)
            | Values.USINT v -> Values.UINT((uint16) v)
            | Values.ULINT v -> Values.UINT((uint16) v)
            | Values.LWORD v -> Values.UINT((uint16) v)
            | Values.DINT v -> Values.UINT((uint16) v)
            | Values.UINT v -> Values.UINT((uint16) v)
            | Values.LINT v -> Values.UINT((uint16) v)
            | Values.BYTE v -> Values.UINT((uint16) v)
            | Values.WORD v -> Values.UINT((uint16) v)
            | Values.LREAL v -> Values.UINT((uint16) v)
            | Values.DWORD v -> Values.UINT((uint16) v)
            | Values.SINT v -> Values.UINT((uint16) v)
    
        let to_lint = 
            function 
            | Values.BOOL true -> Values.LINT((int64) 1)
            | Values.BOOL false -> Values.LINT((int64) 0)
            | Values.REAL v -> Values.LINT((int64) v)
            | Values.UDINT v -> Values.LINT((int64) v)
            | Values.USINT v -> Values.LINT((int64) v)
            | Values.ULINT v -> Values.LINT((int64) v)
            | Values.LWORD v -> Values.LINT((int64) v)
            | Values.DINT v -> Values.LINT((int64) v)
            | Values.UINT v -> Values.LINT((int64) v)
            | Values.LINT v -> Values.LINT((int64) v)
            | Values.BYTE v -> Values.LINT((int64) v)
            | Values.WORD v -> Values.LINT((int64) v)
            | Values.LREAL v -> Values.LINT((int64) v)
            | Values.DWORD v -> Values.LINT((int64) v)
            | Values.SINT v -> Values.LINT((int64) v)
    
        let to_byte = 
            function 
            | Values.BOOL true -> Values.BYTE((byte) 1)
            | Values.BOOL false -> Values.BYTE((byte) 0)
            | Values.REAL v -> Values.BYTE((byte) v)
            | Values.UDINT v -> Values.BYTE((byte) v)
            | Values.USINT v -> Values.BYTE((byte) v)
            | Values.ULINT v -> Values.BYTE((byte) v)
            | Values.LWORD v -> Values.BYTE((byte) v)
            | Values.DINT v -> Values.BYTE((byte) v)
            | Values.UINT v -> Values.BYTE((byte) v)
            | Values.LINT v -> Values.BYTE((byte) v)
            | Values.BYTE v -> Values.BYTE((byte) v)
            | Values.WORD v -> Values.BYTE((byte) v)
            | Values.LREAL v -> Values.BYTE((byte) v)
            | Values.DWORD v -> Values.BYTE((byte) v)
            | Values.SINT v -> Values.BYTE((byte) v)
    
        let to_word = 
            function 
            | Values.BOOL true -> Values.WORD((uint16) 1)
            | Values.BOOL false -> Values.WORD((uint16) 0)
            | Values.REAL v -> Values.WORD((uint16) v)
            | Values.UDINT v -> Values.WORD((uint16) v)
            | Values.USINT v -> Values.WORD((uint16) v)
            | Values.ULINT v -> Values.WORD((uint16) v)
            | Values.LWORD v -> Values.WORD((uint16) v)
            | Values.DINT v -> Values.WORD((uint16) v)
            | Values.UINT v -> Values.WORD((uint16) v)
            | Values.LINT v -> Values.WORD((uint16) v)
            | Values.BYTE v -> Values.WORD((uint16) v)
            | Values.WORD v -> Values.WORD((uint16) v)
            | Values.LREAL v -> Values.WORD((uint16) v)
            | Values.DWORD v -> Values.WORD((uint16) v)
            | Values.SINT v -> Values.WORD((uint16) v)
    
        let to_lreal = 
            function 
            | Values.BOOL true -> Values.LREAL((double) 1)
            | Values.BOOL false -> Values.LREAL((double) 0)
            | Values.REAL v -> Values.LREAL((double) v)
            | Values.UDINT v -> Values.LREAL((double) v)
            | Values.USINT v -> Values.LREAL((double) v)
            | Values.ULINT v -> Values.LREAL((double) v)
            | Values.LWORD v -> Values.LREAL((double) v)
            | Values.DINT v -> Values.LREAL((double) v)
            | Values.UINT v -> Values.LREAL((double) v)
            | Values.LINT v -> Values.LREAL((double) v)
            | Values.BYTE v -> Values.LREAL((double) v)
            | Values.WORD v -> Values.LREAL((double) v)
            | Values.LREAL v -> Values.LREAL((double) v)
            | Values.DWORD v -> Values.LREAL((double) v)
            | Values.SINT v -> Values.LREAL((double) v)
    
        let to_dword = 
            function 
            | Values.BOOL true -> Values.DWORD((uint32) 1)
            | Values.BOOL false -> Values.DWORD((uint32) 0)
            | Values.REAL v -> Values.DWORD((uint32) v)
            | Values.UDINT v -> Values.DWORD((uint32) v)
            | Values.USINT v -> Values.DWORD((uint32) v)
            | Values.ULINT v -> Values.DWORD((uint32) v)
            | Values.LWORD v -> Values.DWORD((uint32) v)
            | Values.DINT v -> Values.DWORD((uint32) v)
            | Values.UINT v -> Values.DWORD((uint32) v)
            | Values.LINT v -> Values.DWORD((uint32) v)
            | Values.BYTE v -> Values.DWORD((uint32) v)
            | Values.WORD v -> Values.DWORD((uint32) v)
            | Values.LREAL v -> Values.DWORD((uint32) v)
            | Values.DWORD v -> Values.DWORD((uint32) v)
            | Values.SINT v -> Values.DWORD((uint32) v)
    
        let to_sint = 
            function 
            | Values.BOOL true -> Values.SINT((int8) 1)
            | Values.BOOL false -> Values.SINT((int8) 0)
            | Values.REAL v -> Values.SINT((int8) v)
            | Values.UDINT v -> Values.SINT((int8) v)
            | Values.USINT v -> Values.SINT((int8) v)
            | Values.ULINT v -> Values.SINT((int8) v)
            | Values.LWORD v -> Values.SINT((int8) v)
            | Values.DINT v -> Values.SINT((int8) v)
            | Values.UINT v -> Values.SINT((int8) v)
            | Values.LINT v -> Values.SINT((int8) v)
            | Values.BYTE v -> Values.SINT((int8) v)
            | Values.WORD v -> Values.SINT((int8) v)
            | Values.LREAL v -> Values.SINT((int8) v)
            | Values.DWORD v -> Values.SINT((int8) v)
            | Values.SINT v -> Values.SINT((int8) v)
    
        let to_bool = 
            function 
            | Values.BOOL b -> Values.BOOL b
            | Values.REAL v -> Values.BOOL(v > (float) 0)
            | Values.UDINT v -> Values.BOOL(v > (uint32) 0)
            | Values.USINT v -> Values.BOOL(v > (uint8) 0)
            | Values.ULINT v -> Values.BOOL(v > (uint64) 0)
            | Values.LWORD v -> Values.BOOL(v > (uint64) 0)
            | Values.DINT v -> Values.BOOL(v > (int32) 0)
            | Values.UINT v -> Values.BOOL(v > (uint16) 0)
            | Values.LINT v -> Values.BOOL(v > (int64) 0)
            | Values.BYTE v -> Values.BOOL(v > (byte) 0)
            | Values.WORD v -> Values.BOOL(v > (uint16) 0)
            | Values.LREAL v -> Values.BOOL(v > (double) 0)
            | Values.DWORD v -> Values.BOOL(v > (uint32) 0)
            | Values.SINT v -> Values.BOOL(v > (int8) 0)
    
        let to_type = 
            function
            | Types.BOOL -> to_bool 
            | Types.REAL -> to_real
            | Types.UDINT -> to_udint
            | Types.USINT -> to_usint
            | Types.ULINT -> to_ulint
            | Types.LWORD -> to_lword
            | Types.DINT -> to_dint
            | Types.UINT -> to_uint
            | Types.LINT -> to_lint
            | Types.BYTE -> to_byte
            | Types.WORD -> to_word
            | Types.LREAL -> to_lreal
            | Types.DWORD -> to_dword
            | Types.SINT -> to_sint
