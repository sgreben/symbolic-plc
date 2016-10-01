namespace FsPlcModel

module Types = 
    type Basic = 
        | BOOL
        | BYTE
        | WORD
        | DWORD
        | LWORD
        | INT
        | SINT
        | DINT
        | LINT
        | UINT
        | USINT
        | UDINT
        | ULINT
        | REAL
        | LREAL
        | TIME
        | DATE
        | TIME_OF_DAY
        | DATE_AND_TIME
        | STRING
        | WSTRING
    
    type Generic = 
        | ANY
        | ANY_DERIVED
        | ANY_ELEMENTARY
        | ANY_MAGNITUDE
        | ANY_NUM
        | ANY_REAL
        | ANY_INT
        | ANY_BIT
        | ANY_STRING
        | ANY_DATE
    
    type Range = int64 * int64
    
    type Enum = Set<Common.Identifier>
    
    type Array = Common.Array_dimension * Type
    
    and Struct = (Common.Identifier * Type) list
    
    and Type = 
        | BASIC of Basic
        | BASIC_RANGE of Basic * Range
        | GENERIC of Generic * Range option
        | ARRAY of Array
        | STRUCT of Struct
        | ENUM of Enum
        | TYPE_REFERENCE of Common.Qualified_identifier
    
module TypeDeclaration = 
    type Declaration = 
        { id : Common.Identifier
          typ : Types.Type }
