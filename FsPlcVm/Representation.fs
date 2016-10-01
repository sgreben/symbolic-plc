namespace FsPlcVm

module IL = FsPlcModel.IL

/// Runtime representation of memory objects
module Representation = 

    /// An auxiliary register identifier
    type Register =  Reg of int
    
    /// A task identifier
    type Task_id = int
    
    /// An array index
    type Array_index = int
    
    /// A code pointer (the index of an instruction)
    type Code_pointer = int
    
    /// A main memory address
    type Memory_address = int
    
    /// A struct field identifier
    type Struct_field = int
    
    /// A basic type
    type Basic_type = 
        | TINT
        | TUINT
        | TREAL
        | TTIME
        | TBOOL
    
    /// A symbolic BOOL value
    type Symbolic_bool = Microsoft.Z3.BoolExpr
    
    /// A symbolic REAL value
    type Symbolic_real = Microsoft.Z3.RealExpr
    
    /// A symbolic UINT value
    type Symbolic_uint = Microsoft.Z3.IntExpr
    
    /// A symbolic INT value
    type Symbolic_int = Microsoft.Z3.IntExpr
    
    /// A symbolic system time
    type Symbolic_time = Microsoft.Z3.IntExpr
    
    /// A symbolic value
    type Symbolic_value = 
        | SYM_INT of Symbolic_int
        | SYM_UINT of Symbolic_uint
        | SYM_REAL of Symbolic_real
        | SYM_TIME of Symbolic_time
        | SYM_BOOL of Symbolic_bool
    
    /// A system timestamp
    type System_time = int64
    
    /// A basic value
    type Basic_value = 
        | INT of int64
        | UINT of uint64
        | REAL of float
        | BOOL of bool
        | TIME of System_time
        | STRING of string
        | SYM of Symbolic_value
    
    // TODO: replace with immutable vector
    /// A boxed representation of an array
    type Array_repr_boxed = Map<Array_index, Memory_address>
    
    /// Memory representation of a struct object
    type Struct_repr = Map<Struct_field, Memory_address>
    
    /// Memory representation of an enum value
    type Enum_repr = int
    
    /// Memory representation of a value
    and Value = 
        | VBasic of Basic_value
        | VStruct of Struct_repr
        | VArray_boxed of Array_repr_boxed
        | VEnum of Enum_repr
        | VCode_ptr of Code_pointer
    
    /// A memory cell
    and Cell = 
        | Value of Value
        | Reference of Memory_address
    
    /// Representation of a program's main memory
    type Memory = Map<Memory_address, Cell>
    
    /// A VM stack frame consisting of a return address and a `this` pointer.
    type Stack_frame = Code_pointer * Memory_address
