namespace FsPlcVm

module IL = FsPlcModel.IL

/// Low-level variant of IL interpreted by the analysis VM
module Language =
    open IL
    open Representation
    
    // TODO: Get rid of this, compile to register ops
    /// A deferred operation
    type Deferred_operation = 
        | ABINOP_POP of Arith_binop 
        | LBINOP_POP of Logic_binop 
        | CMPOP_POP of Cmp_op
    
    /// A low-level-IL (LLIL) operand
    type Operand = Cell

    /// A monitoring command
    type Monitor_operator =
        | FAIL
        | PASS
        | ASSUME
        | LOG_STATE

    /// An OS service command
    type Os_operator =
        | NEXT_CYCLE
        | IS_TASK_READY of System_time*Task_id
        | END_TASK of System_time*Task_id
        | BEGIN_TASK of System_time*Task_id
        | READ_INPUT of Basic_type*Memory_address
        | WRITE_OUTPUT of Memory_address

    /// A low-level-IL (LLIL) operator
    type Operator = 
        | NULLOP of Nullop
        /// CR := (value)
        | LOADOP_IMM of Load_op * Value
        /// CR := Mem[(addr)]
        | LOADOP_REF of Load_op * Memory_address
        /// CR := CR.(field)
        | LOADOP_FIELD of Load_op * Struct_field
        /// CR := CR[(int)]
        | LOADOP_INDEX_IMM of Load_op * Array_index
        /// CR := CR[AUX[i]
        | LOADOP_INDEX_REF of Load_op * Register
        /// Mem[(addr)] := CR
        | STOREOP_IMM of Store_op * Memory_address
        /// Mem[(addr1)] := Mem[(addr2)]
        | MOV of Memory_address*Memory_address
        /// Mem[AUX[i]] := CR
        | STOREOP_AUX of Store_op * Register
        /// Mem[Mem[(addr)]] := CR
        | STOREOP_REF of Store_op * Memory_address
        /// AUX[i].(field) := CR
        | STOREOP_FIELD_IMM of Store_op *  Struct_field * Register
        /// Mem[Mem[AUX[i]]].(field) := CR
        | STOREOP_FIELD_REF of Store_op * Struct_field * Register
        /// Mem[AUX[i]][(int)] := CR
        | STOREOP_INDEX_IMM of Store_op * Array_index * Register
        /// Mem[AUX[i]][Mem[(addr)]] := CR
        | STOREOP_INDEX_REF of Store_op * Memory_address * Register
        /// AUX[i][AUX[j]] := CR
        | STOREOP_INDEX_AUX of Store_op * Register * Register
        /// JMP (pc)
        | JMPOP of Jump_op * Code_pointer
        /// JMP (current pc)+(offset)
        | JMPOP_REL of Jump_op * Code_pointer
        /// JMP Mem[(addr)]+offset
        | JMP_REF of Memory_address * Code_pointer (* offset *)
        /// NONDET JMP (pc)
        | NONDET of Code_pointer
        /// CR (op) (value)
        | ABINOP_IMM of Arith_binop * Basic_value
        /// CR (op) Mem[(addr)]
        | ABINOP_REF of Arith_binop * Memory_address
        /// CR (op) (
        | ABINOP_PUSH of Arith_binop
        /// CR (op) (value)
        | LBINOP_IMM of Logic_binop * bool
        /// CR (op) Mem[(addr)]
        | LBINOP_REF of Logic_binop * Memory_address
        /// CR (op) (
        | LBINOP_PUSH of Logic_binop
        /// CR (op) (value)
        | CMPOP_IMM of Cmp_op * Basic_value
        /// CR (op) Mem[(addr)]
        | CMPOP_REF of Cmp_op * Memory_address
        /// CR (op) (
        | CMPOP_PUSH of Cmp_op
        /// CALL (pc)
        | FUN of Code_pointer*Memory_address
        // <BUILTIN>
        | FUN_BUILTIN_UNOP of Builtin_unop
        // <BUILTIN>(CR,<arg>)
        | FUN_BUILTIN_BINOP of Builtin_binop *  Operand
        // <BUILTIN>(CR,<arg>,<arg>)
        | FUN_BUILTIN_TERNOP of Builtin_binop * Operand * Operand
        // <BUILTIN>(CR,<arg>,...)
        | FUN_BUILTIN_EXTOP of Builtin_extop * (Operand list)
        /// CALL (pc)
        | CALLOP_IMM of Call_operator * Code_pointer * Register
        /// CALL Mem[(addr)]
        | CALLOP_REF of Call_operator * Memory_address * Register
        /// CALL AUX[i]
        | CALLOP_AUX of Call_operator * Register * Register
        /// RET[C,N]
        | RETOP of Ret_operator
        /// OS
        | OSOP of Os_operator
        /// Monitoring 
        | MONITOROP of Monitor_operator
        /// CR := AUX[r]
        | MOV_CR_AUX of Register
        /// AUX[r] := CR
        | MOV_AUX_CR of Register
        /// AUX[r] := Cell
        | ST_AUX of Register*Cell
        /// Dereferece the `this` pointer
        | LD_THIS
        /// )
        | POP
        /// No operation
        | NOP

    /// An LLIL program
    type Program = Operator[]

    /// Relocate LLIL code by a given offset
    let relocate offset : Program -> Program = Array.map (function 
        | JMPOP (op,pc) -> JMPOP (op,pc+offset)
        | NONDET pc -> NONDET (pc+offset)
        | FUN (pc,mem) -> FUN (pc+offset,mem)
        | JMP_REF (r,offset') -> JMP_REF (r,offset+offset')
        | CALLOP_IMM(op,pc,r) -> CALLOP_IMM(op,pc+offset,r)
        | op -> op)
