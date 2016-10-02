namespace FsPlcModel

module IL =
    type Input_op = 
        | CD_CTUD
        | CU_CTUD
        | PV_CTUD
        | R_CTUD
        | CD_CTD
        | PV_CTD
        | CU_CTU
        | PV_CTU
        | R_CTU
        | CLK_TRIGGER
        | IN_TON
        | PT_TON
        | IN_TOF
        | PT_TOF
        | PT_TP
        | IN_TP
        | R_SR
        | S_RS
        | R1_RS
        | S1_SR
    type Nullop = 
        /// Set current result equal to current time, create a fresh timestamp
        | LD_CURRENT_TIME
        /// Set current result equal to current time, create a fresh timestamp
        | LD_CURRENT_TIME_NOCHANGE
        | NOT
        | NEG
    type Store_op =
        /// Store current result to operand location
        | ST
        /// Store current result to operand location (N)
        | STN 
        /// Set operand to 1 if current result is Boolean 1
        | S
        /// Reset operand to 0 if current result is Boolean 0
        | R
    type Load_op = 
        /// Set current result equal to operand
        | LD
        /// Set current result equal to operand (N)
        | LDN
    type Logic_binop =  (* N,PUSH *)
        /// Logical AND
        | AND
        /// Logical OR
        | OR
        /// Logical XOR
        | XOR
        /// Logical ANDN
        | ANDN
        /// Logical ORN
        | ORN
        /// Logical XORN
        | XORN
    type Arith_binop = 
        /// Addition
        | ADD
        /// Subtraction
        | SUB
        /// Multiplication
        | MUL
        /// Division
        | DIV
        /// Modulo
        | MOD
    type Cmp_op =  (*,PUSH *)
        /// Comparison: >
        | GT
        /// Comparison: >=
        | GE
        /// Comparison: =
        | EQ
        /// Comparison: <>
        | NE
        /// Comparison: <=
        | LE
         /// Comparison: <
        | LT
    type Jump_op =
        | JMP
        /// Jump to label (C)
        | JMPC
        /// Jump to label (CN)
        | JMPCN
    type Call_operator = 
        /// Call function block 
        | CAL
        /// Call function block (C)
        | CALC
        /// Call function block  (CN)
        | CALCN

    type Ret_operator = 
        | RET
        | RETC
        | RETCN

    type Builtin_unop =
        //| CONV_BASIC of Types.Basic * Types.Basic
        | TRUNC        
        | ABS 
        | SQRT
        | LN
        | LOG
        | EXP
        | SIN
        | COS
        | TAN
        | ASIN
        | ACOS
        | ATAN
    type Builtin_extop = 
        | ADD_EXT
        | MUL_EXT
        | MUX
        | MIN
        | MAX
        | GT_EXT
        | GE_EXT
        | EQ_EXT
        | LE_EXT
        | LT_EXT
        | NE_EXT
    type Builtin_binop =
        | SUB_BIN
        | DIV_BIN
        | MOD_BIN
        | EXPT
        | MOVE
        | SHL
        | SHR
        | ROR
        | ROL
        | AND_BIN
        | OR_BIN
        | XOR_BIN
    type Builtin_ternop =
        | SEL
        | LIMIT  

    type Builtin =
        | UNOP of Builtin_unop
        | BINOP of Builtin_binop
        | TERNOP of Builtin_ternop
        | EXTOP of Builtin_extop