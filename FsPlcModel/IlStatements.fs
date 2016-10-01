namespace FsPlcModel

module IlStatements = 
    type Label_name = Common.Identifier
    
    type Label = int64
    
    type Input_operator = 
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
    
    type Function_params_formal<'id,'operand, 'lvalue> = 
        { input : ('id * 'operand) list
          output : ('id * 'lvalue) list
          output_not : ('id * 'lvalue) list }
    
    type Function_params<'id, 'operand, 'lvalue> = 
        | Formal of Function_params_formal<'id,'operand, 'lvalue>
        | Nonformal of 'operand list
    
    type Operator<'id,'operand, 'lvalue> = 
        /// Set current result equal to current time
        | LD_CURRENT_TIME
        /// Set current result equal to operand
        | LD of 'operand
        /// Set current result equal to operand (N)
        | LDN of 'operand
        /// Store current result to operand location
        | ST of 'lvalue
        /// Store current result to operand location (N)
        | STN of 'lvalue
        /// Set operand to 1 if current result is Boolean 1
        | S of 'lvalue
        /// Reset operand to 0 if current result is Boolean
        | R of 'lvalue
        /// Logical AND (PUSH)
        | AND_PUSH
        /// Logical AND (N) (PUSH)
        | ANDN_PUSH
        /// Logical AND
        | AND of 'operand
        /// Logical AND (N)
        | ANDN of 'operand
        /// Logical OR (N)
        | ORN of 'operand
        /// Logical OR (PUSH)
        | OR_PUSH
        /// Logical OR
        | OR of 'operand
        /// Logical OR (N) (PUSH)
        | ORN_PUSH
        /// Logical exclusive OR (N)
        | XORN of 'operand
        /// Logical exclusive OR (PUSH)
        | XOR_PUSH
        /// Logical exclusive OR (N) (PUSH)
        | XORN_PUSH
        /// Logical exclusive OR
        | XOR of 'operand
        /// Addition
        | ADD of 'operand
        /// Addition (PUSH)
        | ADD_PUSH
        /// Subtraction
        | SUB of 'operand
        /// Subtraction (PUSH)
        | SUB_PUSH
        /// Multiplication
        | MUL of 'operand
        /// Multiplication (PUSH)
        | MUL_PUSH
        /// Division
        | DIV of 'operand
        /// Division (PUSH)
        | DIV_PUSH
        /// Modulo-division
        | MOD of 'operand
        /// Modulo-division (PUSH)
        | MOD_PUSH
        /// Comparison: >
        | GT of 'operand
        /// Comparison: > (PUSH)
        | GT_PUSH
        /// Comparison: >=
        | GE of 'operand
        /// Comparison: >= (PUSH)
        | GE_PUSH
        /// Comparison: =
        | EQ of 'operand
        /// Comparison: = (PUSH)
        | EQ_PUSH
        /// Comparison: <>
        | NE of 'operand
        /// Comparison: <> (PUSH)
        | NE_PUSH
        /// Comparison: <=
        | LE of 'operand
        /// Comparison: <= (PUSH)
        | LE_PUSH
        /// Comparison: <
        | LT of 'operand
        /// Comparison: < (PUSH)
        | LT_PUSH
        /// Jump to symbolic label
        | SJMP of Label_name
        /// Jump to symbolic label (C)
        | SJMPC of Label_name
        /// Jump to symbolic label (C) (N)
        | SJMPCN of Label_name
        /// Jump to label
        | JMP of Label
        /// Jump to label (C)
        | JMPC of Label
        /// Jump to label (CN)
        | JMPCN of Label
        /// Push the current result to the function argument stack
        | FUN_PUSH
        /// Call function
        | FUN of 'lvalue * Function_params<'id,'operand, 'lvalue>
        /// Standard FB call via input operators
        | INPUT of Input_operator * 'lvalue
        /// Call function block 
        | CAL of 'lvalue
        /// Call function block (C)
        | CALC of 'lvalue
        /// Call function block  (CN)
        | CALCN of 'lvalue
        /// Return from called function, function block or program
        | RET
        /// Return from called function, function block or program (C)
        | RETC
        /// Return from called function, function block or program (CN)
        | RETCN
        /// Logical negation (one's complement) of the current result
        | NOT
        /// Arithmetic negation of the current result (SUB 0 CR)
        | NEG
        /// Evaluate deferred operation
        | POP
        /// No operation
        | NOP
    
    type Statement<'id,'operand, 'lvalue> = 
        | Labeled of Label_name * Operator<'id,'operand, 'lvalue>
        | Unlabeled of Operator<'id,'operand, 'lvalue>
    
    type Program<'id,'operand, 'lvalue> = Statement<'id,'operand, 'lvalue> array

    let map_statement map_operand map_lvalue map_function_params = 
        function 
        | LD_CURRENT_TIME -> LD_CURRENT_TIME
        | LD x -> LD(map_operand x)
        | LDN x -> LDN(map_operand x)
        | ST y -> ST(map_lvalue y)
        | STN y -> STN(map_lvalue y)
        | S y -> S(map_lvalue y)
        | R y -> R(map_lvalue y)
        | AND_PUSH -> AND_PUSH
        | ANDN_PUSH -> ANDN_PUSH
        | AND x -> AND(map_operand x)
        | ANDN x -> ANDN(map_operand x)
        | ORN x -> ORN(map_operand x)
        | OR_PUSH -> OR_PUSH
        | OR x -> OR(map_operand x)
        | ORN_PUSH -> ORN_PUSH
        | XORN x -> XORN(map_operand x)
        | XOR_PUSH -> XOR_PUSH
        | XORN_PUSH -> XORN_PUSH
        | XOR x -> XOR(map_operand x)
        | ADD x -> ADD(map_operand x)
        | ADD_PUSH -> ADD_PUSH
        | SUB x -> SUB(map_operand x)
        | SUB_PUSH -> SUB_PUSH
        | MUL x -> MUL(map_operand x)
        | MUL_PUSH -> MUL_PUSH
        | DIV x -> DIV(map_operand x)
        | DIV_PUSH -> DIV_PUSH
        | MOD x -> MOD(map_operand x)
        | MOD_PUSH -> MOD_PUSH
        | GT x -> GT(map_operand x)
        | GT_PUSH -> GT_PUSH
        | GE x -> GE(map_operand x)
        | GE_PUSH -> GE_PUSH
        | EQ x -> EQ(map_operand x)
        | EQ_PUSH -> EQ_PUSH
        | NE x -> NE(map_operand x)
        | NE_PUSH -> NE_PUSH
        | LE x -> LE(map_operand x)
        | LE_PUSH -> LE_PUSH
        | LT x -> LT(map_operand x)
        | LT_PUSH -> LT_PUSH
        | SJMP l -> SJMP l
        | SJMPC l -> SJMPC l
        | SJMPCN l -> SJMPCN l
        | JMP l -> JMP l
        | JMPC l -> JMPC l
        | JMPCN l -> JMPCN l
        | FUN_PUSH -> FUN_PUSH
        | FUN(y, ps) -> FUN(map_lvalue y, map_function_params y ps)
        | INPUT(o, y) -> INPUT(o, map_lvalue y)
        | CAL y -> CAL(map_lvalue y)
        | CALC y -> CALC(map_lvalue y)
        | CALCN y -> CALCN(map_lvalue y)
        | RET -> RET
        | RETC -> RETC
        | RETCN -> RETCN
        | NOT -> NOT
        | NEG -> NEG
        | POP -> POP
        | NOP -> NOP