namespace FsPlcSpec

/// Finite-state timed automata, represented as assembly-like programs
module Fta = 
    
    module IL = FsPlcModel.IL

    /// An automaton state
    type State = S of int
    
    /// An automaton time variable
    type Variable = V of int
    
    type Time_constraint = (Variable * IL.Cmp_op * int)
    
    type Atom = 
        | Time_constraint of Time_constraint
        | Value_constraint of Spec.Value_constraint
    
    type Constraint = Atom list
    
    type Instruction = 
        | ASSUME of Constraint
        | ASSERT of Constraint
        | PASS
        | NEXT
        | SET_STATE of State
        | NONDET of State
        | LD_TIME of Variable
        | ST_MOMENT of int
        | JMP of State
        | STATE of State

    type Fta = 
        { initial_state : State
          instructions : Instruction list }
