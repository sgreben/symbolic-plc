namespace FsPlcSpec

/// Declarative temporal test case specifications
module Spec = 
    open FsPlcVm.Representation
    
    module IL = FsPlcModel.IL
    
    /// A value constraint is a comparison between two data items (references or literals)
    type Value_constraint = Cell * IL.Cmp_op * Cell
    
    /// A time constraint is either (<= t ms) or (>= t ms)
    type Time_constraint_spec = 
        | Time_le of int
        | Time_ge of int
    
    /// A spec formula
    type Formula = 
        /// constraint(C) { phi }
        | Constraint of Value_constraint * Formula
        /// within(T) { phi }
        | Within of Time_constraint_spec * Formula
        /// delay(T) { phi }
        | Delay of Time_constraint_spec * Formula
        /// phi1; phi2
        | Seq of Formula * Formula
        /// assert(phi)
        | Check of Value_constraint
        /// Signals a successful test case
        | Pass
        /// The empty formula (equiv. to Assert(True))
        | Empty
