namespace FsPlcVm

module IL = FsPlcModel.IL

open Microsoft.Z3

module States = 
    open IL
    open Representation
    open FSharpx.Collections
    open Language
    
    /// A VM state
    type State = 
        { cr : Cell
          aux : PersistentVector<Cell>
          this_ptr : Memory_address
          op_stack : List<Value * Deferred_operation>
          call_stack : List<Stack_frame>
          memory : Memory
          task_count : Map<Task_id, int>
          task_current : (System_time * Task_id) option
          pc : Code_pointer
          instruction_count : uint64
          time : Symbolic_time
          cycle : int }
    
    /// Returns an initial state given a memory map
    let initial memory = 
        { memory = memory
          task_count = Map.empty
          this_ptr = 0
          task_current = None
          cr = Value(VBasic(INT 0L))
          aux = PersistentVector.empty
          op_stack = []
          call_stack = []
          pc = 0
          time = SMT.Const.mk_int 0L
          instruction_count = 0UL
          cycle = 0 }
