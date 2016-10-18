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
        { /// Current result
          cr : Cell 
          /// Auxiliary registers
          aux : PersistentVector<Cell>
          /// Pointer to "this" structure
          this_ptr : Memory_address
          /// Deferred operation stack
          op_stack : List<Value * Deferred_operation>
          /// Call stack
          call_stack : List<Stack_frame>
          /// Heap memory
          memory : Memory
          /// Execution count for each task
          task_count : Map<Task_id, int>
          /// Current task
          task_current : (System_time * Task_id) option
          /// Program counter
          pc : Code_pointer
          /// Number of instructions executed so far
          instruction_count : uint64
          /// Current system time (symbolic)
          time : Symbolic_time
          /// Current system cycle
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
