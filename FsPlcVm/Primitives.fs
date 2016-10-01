namespace FsPlcVm

/// LLIL VM primitives
module Primitives = 
    open States
    open Representation
    exception Type_error

    let inline read_mem ptr (m:Memory) = m.[ptr]
    let inline write_mem ptr v (m:Memory) = m.Add(ptr, v)
    let inline read ptr s = read_mem ptr s.memory
    let inline read_ref ptr s = 
        match read ptr s with
        | Reference r -> r
        | _ -> raise Type_error
    let inline read_value ptr s =
        match read ptr s with
        | Value v -> v
        | _ -> raise Type_error
    let inline read_code_ptr ptr s = 
        match read_value ptr s with
        | VCode_ptr pc -> pc
        | _ -> raise Type_error
    let inline read_boxed_array ptr s =
        match read_value ptr s with
        | VArray_boxed v -> v
        | _ -> raise Type_error
    let inline read_basic_value ptr s =
        match read_value ptr s with
        | VBasic v -> v
        | _ -> raise Type_error

    let inline ensure_ptr v = 
        match v with
        | Value (VCode_ptr ptr) -> ptr
        | _ -> raise Type_error

    let inline write ptr v s = { s with memory = write_mem ptr v s.memory }

    let inline modify ptr f s = write ptr (f (read ptr s)) s
    
    exception Invalid_read
    let inline read_cr s = 
        match s.cr with
        | Reference ptr -> read ptr s
        | _ -> raise Invalid_read

    let inline cr s = s.cr
    let inline cr_value s = 
        match s.cr with
        | Value v -> v
        | _ -> raise Type_error
    let inline cr_value_basic s =
        match cr_value s with
        | VBasic v -> v
        | _ -> raise Type_error
    let inline cr_value_bool s = 
        match cr_value_basic s with
        | BOOL b -> b
        | _ -> raise Type_error


    let inline set_cr v s = { s with cr = v }
    let inline set_cr_value v s = set_cr (Value v) s
    let inline set_cr_value_basic v s = set_cr_value (VBasic v) s

    let inline negate_cr s = 
        match s.cr with
        | Value (VBasic v) -> set_cr (Value (VBasic (Semantics.complement v))) s
        | _ -> raise Type_error

    let inline pc s = s.pc
    let inline set_pc pc' s = { s with pc = pc' }
    let inline inc_pc s = {s with pc = s.pc + 1}
    let inline push_pc s = { s with call_stack = (s.pc,s.this_ptr) :: s.call_stack }
    let inline push_op op s = { s with op_stack = (cr_value s,op)::s.op_stack  }

    let inline cycle s = s.cycle
    let inline inc_cycle s = {s with cycle = s.cycle + 1}

    let inline ic s = s.instruction_count
    let inline inc_ic s = {s with instruction_count = s.instruction_count+1UL }

    let inline time s = s.time
    let inline set_time t s = { s with time = t  }
    let inline task_count tid s  = s.task_count.[tid]
    let inline inc_task_count tid s = {s with task_count = s.task_count.Add(tid,s.task_count.[tid]+1)}
    let inline set_task_current i s = { s with task_current = Some i }
    let inline unset_task_current s = { s with  task_current = None }
    exception Call_stack_underflow

    let inline ret s = 
        match s with
        | { call_stack = [] } -> raise Call_stack_underflow
        | { call_stack = (pc',this_ptr) :: stack' } -> { s with call_stack = stack'; pc = pc'; this_ptr = this_ptr }
        
    exception Op_stack_underflow

    let inline pop_op s = 
        match s.op_stack with
        | (cr', op) :: op_stack -> (cr',op), { s with op_stack=op_stack }
        | _ -> raise Op_stack_underflow

    exception Invalid_truth_check
    let inline cr_is_sat s = 
        match (cr s) with
        | Value (VBasic v) -> Semantics.check_sat v
        | _ -> raise Invalid_truth_check
    let inline not_cr_is_sat s = 
        match (cr s) with
        | Value (VBasic v) -> Semantics.check_sat (Semantics.complement v)
        | _ -> raise Invalid_truth_check
    
    let inline mov_cr_aux (Reg i) s = { s with cr = s.aux.[i] }
    let inline mov_aux_cr (Reg i) s = { s with aux = s.aux.Update(i,s.cr) }
    let inline mov r r' s  = write r (read r' s) s
    let inline aux (Reg i) s = s.aux.[i]
    let inline aux_value (Reg i) s = 
        match s.aux.[i] with
        | Value v -> v
        | _ -> raise Type_error
    let inline aux_code_ptr i s = 
        match aux_value i s with
        | VCode_ptr pc -> pc
        | _ -> raise Type_error
    let inline aux_value_array i s =
        match aux_value i s with
        | VArray_boxed a -> a
        | _ -> raise Type_error
    let inline aux_ref (Reg i) s = 
        match s.aux.[i] with
        | Reference r -> r
        | _ -> raise Type_error
    let inline st_aux (Reg ri) c s = { s with aux = s.aux.Update(ri, c) }
    let inline ld_this s = { s with cr = read s.this_ptr s }
