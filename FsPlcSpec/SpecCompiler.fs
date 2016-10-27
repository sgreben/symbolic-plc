namespace FsPlcSpec

/// Compiles declarative specifications to finite timed automata (module Fta)
module SpecCompiler =
    open Spec
    open Fta

    type Compile_env = 
        { max_var : Variable
          max_state : State
          max_moment : int }
    
    let fresh_state env = 
        match env.max_state with
        | S i -> S(i + 1), { env with max_state = S(i + 1) }
    
    let fresh_var env = 
        match env.max_var with
        | V i -> V(i + 1), { env with max_var = V(i + 1) }
    
    let next_moment env = env.max_moment, { env with max_moment = env.max_moment + 1 }
    
    let empty_env = 
        { max_state = S 1
          max_var = V 0
          max_moment = 0 }

    let tc_of_spec t = 
        function 
        | Time_le x -> Diff_le(t, x)
        | Time_ge x -> Diff_ge(t, x)
        
    let rec compile' env on_finish C A = 
        match A with
        | Pass -> 
            let s_i, env = fresh_state env
            { initial_state = s_i
              instructions = 
                  [ STATE s_i
                    ASSUME C
                    PASS
                    SET_STATE on_finish
                    NEXT ] }, env
        | Check assertion -> 
            let s_i, env = fresh_state env
            { initial_state = s_i
              instructions = 
                  [ STATE s_i
                    ASSUME C
                    ASSERT [ Value_constraint assertion ]
                    ST_MOMENT env.max_moment
                    JMP on_finish ] }, env
        | Empty -> 
            let s_i, env = fresh_state env
            { initial_state = s_i
              instructions = 
                  [ STATE s_i
                    ASSUME C
                    ST_MOMENT env.max_moment
                    JMP on_finish ] }, env
        | Seq(A, B) -> 
            let s_i, env = fresh_state env
            let s_temp, env = fresh_state env
            let s_f, env = fresh_state env
            let A, env = compile' env s_temp C A
            let B, env = compile' env s_f C B
            { initial_state = s_i
              instructions = 
                  List.concat [ [ STATE s_i
                                  ASSUME C ]
                                A.instructions
                                [ STATE s_temp ]
                                B.instructions
                                [ STATE s_f
                                  JMP on_finish ] ] }, env
        | Constraint(phi, A) -> compile' env on_finish ((Value_constraint phi) :: C) A
        | Delay(Time_le x, A) -> 
            let s_i, env = fresh_state env
            let s_wait, env = fresh_state env
            let s_go, env = fresh_state env
            let s_f, env = fresh_state env
            let t, env = fresh_var env
            let moment, env = next_moment env
            let A, env = compile' env s_f C A
            let t_phi = Time_constraint(tc_of_spec t (Time_le x))
            let t_phi_not = Time_constraint(Diff_gt(t, x))
            { initial_state = s_i
              instructions = 
                  List.concat [ [ STATE s_i
                                  ASSUME C
                                  ST_MOMENT moment
                                  LD_TIME t
                                  STATE s_wait
                                  NONDET s_go
                                  ASSUME(t_phi :: C)
                                  NEXT
                                  STATE s_go
                                  ASSUME(t_phi :: C)
                                  JMP A.initial_state ]
                                A.instructions
                                [ STATE s_f
                                  ASSUME C
                                  SET_STATE on_finish
                                  JMP on_finish ] ] }, env
        | Delay(Time_ge x, A) -> 
            let s_i, env = fresh_state env
            let s_wait, env = fresh_state env
            let s_go, env = fresh_state env
            let s_f, env = fresh_state env
            let t, env = fresh_var env
            let moment, env = next_moment env
            let A, env = compile' env s_f C A
            let t_phi = Time_constraint(tc_of_spec t (Time_ge x))
            let t_phi_not = Time_constraint(Diff_lt(t, x))
            { initial_state = s_i
              instructions = 
                  List.concat [ [ STATE s_i
                                  ASSUME C
                                  ST_MOMENT moment
                                  LD_TIME t
                                  STATE s_wait
                                  NONDET s_go
                                  ASSUME(t_phi_not :: C)
                                  NEXT
                                  STATE s_go
                                  ASSUME(t_phi :: C)
                                  JMP A.initial_state ]
                                A.instructions
                                [ STATE s_f
                                  ASSUME C
                                  SET_STATE on_finish
                                  JMP on_finish ] ] }, env
        | Within(Time_le x, A) -> 
            let s_i, env = fresh_state env
            let s_f, env = fresh_state env
            let t, env = fresh_var env
            let t_phi = Time_constraint(tc_of_spec t (Time_le x))
            let A, env = compile' env s_f (t_phi :: C) A
            let start_moment, env = next_moment env
            let end_moment, env = next_moment env
            { initial_state = s_i
              instructions = 
                  List.concat [ [ STATE s_i
                                  ASSUME C
                                  LD_TIME t
                                  ST_MOMENT start_moment
                                  JMP A.initial_state ]
                                A.instructions
                                [ STATE s_f
                                  ASSUME(t_phi :: C)
                                  ST_MOMENT end_moment
                                  SET_STATE on_finish
                                  JMP on_finish ] ] }, env
        | Within(Time_ge x, A) -> 
            let s_i, env = fresh_state env
            let s_wait, env = fresh_state env
            let s_go, env = fresh_state env
            let s_f, env = fresh_state env
            let t, env = fresh_var env
            let t_phi = Time_constraint(tc_of_spec t (Time_ge x))
            let t_phi_not = Time_constraint(Diff_lt(t, x))
            let A, env = compile' env s_f (t_phi :: C) A
            let moment, env = next_moment env
            { initial_state = s_i
              instructions = 
                  List.concat [ [ STATE s_i
                                  ASSUME C
                                  LD_TIME t
                                  JMP A.initial_state ]
                                A.instructions
                                [ STATE s_f
                                  NONDET s_wait
                                  ASSUME(t_phi :: C)
                                  ST_MOMENT moment
                                  JMP on_finish
                                  STATE s_wait
                                  ASSUME(t_phi_not :: C)
                                  SET_STATE on_finish
                                  JMP on_finish ] ] }, env
    
    let compile A = compile' empty_env (S 0) [] A
    