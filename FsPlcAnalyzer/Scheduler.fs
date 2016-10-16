namespace FsPlcAnalyzer

module Scheduler = 
    open Analysis
    
    type Schedule = C5.IPriorityQueue<Worklist_item>
    
    module Bfs = 
        let empty() = 
            C5.IntervalHeap<Worklist_item>
                ({ new System.Collections.Generic.IComparer<Worklist_item> with
                       member this.Compare(left, right) = left.path.Count.CompareTo(right.path.Count) })
        
        let inline schedule (Q : Schedule) s = ignore (Q.Add(s))
        
        let inline deschedule (Q : Schedule) () = 
            if Q.IsEmpty then None
            else Some(Q.DeleteMin())
