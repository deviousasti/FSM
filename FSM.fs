namespace FSM

type State<'a, 'b> = delegate of State<'a, 'b> * 'a -> State<'a, 'b> * 'b

type StateMachine<'a, 'b>(initial : State<'a, 'b>) = 
    let state = ref initial
    with member x.Current = !state
         member x.Move(value) =              
            let current = !state in let s, v = current.Invoke(current, value)
            System.Threading.Interlocked.Exchange(state, s) |> ignore
            v

module States =     
    let identity = State(fun s v -> s, v)
    let identityOpt = State(fun s v -> s, None)
    let zero = State(fun s _ -> (s, Unchecked.defaultof<'b>))

    let eval (s : State<_,_>) v = s.Invoke(s, v)
    let refer sref = State(fun _ v -> eval !sref v)    
    let defer factory = State(fun s v -> eval (factory()) v)
    let lazily (lazyState:Lazy<_>) = State(fun s v -> eval (lazyState.Force()) v)
    
    let bind selector state = State(fun s v -> eval (selector (snd (eval state v))) v)
    let returnValue value = State(fun s _ -> s, value)
     
    let rec map mapping state = State(fun s v -> let ns, nv = (eval state v) in (map mapping ns), mapping(nv))
    let rec mapin mapping state = State(fun s v -> let ns, nv = eval state (mapping v) in (mapin mapping ns), nv)
    
    let rec define fn initial = State(fun _ v -> let state = fn v initial in define fn state, state)
    let rec startWith list cap = State(fun _ v -> match list with x::xs -> (startWith xs cap, x) | [] -> eval cap v)    

    let skip state = State(fun _ v -> eval (fst (eval state v)) v)
    let rec skipn n state = if n > 0 then skipn (n - 1) (skip state) else state
    
    let ignore state = State(fun s v -> state, None)
    let rec ignoren n state = if n > 0 then ignoren (n - 1) (ignore state) else state
    let ignoreUntil pred state = let state' = map Some state in State(fun s v -> if pred(v) then eval state' v else (s, None))
  
    let moveIf pred defaultValue state = State(fun s v -> if pred(v) then (eval state v) else s, defaultValue)
    let moveIfElse pred defaultValue stateElse stateIf = State(fun s v -> if pred(v) then stateIf, defaultValue else stateElse, defaultValue)
    let switch cases defaultValue = State(fun s v -> match Seq.tryPick(fun pred -> pred(v)) cases with Some(state) -> eval state v | None -> s, defaultValue)    

    let doAction action state = State(fun _ v -> let s', v' = eval state v
                                                 do action v v'
                                                 s', v')

    let apply state sequence = seq { let sm = StateMachine(state)
                                     for s in sequence -> sm.Move(s) }

    let unfold state initial = let sm = StateMachine(state) 
                               let value = ref initial
                               Seq.initInfinite(fun _ -> value := sm.Move(!value); !value)