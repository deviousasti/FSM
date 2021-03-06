﻿FSM is a minimalist library for functionally defining state-machines in F#, although the approach may be generalised to any language supporting higher order functions. In FSM, a state is defined as a computation of the next state. This makes it possible to write state-machines as n-level DFAs or NFAs.

State
=====

The base of this approach is the ```State``` function:

	State<'a, 'b> = State<'a, 'b> * 'a -> State<'a, 'b> * 'b

A state takes in a state and an input, and returns the next state and an output. A higher order state has input values which are states.
To create a statemachine, supply it with the initial state and call ```Move``` to move the machine to the next state:

	sm = StateMachine(<intial state>)
	sm.Move(value)

Defining States
===============

From the definition ```(s, a) -> (s', b)```, the simplest state-machine we can define is one with a single state that transitions onto itself. (I: S → S)

	identity = State(fun s v -> s, v)

In situ
--------

Another simple two state machine can be an even-odd machine. (P: Even → Odd → Even)

	oddeven = State(fun s v -> State(fun _ v -> s, Odd), Even)

spelled out better as:

	oddeven = State(fun even v -> State(fun odd v -> even, Odd), Even)

Here, the next odd state is generated by the even state.

By name/binding
-----------

To define this in a more conventional way, we can express it like so:
        
    let rec odd = State(fun s v -> even, Odd) 
        and even = State(fun s v -> odd, Even)

This is easily defined in lazy languages like Haskell, but since F# has strict evaluation, the compiler will warn about recursive references.
To get around this, we can use also reference cells.

    odd := State(fun _ _ -> !even, Odd)
    even := State(fun _ _ -> !odd, Even)

The initial value of the reference cell can be set as ```States.zero```.

A state machine which checks if numbers are divisible by 3 given binary input:

    let rec A = State(fun s v -> if v = 0 then s, 0 else B, 0)
        and B = State(fun s v -> if v = 0 then C, 0 else A, 1)
        and C = State(fun s v -> if v = 0 then B, 0 else s, 0)    

Internal state
--------------

A recursive function can allow any type of state to be maintained internally.

    let evenodd = 
        let rec evenOddFn state = 
            let ns = match state with
                     | Even -> Odd
                     | Odd -> Even
            State(fun s v -> evenOddFn state, v)
        evenOddFn Even //initial state

This can be be rewritten using ```States.define``` as:

    let evenOddfn _ = function
    | Even -> Odd
    | Odd -> Even

	let evenodd = States.define evenOddfn Even


State Combinators
=================

State combinators are higher order functions which can be to declaratively combine states.

For example,

    let valuecards _ = function Number(n) when n > 1 -> Number(n - 1) | _ -> Joker

    let initial = 
        States.define valuecards (Number 10) 
        |> States.startWith [ Ace; Queen; King; Jack ] 

returns a sequence of all the cards (Ace, Queen, ... Number(1), Joker).

To execute side-effects on evaluating a state, the ```doAction``` combinator is useful:
        
    let rec odd = State(fun s v -> even, Odd)  |> States.doAction (printfn "odd: %A -> %A")
        and even = State(fun s v -> odd, Even) |> States.doAction (printfn "even: %A -> %A")


Similarly other combinators like ```moveIf``` and ```moveIfElse``` can be use to declaratively encode transitions.
The former example for divisible by 3 rewritten:

    let rec A = moveIf     ((=) 1)      1   (lazily (lazy B))
        and B = moveIfElse ((=) 0)      0 C (lazily (lazy A))
        and C = moveIf     ((=) 0)      0   (lazily (lazy B))

For example, to represent a lock which becomes active after entering the sequence ```[1, 2, 3]``` (any other number out of sequence causes it to go back to the starting state):

    let rec checkPin =       
       States.returnValue "Success"
       |> States.moveIfElse ((=) 3) "C3" init
       |> States.moveIfElse ((=) 2) "C2" init
       |> States.moveIfElse ((=) 1) "C1" init
    and init = States.lazily (lazy checkPin) 

By repeatedly applying this combinator, this can be generalized to be constructed around any list:

    let moveSeq endState list = 
        let rec move zero = function
            | [] -> endState
            | x::xs -> States.moveIfElse ((=) x) (sprintf "State: %A" x) zero (move zero xs)    
        let rec initial = move (States.lazily (lazy initial)) list
        initial

Most combinators are simplistic and are defined in a single line lambda function.