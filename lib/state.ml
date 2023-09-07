type ('a, 'b) continuation =
{ k : ('a, 'b) Effect.Shallow.continuation; id : int }

type 'a t =
  | Finished of ('a, exn) result
  | Suspended : ('a, 'b) continuation * 'a Effect.t -> 'b t

let effc eff id k = Suspended ({ k; id }, eff)

let handler_continue id =
  let open Effect.Shallow in
  let retc value = Finished (Ok value) in
  let exnc exn = Finished (Error exn) in
  let effc :
      type c. c Effect.t -> ((c, 'a) Effect.Shallow.continuation -> 'b) option =
   fun effect -> Some (effc effect id)
  in
  { retc; exnc; effc }

let continue_with : ('c, 'a) continuation -> 'c -> 'a t =
 fun k v ->
  Meio_runtime_events.note_switch k.id;
  Effect.Shallow.continue_with k.k v (handler_continue k.id)

let handler_discontinue exn =
  let open Effect.Shallow in
  let effc :
      type c. c Effect.t -> ((c, 'a) Effect.Shallow.continuation -> 'b) option =
    function
    | _ -> Some (Fun.const (Finished (Error exn)))
  in
  let retc = Fun.const (Finished (Error exn)) in
  let exnc = Fun.const (Finished (Error exn)) in
  { retc; exnc; effc }

let discontinue_with : ('c, 'a) continuation -> exn -> 'a t =
 fun k exn -> Effect.Shallow.discontinue_with k.k exn (handler_discontinue exn)

let suspended_with : ('c, 'a) continuation -> 'c Effect.t -> 'a t =
 fun k e -> Suspended (k, e)

let pure res = Finished res

let make k id v =
  let k = Effect.Shallow.fiber k in
  continue_with { k; id } v

type 'a step =
  | Send of 'a
  | Fail of exn
  | Intr
  | Cont : 'a Effect.t -> 'a step
  | Yield : unit step

type ('a, 'b) k = ('a step -> 'b t) -> 'a Effect.t -> 'b t
type perform = { perform: 'a 'b. ('a, 'b) k } [@@unboxed]

let once : type a. perform:perform -> a t -> a t =
 fun ~perform -> function
  | Finished _ as finished -> finished
  | Suspended (fn, e) as state ->
      let k : type c. (c, a) continuation -> c step -> a t =
       fun fn -> function
        | Send v -> continue_with fn v
        | Fail exn -> discontinue_with fn exn
        | Intr -> state
        | Cont e -> suspended_with fn e
        | Yield -> continue_with fn ()
      in
      perform.perform (k fn) e

exception Break

let is_finished = function Finished _ -> true | _ -> false

[@@@warning "-8"]

let run : type a. quanta:int -> perform:perform -> a t -> a t =
 fun ~quanta ~perform state ->
  let exception Yield of a t in
  let k : type c. (c, a) continuation -> c step -> a t =
   fun fn -> function
    | Send v -> continue_with fn v
    | Fail e -> discontinue_with fn e
    | Cont e -> suspended_with fn e
    | Intr -> raise_notrace Break
    | Yield -> raise_notrace (Yield (continue_with fn ()))
  in
  let quanta = ref quanta and state = ref state in
  try
    while !quanta > 0 && is_finished !state = false do
      let (Suspended (fn, e)) = !state in
      state := perform.perform (k fn) e;
      quanta := !quanta - 1
    done;
    !state
  with
  | Break -> !state
  | Yield state -> state

(* Please, don't... *)

[@@@warning "+8"]

let fail ~exn = function
  | Finished _ -> Finished (Error exn)
  | Suspended (k, _) -> discontinue_with k exn
