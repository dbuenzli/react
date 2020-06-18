(*---------------------------------------------------------------------------
   Copyright (c) 2009 The react programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

let err_max_rank = "maximal rank exceeded"
let err_sig_undef = "signal value undefined yet"
let err_fix = "trying to fix a delayed value"
let err_retain_never = "E.never cannot retain a closure"
let err_retain_cst_sig = "constant signals cannot retain a closure"
let err_step_executed = "step already executed"
let err_event_scheduled = "event already scheduled on a step"
let err_signal_scheduled = "signal already scheduled on a step"

module Wa = struct
  type 'a t = { mutable arr : 'a Weak.t; mutable len : int }
  (* The type for resizeable weak arrays.

     For now the arrays only grow. We could try to compact and
     downsize the array in scan_add if a threshold of empty slots is
     exceeded. *)

  let create size = { arr = Weak.create size; len = 0 }
  let length a = a.len
  let is_empty a =
    try
      for i = 0 to a.len - 1 do
        if Weak.check a.arr i then raise Exit;
      done;
      true
    with Exit -> false

  let clear a = a.arr <- Weak.create 0; a.len <- 0
  let get a i = Weak.get a.arr i
  let set a i = Weak.set a.arr i
  let swap a i i' =
    let v = Weak.get a.arr i' in
    Weak.blit a.arr i a.arr i' 1;     (* blit prevents i from becoming live. *)
    Weak.set a.arr i v

  let grow a =
    let arr' = Weak.create (2 * (a.len + 1)) in
    Weak.blit a.arr 0 arr' 0 a.len;
    a.arr <- arr'

  let add a v =                                   (* adds v at the end of a. *)
    if a.len = Weak.length a.arr then grow a;
    Weak.set a.arr a.len (Some v);
    a.len <- a.len + 1

  let scan_add a v =  (* adds v to a, tries to find an empty slot, O(a.len). *)
    try
      for i = 0 to a.len - 1 do
        match Weak.get a.arr i with
        | None -> Weak.set a.arr i (Some v); raise Exit | Some _ -> ()
      done;
      add a v
    with Exit -> ()

  let rem_last a = let l = a.len - 1 in (a.len <- l; Weak.set a.arr l None)
  let rem a v =       (* removes v from a, uses physical equality, O(a.len). *)
    try
      for i = 0 to a.len - 1 do
        match Weak.get a.arr i with
        | Some v' when  v == v' -> Weak.set a.arr i None; raise Exit
        | _ -> ()
      done
    with Exit -> ()

  let iter f a =
    for i = 0 to a.len - 1 do
      match Weak.get a.arr i with Some v -> f v | None -> ()
    done

  let fold f acc a =
    let acc = ref acc in
    for i = 0 to a.len - 1 do
      match Weak.get a.arr i with Some v -> acc := f !acc v | None -> ()
    done;
    !acc
end

type node =
  { mutable rank : int;        (* its rank (height) in the dataflow graph. *)
    mutable stamp : step;          (* last step in which it was scheduled. *)
    mutable retain : unit -> unit; (* retained by the node, NEVER invoked. *)
    mutable producers : unit -> node list;   (* nodes on which it depends. *)
    mutable update : step -> unit;                      (* update closure. *)
    deps : node Wa.t }              (* weak references to dependent nodes. *)
(* The type for nodes.

   Each event and (non-constant) signal has an associated node. The
   fields producers and update keep, in their closure environment,
   references to mutables (see later) on which the node depends.
   Defining their contents via a let rec allows the environment to be
   shared by the two closures.

   There are special nodes to represent infinitesimally delayed nodes
   (needed for recursive definitions). These nodes all have a rank of
   Node.delayed_rank and depend only on the node they delay. Since
   they have the highest rank possible they are updated only at the
   end of the step and treated specially at that point (see
   Step.execute). *)

and step =
  { mutable over : bool;                    (* true when the step is over. *)
    mutable heap : heap;              (* min-heap of nodes sorted by rank. *)
    mutable eops : (unit -> unit) list;         (* end of step operations. *)
    mutable cops : (unit -> unit) list }       (* cleanup step operations. *)
(* The type for update steps.

   Note for historical reasons we use the variable names [c] and [c']
   in the code for representing update steps.

   There are four successive phases in the execution of a step c (see
   Step.execute).

   1. Nodes are updated in topological order until c.heap is empty or
      we reach a delayed node.

   2. End of step operations are executed. This may add new
      dependencies (see S.diff and S.changes) and clear the occurence
      of delayed events from a previous step (but used in this
      step).

   3. If there are delayed nodes in c.heap, we create a new step
      c'. Each delayed node is updated and its dependents are put in
      c'.heap. For delayed events, an end of step operation is added
      in c' to clear the occurence at step 2 of c'. Delayed nodes are
      updated in any order as a delayed node updating in a step
      cannot depend on a delayed node updating in the same step.

   4. Cleanup operations are executed. This clears the event occurences of
      non-delayed event that occured in c.

   After this, if a step c' was created in 3. the step gets executed. *)

and heap = node Wa.t
(* The type for heaps.

   Weak min-heaps of nodes sorted according to their rank. Classic
   imperative implementation with a twist to accomodate the fact
   that nodes may disappear.

   The heap property we maintain is that for any node its descendents
   (vs. children) are either of no smaller rank or they are None. None
   nodes need to be treated specially in percolate up and down. The
   reason is that it blocks information about the rank of their
   descendents.  In percolate down the solution is to systematically
   swap with None children.  So do we in percolate up, however, in
   that case we may violate the property if we swap with a None node
   and stop right after (either because we got the root or we found a
   parent of smaller rank), the property can however be reestablished
   by percolating down from that point. *)

type 'a emut =
  { ev : 'a option ref;     (* during steps, holds a potential occurence. *)
    enode : node; }                                    (* associated node. *)

type 'a event = Never | Emut of 'a emut
(* The type for events.

   An event is either the never occuring event Never or a mutable
   Emut.  A mutable m has some value in m.v iff a step is being
   executed and m has an occurence in the step. m's dependents are
   scheduled for update iff m has a value in m.v.

   Mutables that occur in a step are set back to None when the step
   terminates with an cleanup step operation (see eupdate and
   Step.execute). To avoid a weak reference on m in the cleanup
   operation, the field m.v is a field on a reference instead of a
   mutable field.

   A new node n can be made dependent on a an event mutable m during a
   step. But when n is added to m's dependents, m may already have
   updated and scheduled its dependents. In that case n also need to
   be scheduled (see E.add_dep). If m only occurs later in the step,
   the n will be scheduled as usual with the others. *)

type 'a smut =
  { mutable sv : 'a option;         (* signal value (None only temporary). *)
    eq : 'a -> 'a -> bool;              (* to detect signal value changes. *)
    snode : node }                                     (* associated node. *)

type 'a signal = Const of 'a | Smut of 'a smut
(* The type for signals.

   A signal is either a constant signal Const or a mutable Smut.  A
   mutable m has a value in m.v iff m.v initialized. m's dependents
   are scheduled for update iff m is initialized and m.v changed
   according to m.eq in the step.

   Signal initialization occurs as follows. If we have an init. value
   we set the signal's value to this value and then :

   1. If the creation occurs outside a step, the signal's update
      function is invoked with Step.nil. This may overwrite the
      init. value, but no dependent will see this change as there
      cannot be any at that time.

   2. If the creation occurs inside a step, the signal is scheduled
      for update. Here again this may overwrite the init. value. If
      the new value is equal to the init. value this will not schedule
      the signals' dependents. However this is not a problem since
      dependents are either new signals and will be scheduled via the
      init. process or a new dependency added by S.switch in which
      case this dependent is also be scheduled.

  Note that in both cases if we had no init. value, the call to the
  update function must unconditionaly write a concrete value for the
  signal.

  To find out whether the creation occurs in a step we walk back the
  signal's producers recursively looking for a node stamp with an
  unfinished step (see Step.find_unfinished). This is not in favor
  of static signal creation but this is the price we have to pay for
  not having global data structures.

  A new node n can be made dependent on a signal mutable m during a
  step. In contrast to events (see above) nothing special has to be
  done. Here's the rationale :

  1. If n is the node of a new event then either the event cannot
     happen in the same step and thus the depency addition occurs at
     the end of the step (S.diff, S.changes) or the event cares only
     about having an up to date value if some other event occurs
     (S.sample, E.on) in the same step and the rank of n ensures
     this.

  2. If n is the node of a new signal then n cares only about having
     m's up to date values whenever n will initialize and the rank of
     n ensures this. *)

module H = struct
  let size = Wa.length
  let els h = Wa.fold (fun acc e -> e :: acc) [] h  (*  no particular order. *)
  let compare_down h i i' = match Wa.get h i, Wa.get h i' with
  | Some n, Some n' -> compare n.rank n'.rank
  | Some _, None -> 1                      (* None is smaller than anything. *)
  | None, Some _ -> -1                     (* None is smaller than anything. *)
  | None, None -> 0

  let rec down h i =
    let last = size h - 1 in
    let start = 2 * i in
    let l = start + 1 in                                (* left child index. *)
    let r = start + 2 in                               (* right child index. *)
    if l > last then () (* no child, stop *) else
    let child =                                  (* index of smallest child. *)
      if r > last then l else (if compare_down h l r < 0 then l else r)
    in
    if compare_down h i child > 0 then (Wa.swap h i child; down h child)

  let up h i =
    let rec aux h i last_none =
      if i = 0 then (if last_none then down h 0) else
      let p = (i - 1) / 2 in                                (* parent index. *)
      match Wa.get h i, Wa.get h p with
      | Some n, Some n' ->
          if compare n.rank n'.rank < 0 then (Wa.swap h i p; aux h p false) else
          (if last_none then down h i)
      | Some _, None ->
          Wa.swap h i p; aux h p true
      | None, _ -> ()
    in
    aux h i false

  let rebuild h = for i = (size h - 2) / 2 downto 0 do down h i done
  let add h n = Wa.add h n; up h (size h - 1)
  let rec take h =
    let s = size h in
    if s = 0 then None else
    let v = Wa.get h 0 in
    begin
      if s > 1
      then (Wa.set h 0 (Wa.get h (s - 1)); Wa.rem_last h; down h 0)
      else Wa.rem_last h
    end;
    match v with None -> take h | v -> v
end

let delayed_rank = max_int

module Step = struct                                       (* Update steps. *)
  type t = step
  let nil = { over = true; heap = Wa.create 0; eops = []; cops = []}

  let create () =
    let h = Wa.create 11 in
    { over = false; heap = h; eops = []; cops = []}

  let add c n = if n.stamp == c then () else (n.stamp <- c; H.add c.heap n)
  let add_deps c n = Wa.iter (add c) n.deps
  let add_eop c op = c.eops <- op :: c.eops
  let add_cop c op = c.cops <- op :: c.cops
  let allow_reschedule n = n.stamp <- nil
  let rebuild c = H.rebuild c.heap

  let rec execute c =
    let eops c = List.iter (fun op -> op ()) c.eops; c.eops <- [] in
    let cops c = List.iter (fun op -> op ()) c.cops; c.cops <- [] in
    let finish c = c.over <- true; c.heap <- Wa.create 0 in
    let rec update c = match H.take c.heap with
    | Some n when n.rank <> delayed_rank -> n.update c; update c
    | Some n ->
        let c' = create () in
        eops c; List.iter (fun n -> n.update c') (n :: H.els c.heap); cops c;
        finish c;
        execute c'
    | None -> eops c; cops c; finish c
    in
    update c

  let execute c = if c.over then invalid_arg err_step_executed else execute c


  let find_unfinished nl = (* find unfinished step in recursive producers. *)
    let rec aux next = function            (* zig-zag breadth-first search. *)
    | [] -> if next = [] then nil else aux [] next
    | [] :: todo -> aux next todo
    | nl :: todo -> find next todo nl
    and find next todo = function
    | [] -> aux next todo
    | n :: nl ->
        if not n.stamp.over then n.stamp else
        find (n.producers () :: next) todo nl
    in
    aux [] [ nl ]
end

module Node = struct
  let delayed_rank = delayed_rank
  let min_rank = min_int
  let max_rank = delayed_rank - 1

  let nop _ = ()
  let no_producers () = []
  let create r =
    { rank = r; stamp = Step.nil; update = nop; retain = nop;
      producers = no_producers; deps = Wa.create 0 }

  let rem_dep n n' = Wa.rem n.deps n'
  let add_dep n n' = Wa.scan_add n.deps n'
  let has_dep n = not (Wa.is_empty n.deps)
  let deps n = Wa.fold (fun acc d -> d :: acc) [] n.deps

  let bind n p u = n.producers <- p; n.update <- u
  let stop ?(strong = false) n =
    if not strong then begin
      n.producers <- no_producers; n.update <- nop; Wa.clear n.deps;
    end else begin
      let rec loop next to_rem = function
      | [] ->
          begin match next with
          | (to_rem, prods) :: next -> loop next to_rem prods
          | [] -> ()
          end
      | n :: todo ->
          rem_dep n to_rem;   (* N.B. rem_dep could be combined with has_dep *)
          if n.rank = min_rank (* is a primitive *) || has_dep n
          then loop next to_rem todo else
          begin
            let prods = n.producers () in
            n.producers <- no_producers; n.update <- nop; Wa.clear n.deps;
            loop ((n, prods) :: next) to_rem todo
          end
      in
      let producers = n.producers () in
      n.producers <- no_producers; n.update <- nop; Wa.clear n.deps;
      loop [] n producers
    end

  let set_rank n r = n.rank <- r
  let rmin = create min_rank
  let rmax n n' = if n.rank > n'.rank then n else n'
  let rsucc n =
    if n.rank = delayed_rank then min_rank else
    if n.rank < max_rank then n.rank + 1 else invalid_arg err_max_rank

  let rsucc2 n n' =
    let r = rsucc n in
    let r' = rsucc n' in
    if r > r' then r else r'

 (* Rank updates currently only increases ranks. If this is problematic
    udpate ranks orthodoxly by taking the succ of the max of n.producers.
    Note that rank update stops at delayed nodes (otherwise we would
    loop and blow the ranks). *)
  let update_rank n r =              (* returns true iff n's rank increased. *)
    let rec aux = function
    | []  -> ()
    | n :: todo ->
        let update todo d =
          if n.rank < d.rank || n.rank = delayed_rank then todo else
          (d.rank <- rsucc n; d :: todo)
        in
        aux (Wa.fold update todo n.deps)
    in
    if r > n.rank then (n.rank <- r; aux [ n ]; true) else false
end

(* Shortcuts *)

let rsucc = Node.rsucc
let rsucc2 = Node.rsucc2
let rmax = Node.rmax

(* Event value, creation and update *)

let eval m = match !(m.ev) with Some v -> v | None -> assert false
let emut rank = { ev = ref None; enode = Node.create rank }
let event m p u = Node.bind m.enode p u; Emut m
let eupdate v m c =
  let clear v () = v := None in
  m.ev := Some v;
  Step.add_cop c (clear m.ev);
  Step.add_deps c m.enode

(* Signal value, creation and update *)

let sval m = match m.sv with Some v -> v | None -> assert false
let smut rank eq = { sv = None; eq = eq; snode = Node.create rank }
let signal ?i m p u =
  Node.bind m.snode p u;
  begin match i with Some _ as v -> m.sv <- v | None -> () end;
  begin match Step.find_unfinished (m.snode.producers ()) with
  | c when c == Step.nil -> m.snode.update Step.nil
  | c -> Step.add c m.snode
  end;
  Smut m

let supdate v m c = match m.sv with
| Some v' when (m.eq v v') -> ()
| Some _ -> m.sv <- Some v; if c != Step.nil then Step.add_deps c m.snode
| None -> m.sv <- Some v                       (* init. without init value. *)

module E = struct
  type 'a t = 'a event

  let add_dep m n =
    Node.add_dep m.enode n;
    if !(m.ev) <> None then Step.add m.enode.stamp n

  let send m ?step v = match step with          (* sends an event occurence. *)
  | Some c ->
      if c.over then invalid_arg err_step_executed else
      if not m.enode.stamp.over then invalid_arg err_event_scheduled else
      m.enode.stamp <- c;
      eupdate v m c
  | None ->
      let c = Step.create () in
      m.enode.stamp <- c;
      eupdate v m c;
      Step.execute c

  (* Basics *)

  let never = Never
  let create () =
    let m = emut Node.min_rank in
    Emut m, send m

  let retain e c = match e with
  | Never -> invalid_arg err_retain_never
  | Emut m -> let c' = m.enode.retain in (m.enode.retain <- c); (`R c')

  let stop ?strong = function Never -> () | Emut m -> Node.stop ?strong m.enode
  let equal e e' = match e, e' with
  | Never, Never -> true
  | Never, _ | _, Never -> false
  | Emut m, Emut m' -> m == m'

  let trace ?(iff = Const true) t e = match iff with
  | Const false -> e
  | Const true ->
      begin match e with
      | Never -> e
      | Emut m ->
          let m' = emut (rsucc m.enode) in
          let rec p () = [ m.enode ]
          and u c = let v = eval m in t v; eupdate v m' c in
          add_dep m m'.enode;
          event m' p u
      end
  | Smut mc ->
      match e with
      | Never -> Never
      | Emut m ->
          let m' = emut (rsucc2 mc.snode m.enode) in
          let rec p () = [mc.snode; m.enode]
          and u c = match !(m.ev) with
          | None -> ()                                        (* mc updated. *)
          | Some v -> if (sval mc) then t v; eupdate v m' c
          in
          Node.add_dep mc.snode m'.enode;
          add_dep m m'.enode;
          event m' p u

  (* Transforming and filtering *)

  let once = function
  | Never -> Never
  | Emut m ->
      let m' = emut (rsucc m.enode) in
      let rec p () = [ m.enode ]
      and u c =
        Node.rem_dep m.enode m'.enode;
        eupdate (eval m) m' c;
        Node.stop m'.enode
      in
      add_dep m m'.enode;
      event m' p u

  let drop_once = function
  | Never -> Never
  | Emut m ->
      let m' = emut (rsucc m.enode) in
      let rec p () = [ m.enode ]
      and u c =                                           (* first update. *)
        let u' c = eupdate (eval m) m' c in         (* subsequent updates. *)
        Node.bind m'.enode p u'
      in
      add_dep m m'.enode;
      event m' p u

  let app ef = function
  | Never -> Never
  | Emut m ->
      match ef with
      | Never -> Never
      | Emut mf ->
          let m' = emut (rsucc2 m.enode mf.enode) in
          let rec p () = [ m.enode; mf.enode ]
          and u c = match !(mf.ev), !(m.ev) with
          | None, _ | _, None -> ()
          | Some f, Some v -> eupdate (f v) m' c
          in
          add_dep m m'.enode;
          add_dep mf m'.enode;
          event m' p u

  let map f = function
  | Never -> Never
  | Emut m ->
      let m' = emut (rsucc m.enode) in
      let rec p () = [ m.enode ]
      and u c = eupdate (f (eval m)) m' c in
      add_dep m m'.enode;
      event m' p u

  let stamp e v = match e with
  | Never -> Never
  | Emut m ->
      let m' = emut (rsucc m.enode) in
      let rec p () = [ m.enode ]
      and u c = eupdate v m' c in
      add_dep m m'.enode;
      event m' p u

  let filter pred = function
  | Never -> Never
  | Emut m ->
      let m' = emut (rsucc m.enode) in
      let rec p () = [ m.enode ]
      and u c = let v = eval m in if pred v then eupdate v m' c else () in
      add_dep m m'.enode;
      event m' p u

  let fmap fm = function
  | Never -> Never
  | Emut m ->
      let m' = emut (rsucc m.enode) in
      let rec p () = [ m.enode ]
      and u c = match fm (eval m) with Some v -> eupdate v m' c | None -> ()
      in
      add_dep m m'.enode;
      event m' p u

  let diff d = function
  | Never -> Never
  | Emut m ->
      let m' = emut (rsucc m.enode) in
      let last = ref None in
      let rec p () = [ m.enode ]
      and u c =
        let v = eval m in
        match !last with
        | None -> last := Some v
        | Some v' -> last := Some v; eupdate (d v v') m' c
      in
      add_dep m m'.enode;
      event m' p u

  let changes ?(eq = ( = )) = function
  | Never -> Never
  | Emut m ->
      let m' = emut (rsucc m.enode) in
      let last = ref None in
      let rec p () = [ m.enode ]
      and u c =
        let v = eval m in
        match !last with
        | None -> last := Some v; eupdate v m' c
        | Some v' -> last := Some v; if eq v v' then () else eupdate v m' c
      in
      add_dep m m'.enode;
      event m' p u

  let on c = function
  | Never -> Never
  | Emut m as e ->
      match c with
      | Const true -> e
      | Const false -> Never
      | Smut mc ->
          let m' = emut (rsucc2 m.enode mc.snode) in
          let rec p () = [ m.enode; mc.snode ]
          and u c = match !(m.ev) with
          | None -> ()                                     (* mc updated. *)
          | Some _ -> if (sval mc) then eupdate (eval m) m' c else ()
          in
          add_dep m m'.enode;
          Node.add_dep mc.snode m'.enode;
          event m' p u

  let when_ = on

  let dismiss c = function
  | Never -> Never
  | Emut m as e ->
      match c with
      | Never -> e
      | Emut mc ->
          let m' = emut (rsucc2 mc.enode m.enode) in
          let rec p () = [ mc.enode; m.enode ]
          and u c = match !(mc.ev) with
          | Some _ -> ()
          | None -> eupdate (eval m) m' c
          in
          add_dep mc m'.enode;
          add_dep m m'.enode;
          event m' p u

  let until c = function
  | Never -> Never
  | Emut m as e ->
      match c with
      | Never -> e
      | Emut mc ->
          let m' = emut (rsucc2 m.enode mc.enode) in
          let rec p () = [ m.enode; mc.enode] in
          let u c = match !(mc.ev) with
          | None -> eupdate (eval m) m' c
          | Some _ ->
              Node.rem_dep m.enode m'.enode;
              Node.rem_dep mc.enode m'.enode;
              Node.stop m'.enode
          in
          add_dep m m'.enode;
          add_dep mc m'.enode;
          event m' p u

  (* Accumulating *)

  let accum ef i = match ef with
  | Never -> Never
  | Emut m ->
      let m' = emut (rsucc m.enode) in
      let acc = ref i in
      let rec p () = [ m.enode ]
      and u c = acc := (eval m) !acc; eupdate !acc m' c in
      add_dep m m'.enode;
      event m' p u

  let fold f i = function
  | Never -> Never
  | Emut m ->
      let m' = emut (rsucc m.enode) in
      let acc = ref i in
      let rec p () = [ m.enode ]
      and u c = acc := f !acc (eval m); eupdate !acc m' c in
      add_dep m m'.enode;
      event m' p u

  (* Combining *)

  let occurs m = !(m.ev) <> None
  let find_muts_and_next_rank el =
    let rec aux acc max = function
    | [] -> List.rev acc, rsucc max
    | (Emut m) :: l -> aux (m :: acc) (rmax max m.enode) l
    | Never :: l -> aux acc max l
    in
    aux [] Node.rmin el

  let select el =
    let emuts, r = find_muts_and_next_rank el in
    let m' = emut r in
    let rec p () = List.rev_map (fun m -> m.enode) emuts
    and u c = try eupdate (eval (List.find occurs emuts)) m' c with
    | Not_found -> assert false
    in
    List.iter (fun m -> add_dep m m'.enode) emuts;
    event m' p u

  let merge f a el =
    let rec fold f acc = function
    | m :: l when occurs m -> fold f (f acc (eval m)) l
    | m :: l -> fold f acc l
    | [] -> acc
    in
    let emuts, r = find_muts_and_next_rank el in
    let m' = emut r in
    let rec p () = List.rev_map (fun m -> m.enode) emuts
    and u c = eupdate (fold f a emuts) m' c in
    List.iter (fun m -> add_dep m m'.enode) emuts;
    event m' p u

  let switch e = function
  | Never -> e
  | Emut ms ->
      let r = match e with
      | Emut m -> rsucc2 m.enode ms.enode | Never -> rsucc ms.enode
      in
      let m' = emut r in
      let src = ref e in                          (* current event source. *)
      let rec p () = match !src with
      | Emut m -> [ m.enode; ms.enode ] | Never -> [ ms.enode ]
      and u c = match !(ms.ev) with
      | None -> (match !src with                       (* only src occurs. *)
        | Emut m -> eupdate (eval m) m' c | Never -> assert false)
      | Some e ->
          begin match !src with
          | Emut m -> Node.rem_dep m.enode m'.enode | Never -> ()
          end;
          src := e;
          match e with
          | Never -> ignore (Node.update_rank m'.enode (rsucc ms.enode))
          | Emut m ->
              Node.add_dep m.enode m'.enode;
              if Node.update_rank m'.enode (rsucc2 m.enode ms.enode) then
                begin
                  (* Rank increased because of m. Thus m may stil
                     update and we may be rescheduled. If it happens
                     we'll be in the other branch without any harm
                     but some redundant computation. *)
                  Step.allow_reschedule m'.enode;
                  Step.rebuild c;
                end
              else
              (* No rank increase, m already updated if needed. *)
              (match !(m.ev) with Some v -> eupdate v m' c | None -> ())
      in
      (match e with Emut m -> add_dep m m'.enode | Never -> ());
      add_dep ms m'.enode;
      event m' p u

  let fix f =
    let m = emut Node.delayed_rank in
    let e = event m (fun () -> []) (fun _ -> assert false) in
    match f e with
    | Never, r -> r
    | Emut m', r ->
        if m'.enode.rank = Node.delayed_rank then invalid_arg err_fix;
        let rec p () = [ (* avoid cyclic dep. *) ]
        and u c =                               (* N.B. c is the next step. *)
          let clear v () = v := None in
          m.ev := Some (eval m');
          Step.add_eop c (clear m.ev);   (* vs. add_cop for regular events. *)
          Step.add_deps c m.enode
        in
        Node.bind m.enode p u;
        add_dep m' m.enode;
        r

  (* Lifting *)

  let l1 = map
  let l2 f e0 e1 = match e0, e1 with
  | Never, _ -> Never
  | _, Never -> Never
  | Emut m0, Emut m1 ->
      let r = rsucc2 m0.enode m1.enode in
      let m' = emut r in
      let rec p () = [ m0.enode; m1.enode ] in
      let u c = match !(m0.ev), !(m1.ev)  with
      | None, _
      | _, None -> ()
      | Some v0, Some v1 -> eupdate (f v0 v1) m' c
      in
      add_dep m0 m'.enode;
      add_dep m1 m'.enode;
      event m' p u

  let l3 f e0 e1 e2 = match e0, e1, e2 with
  | Never, _, _ -> Never
  | _, Never, _ -> Never
  | _, _, Never -> Never
  | Emut m0, Emut m1, Emut m2 ->
      let r = rsucc (rmax (rmax m0.enode m1.enode) m2.enode) in
      let m' = emut r in
      let rec p () = [ m0.enode; m1.enode; m2.enode ] in
      let u c = match !(m0.ev), !(m1.ev), !(m2.ev)  with
      | None, _, _
      | _, None, _
      | _, _, None -> ()
      | Some v0, Some v1, Some v2 -> eupdate (f v0 v1 v2) m' c
      in
      add_dep m0 m'.enode;
      add_dep m1 m'.enode;
      add_dep m2 m'.enode;
      event m' p u


  let l4 f e0 e1 e2 e3 = match e0, e1, e2, e3 with
  | Never, _, _, _ -> Never
  | _, Never, _, _ -> Never
  | _, _, Never, _ -> Never
  | _, _, _, Never -> Never
  | Emut m0, Emut m1, Emut m2, Emut m3 ->
      let r = rsucc (rmax (rmax m0.enode m1.enode) (rmax m2.enode m3.enode)) in
      let m' = emut r in
      let rec p () = [ m0.enode; m1.enode; m2.enode; m3.enode ] in
      let u c = match !(m0.ev), !(m1.ev), !(m2.ev), !(m3.ev) with
      | None, _, _, _
      | _, None, _, _
      | _, _, None, _
      | _, _, _, None -> ()
      | Some v0, Some v1, Some v2, Some v3 -> eupdate (f v0 v1 v2 v3) m' c
      in
      add_dep m0 m'.enode;
      add_dep m1 m'.enode;
      add_dep m2 m'.enode;
      add_dep m3 m'.enode;
      event m' p u

  let l5 f e0 e1 e2 e3 e4 = match e0, e1, e2, e3, e4 with
  | Never, _, _, _, _ -> Never
  | _, Never, _, _, _ -> Never
  | _, _, Never, _, _ -> Never
  | _, _, _, Never, _ -> Never
  | _, _, _, _, Never -> Never
  | Emut m0, Emut m1, Emut m2, Emut m3, Emut m4 ->
      let r =
        rsucc (rmax (rmax (rmax m0.enode m1.enode) (rmax m2.enode m3.enode))
          m4.enode)
      in
      let m' = emut r in
      let rec p () = [ m0.enode; m1.enode; m2.enode; m3.enode; m4.enode ] in
      let u c = match !(m0.ev), !(m1.ev), !(m2.ev), !(m3.ev), !(m4.ev) with
      | None, _, _, _, _
      | _, None, _, _, _
      | _, _, None, _, _
      | _, _, _, None, _
      | _, _, _, _, None -> ()
      | Some v0, Some v1, Some v2, Some v3, Some v4 ->
          eupdate (f v0 v1 v2 v3 v4) m' c
      in
      add_dep m0 m'.enode;
      add_dep m1 m'.enode;
      add_dep m2 m'.enode;
      add_dep m3 m'.enode;
      add_dep m4 m'.enode;
      event m' p u

  let l6 f e0 e1 e2 e3 e4 e5 = match e0, e1, e2, e3, e4, e5 with
  | Never, _, _, _, _, _ -> Never
  | _, Never, _, _, _, _ -> Never
  | _, _, Never, _, _, _ -> Never
  | _, _, _, Never, _, _ -> Never
  | _, _, _, _, Never, _ -> Never
  | _, _, _, _, _, Never -> Never
  | Emut m0, Emut m1, Emut m2, Emut m3, Emut m4, Emut m5 ->
      let r =
        rsucc (rmax (rmax (rmax m0.enode m1.enode) (rmax m2.enode m3.enode))
          (rmax m4.enode m5.enode))
      in
      let m' = emut r in
      let rec p () = [ m0.enode; m1.enode; m2.enode; m3.enode; m4.enode;
                       m5.enode; ] in
      let u c = match !(m0.ev), !(m1.ev), !(m2.ev), !(m3.ev), !(m4.ev),
                      !(m5.ev) with
      | None, _, _, _, _, _
      | _, None, _, _, _, _
      | _, _, None, _, _, _
      | _, _, _, None, _, _
      | _, _, _, _, None, _
      | _, _, _, _, _, None -> ()
      | Some v0, Some v1, Some v2, Some v3, Some v4, Some v5 ->
          eupdate (f v0 v1 v2 v3 v4 v5) m' c
      in
      add_dep m0 m'.enode;
      add_dep m1 m'.enode;
      add_dep m2 m'.enode;
      add_dep m3 m'.enode;
      add_dep m4 m'.enode;
      add_dep m5 m'.enode;
      event m' p u

  (* Stdlib support *)

  module Option = struct
    let some e = map (fun v -> Some v) e
    let value ?default e = match default with
    | None -> fmap (fun v -> v) e
    | Some (Const dv) -> map (function None -> dv | Some v -> v) e
    | Some (Smut ms) ->
        match e with
        | Never -> Never
        | Emut m ->
            let m' = emut (rsucc2 m.enode ms.snode) in
            let rec p () = [ m.enode; ms.snode ]
            and u c = match !(m.ev) with
            | None -> () (* ms updated. *)
            | Some None -> eupdate (sval ms) m' c
            | Some Some v -> eupdate v m' c
            in
            add_dep m m'.enode;
            Node.add_dep ms.snode m'.enode;
            event m' p u
  end
end

module S = struct
  type 'a t = 'a signal

  let set_sval v m c = m.sv <- Some v; Step.add_deps c m.snode
  let set m ?step v =                             (* starts an update step. *)
    if m.eq (sval m) v then () else
    match step with
    | Some c ->
        if c.over then invalid_arg err_step_executed else
        if not m.snode.stamp.over then invalid_arg err_signal_scheduled else
        m.snode.stamp <- c;
        m.sv <- Some v;
        Step.add_deps c m.snode
    | None ->
        let c = Step.create () in
        m.snode.stamp <- c;
        m.sv <- Some v;
        Step.add_deps c m.snode;
        Step.execute c

  let end_of_step_add_dep ?(post_add_op = fun () -> ()) ~stop_if_stopped m m' =
    (* In some combinators, when the semantics of event m' is such
       that it should not occur in the (potential) step it is created,
       we add the dependency [m'] to signal [m] only via an end of
       step operation to avoid being scheduled in the step. *)
    match Step.find_unfinished (m.snode.producers ()) with
    | c when c == Step.nil ->
        Node.add_dep m.snode m'.enode;
        post_add_op ();
    | c ->
        let add_dep () =
          if m.snode.update == Node.nop then
            (* m stopped in step *)
            (if stop_if_stopped then Node.stop m'.enode)
          else
          begin
            ignore (Node.update_rank m'.enode (rsucc m.snode));
            Node.add_dep m.snode m'.enode;
            post_add_op ();
          end
        in
        Step.add_eop c add_dep

  (* Basics *)

  let const v = Const v
  let create ?(eq = ( = )) v =
    let m = smut Node.min_rank eq in
    m.sv <- Some v;
    Smut m, set m

  let retain s c = match s with
  | Const _ -> invalid_arg err_retain_cst_sig
  | Smut m -> let c' = m.snode.retain in m.snode.retain <- c; (`R c')

  let eq_fun = function Const _ -> None | Smut m -> Some m.eq

  let value = function
  | Const v | Smut { sv = Some v }  -> v
  | Smut { sv = None } -> failwith err_sig_undef

  let stop ?strong = function
  | Const _ -> ()
  | Smut m ->
      match m.sv with
      | Some _ -> Node.stop ?strong m.snode
      | None ->
          (* The signal was dynamically created and didn't update yet. Add the
             stop as an end of step operation. *)
          match Step.find_unfinished (m.snode.producers ()) with
          | c when c == Step.nil -> assert false
          | c ->
              let stop () = Node.stop ?strong m.snode in
              Step.add_eop c stop

  let equal ?(eq = ( = )) s s' = match s, s' with
  | Const v, Const v' -> eq v v'
  | Const _, _ | _, Const _ -> false
  | Smut m, Smut m' -> m == m'

  let trace ?(iff = const true) t s = match iff with
  | Const false -> s
  | Const true ->
      begin match s with
      | Const v -> t v; s
      | Smut m ->
          let m' = smut (rsucc m.snode) m.eq in
          let rec p () = [ m.snode ] in
          let u c = let v = sval m in t v; supdate v m' c in
          Node.add_dep m.snode m'.snode;
          signal m' p u
      end
  | Smut mc ->
      match s with
      | Const v ->
          let m' = smut (rsucc mc.snode) ( = ) (* we don't care about eq *) in
          let rec p () = [ mc.snode ]
          and u c =
            if (sval mc) then t v;
            Node.rem_dep mc.snode m'.snode;
            Node.stop m'.snode;
          in
          Node.add_dep mc.snode m'.snode;
          signal ~i:v m' p u
      | Smut m ->
          let m' = smut (rsucc2 mc.snode m.snode) m.eq in
          let rec p () = [ mc.snode; m.snode ]
          and u c =
            let v = sval m in
            match m'.sv with
            | Some v' when m'.eq v v' -> ()                   (* mc updated. *)
            | _ -> if (sval mc) then t v; supdate v m' c    (* init or diff. *)
          in
          Node.add_dep mc.snode m'.snode;
          Node.add_dep m.snode m'.snode;
          signal m' p u

  (* From events *)

  let hold ?(eq = ( = )) i = function
  | Never -> Const i
  | Emut m ->
      let m' = smut (rsucc m.enode) eq in
      let rec p () = [ m.enode ]
      and u c = match !(m.ev) with
      | None -> ()                                          (* init. only. *)
      | Some v -> supdate v m' c
      in
      E.add_dep m m'.snode;
      signal ~i m' p u

  (* Filtering and transforming *)

  let map ?(eq = ( = )) f = function
  | Const v -> Const (f v)
  | Smut m ->
      let m' = smut (rsucc m.snode) eq  in
      let rec p () = [ m.snode ]
      and u c = supdate (f (sval m)) m' c in
      Node.add_dep m.snode m'.snode;
      signal m' p u

  let app ?(eq = ( = )) sf sv = match sf, sv with
  | Smut mf, Smut mv ->
      let m' = smut (rsucc2 mf.snode mv.snode) eq in
      let rec p () = [ mf.snode; mv.snode ]
      and u c = supdate ((sval mf) (sval mv)) m' c in
      Node.add_dep mf.snode m'.snode;
      Node.add_dep mv.snode m'.snode;
      signal m' p u
  | Const f, Const v -> Const (f v)
  | Const f, sv -> map ~eq f sv
  | Smut mf, Const v ->
      let m' = smut (rsucc mf.snode) eq in
      let rec p () = [ mf.snode ]
      and u c = supdate ((sval mf) v) m' c in
      Node.add_dep mf.snode m'.snode;
      signal m' p u

  let filter ?(eq = ( = )) pred i = function
  | Const v as s -> if pred v then s else Const i
  | Smut m ->
      let m' = smut (rsucc m.snode) eq in
      let rec p () = [ m.snode ]
      and u c = let v = sval m in if pred v then supdate v m' c else () in
      Node.add_dep m.snode m'.snode;
      signal ~i m' p u

  let fmap ?(eq = ( = )) fm i = function
  | Const v -> (match fm v with Some v' -> Const v' | None -> Const i)
  | Smut m ->
      let m' = smut (rsucc m.snode) eq in
      let rec p () = [ m.snode ]
      and u c = match fm (sval m) with Some v -> supdate v m' c | None -> ()
      in
      Node.add_dep m.snode m'.snode;
      signal ~i m' p u

  let diff d = function
  | Const _ -> Never
  | Smut m ->
      let m' = emut (rsucc m.snode) in
      let last = ref None in
      let rec p () = [ m.snode ]
      and u c =
        let v = sval m in
        match !last with
        | Some v' -> last := Some v; eupdate (d v v') m' c
        | None -> assert false
      in
      let post_add_op () = last := Some (sval m) in
      end_of_step_add_dep ~post_add_op ~stop_if_stopped:true m m';
      event m' p u

  let changes = function
  | Const _ -> Never
  | Smut m ->
      let m' = emut (rsucc m.snode) in
      let rec p () = [ m.snode ]
      and u c = eupdate (sval m) m' c in
      end_of_step_add_dep ~stop_if_stopped:true m m';
      event m' p u

  let sample f e = function
  | Const v -> E.map (fun ev -> f ev v) e
  | Smut ms ->
      match e with
      | Never -> Never
      | Emut me ->
          let m' = emut (rsucc2 me.enode ms.snode) in
          let rec p () = [ me.enode; ms.snode ]
          and u c = match !(me.ev) with
          | None -> () (* ms updated *)
          | Some v -> eupdate (f v (sval ms)) m' c
          in
          E.add_dep me m'.enode;
          Node.add_dep ms.snode m'.enode;
          event m' p u

  let on ?(eq = ( = )) c i s = match c with
  | Const true -> s
  | Const false -> Const i
  | Smut mc ->
      match s with
      | Const v ->
          let m' = smut (rsucc mc.snode) eq in
          let rec p () = [ mc.snode ]
          and u c = if (sval mc) then supdate v m' c else () in
          Node.add_dep mc.snode m'.snode;
          signal ~i m' p u
      | Smut ms ->
          let m' = smut (rsucc2 mc.snode ms.snode) eq in
          let rec p () = [ mc.snode; ms.snode ]
          and u c = if (sval mc) then supdate (sval ms) m' c else () in
          Node.add_dep mc.snode m'.snode;
          Node.add_dep ms.snode m'.snode;
          signal ~i m' p u

  let when_ = on

  let dismiss ?(eq = ( = )) c i s = match c with
  | Never -> s
  | Emut mc ->
      match s with
      | Const v ->
          let m' = smut (rsucc mc.enode) eq in
          let rec p () = [ mc.enode ]
          and u c = match !(mc.ev) with
          | Some _ -> () | None -> supdate  v m' c
          in
          Node.add_dep mc.enode m'.snode;
          signal ~i m' p u
      | Smut ms ->
          let m' = smut (rsucc2 mc.enode ms.snode) eq in
          let rec p () = [ mc.enode; ms.snode ]
          and u c = match !(mc.ev) with
          | Some _ -> () | None -> supdate (sval ms) m' c
          in
          Node.add_dep mc.enode m'.snode;
          Node.add_dep ms.snode m'.snode;
          signal ~i m' p u

  (* Accumulating *)

  let accum ?(eq = ( = )) ef i = match ef with
  | Never -> Const i
  | Emut m ->
      let m' = smut (rsucc m.enode) eq in
      let rec p () = [ m.enode ]
      and u c = match !(m.ev) with
      | None -> ()                                             (* init only. *)
      | Some v -> supdate (v (sval m')) m' c
      in
      E.add_dep m m'.snode;
      signal ~i m' p u

  let fold ?(eq = ( = )) f i = function
  | Never -> Const i
  | Emut m ->
      let m' = smut (rsucc m.enode) eq in
      let rec p () = [ m.enode ]
      and u c = match !(m.ev) with
      | None -> ()                                           (* init only. *)
      | Some v -> supdate (f (sval m') v) m' c in
      E.add_dep m m'.snode;
      signal ~i m' p u

  (* Combining *)

  let merge ?(eq = ( = )) f a sl =
    let rmax' acc = function Const _ -> acc | Smut m -> rmax acc m.snode in
    let nodes acc = function Const _ -> acc | Smut m -> m.snode :: acc in
    let merger f a = function Const v -> f a v | Smut m -> f a (sval m) in
    let m' = smut (rsucc (List.fold_left rmax' Node.rmin sl)) eq in
    let rec p () = List.fold_left nodes [] sl
    and u c = supdate (List.fold_left (merger f) a sl) m' c in
    let dep = function Const _ -> ()| Smut m -> Node.add_dep m.snode m'.snode in
    List.iter dep sl;
    signal m' p u

  let switch ?(eq = ( = )) = function
  | Const s -> s
  | Smut mss ->
      let dummy = smut Node.min_rank eq in
      let src = ref (Smut dummy) in    (* dummy is overwritten by sig. init *)
      let m' = smut (rsucc mss.snode) eq in
      let rec p () = match !src with
      | Smut m -> [ mss.snode; m.snode] | Const _ -> [ mss.snode ]
      and u c =
        if (sval mss) == !src then (* ss didn't change, !src did *)
          begin match !src with
          | Smut m -> supdate (sval m) m' c
          | Const _ -> () (* init only. *)
          end
        else (* ss changed *)
          begin
            begin match !src with
            | Smut m -> Node.rem_dep m.snode m'.snode
            | Const _ -> ()
            end;
            let new_src = sval mss in
            src := new_src;
            match new_src with
            | Const v ->
                ignore (Node.update_rank m'.snode (rsucc mss.snode));
                supdate v m' c
            | Smut m ->
                Node.add_dep m.snode m'.snode;
                if c == Step.nil then
                  begin
                    ignore (Node.update_rank m'.snode
                              (rsucc2 m.snode mss.snode));
                    (* Check if the init src is in a step. *)
                    match Step.find_unfinished [m.snode] with
                    | c when c == Step.nil -> supdate (sval m) m' c
                    | c -> Step.add c m'.snode
                  end
                else
                if Node.update_rank m'.snode (rsucc2 m.snode mss.snode) then
                  begin
                    (* Rank increased because of m. Thus m may still
                       update and we need to reschedule. Next time we
                       will be in the other branch. *)
                    Step.allow_reschedule m'.snode;
                    Step.rebuild c;
                    Step.add c m'.snode
                  end
                else
                  (* No rank increase. m already updated if needed, no need
                     to reschedule and rebuild the queue. *)
                supdate (sval m) m' c
          end
      in
      Node.add_dep mss.snode m'.snode;
      (* We add a dep to dummy to avoid a long scan of Wa.rem when we remove
         the dep in the [u] function during static init. *)
      Node.add_dep dummy.snode m'.snode;
      signal m' p u

  let bind ?eq s sf = switch ?eq (map ~eq:( == ) sf s)

  let fix ?(eq = ( = )) i f =
    let update_delayed n p u nl =
      Node.bind n p u;
      match Step.find_unfinished nl with
      | c when c == Step.nil ->
          (* no pertinent occuring step, create a step for update. *)
          let c = Step.create () in
          n.update c;
          Step.execute c
      | c -> Step.add c n
    in
    let m = smut Node.delayed_rank eq in
    let s = signal ~i m (fun () -> []) (fun _ -> ()) in
    match f s with
    | Const v, r ->
        let rec p () = []
        and u c = supdate v m c in
        update_delayed m.snode p u (Node.deps m.snode);
        r
    | Smut m', r ->
        if m'.snode.rank = Node.delayed_rank then invalid_arg err_fix;
        let rec p () = [ (* avoid cyclic dep. *) ]
        and u c = supdate (sval m') m c in    (* N.B. c is the next step. *)
        Node.add_dep m'.snode m.snode;
        update_delayed m.snode p u (m'.snode :: Node.deps m.snode);
        r

  (* Lifting *)

  let l1 = map
  let l2 ?(eq = ( = )) f s s' = match s, s' with
  | Smut m0, Smut m1 ->
      let m' = smut (rsucc2 m0.snode m1.snode) eq in
      let rec p () = [ m0.snode; m1.snode ]
      and u c = supdate (f (sval m0) (sval m1)) m' c in
      Node.add_dep m0.snode m'.snode;
      Node.add_dep m1.snode m'.snode;
      signal m' p u
  | Const v, Const v' -> Const (f v v')
  | Const v, Smut m ->
      let m' = smut (rsucc m.snode) eq  in
      let rec p () = [ m.snode ]
      and u c = supdate (f v (sval m)) m' c in
      Node.add_dep m.snode m'.snode;
      signal m' p u
  | Smut m, Const v ->
      let m' = smut (rsucc m.snode) eq in
      let rec p () = [ m.snode ]
      and u c = supdate (f (sval m) v) m' c in
      Node.add_dep m.snode m'.snode;
      signal m' p u

  let l3 ?(eq = ( = )) f s0 s1 s2 = match s0, s1, s2 with
  | Smut m0, Smut m1, Smut m2 ->
      let r = rsucc (rmax (rmax m0.snode m1.snode) m2.snode) in
      let m' = smut r eq in
      let rec p () = [ m0.snode; m1.snode; m2.snode ]
      and u c = supdate (f (sval m0) (sval m1) (sval m2)) m' c in
      Node.add_dep m0.snode m'.snode;
      Node.add_dep m1.snode m'.snode;
      Node.add_dep m2.snode m'.snode;
      signal m' p u
  | Const v0, Const v1, Const v2 -> Const (f v0 v1 v2)
  | s0, s1, s2 -> app ~eq (l2 ~eq:( == ) f s0 s1) s2

  let l4 ?(eq = ( = )) f s0 s1 s2 s3 = match s0, s1, s2, s3 with
  | Smut m0, Smut m1, Smut m2, Smut m3 ->
      let r = rsucc (rmax (rmax m0.snode m1.snode) (rmax m2.snode m3.snode)) in
      let m' = smut r eq in
      let rec p () = [ m0.snode; m1.snode; m2.snode; m3.snode ]
      and u c = supdate (f (sval m0) (sval m1) (sval m2) (sval m3)) m' c in
      Node.add_dep m0.snode m'.snode;
      Node.add_dep m1.snode m'.snode;
      Node.add_dep m2.snode m'.snode;
      Node.add_dep m3.snode m'.snode;
      signal m' p u
  | Const v0, Const v1, Const v2, Const v3 -> Const (f v0 v1 v2 v3)
  | s0, s1, s2, s3 -> app ~eq (l3 ~eq:( == ) f s0 s1 s2) s3

  let l5 ?(eq = ( = )) f s0 s1 s2 s3 s4 = match s0, s1, s2, s3, s4 with
  | Smut m0, Smut m1, Smut m2, Smut m3, Smut m4 ->
      let m = rmax in
      let r = rsucc (m (m m0.snode m1.snode)
                       (m m2.snode (m m3.snode m4.snode)))
      in
      let m' = smut r eq in
      let rec p () = [ m0.snode; m1.snode; m2.snode; m3.snode; m4.snode ]
      and u c =
        let v = f (sval m0) (sval m1) (sval m2) (sval m3) (sval m4) in
        supdate v m' c
      in
      Node.add_dep m0.snode m'.snode;
      Node.add_dep m1.snode m'.snode;
      Node.add_dep m2.snode m'.snode;
      Node.add_dep m3.snode m'.snode;
      Node.add_dep m4.snode m'.snode;
      signal m' p u
  | Const v0, Const v1, Const v2, Const v3, Const v4 -> Const (f v0 v1 v2 v3 v4)
  | s0, s1, s2, s3, s4 -> app ~eq (l4 ~eq:( == ) f s0 s1 s2 s3) s4

  let l6 ?(eq = ( = )) f s0 s1 s2 s3 s4 s5 = match s0, s1, s2, s3, s4, s5 with
  | Smut m0, Smut m1, Smut m2, Smut m3, Smut m4, Smut m5 ->
      let m = rmax in
      let m = m (m m0.snode (m m1.snode m2.snode))
          (m m3.snode (m m4.snode m5.snode))
      in
      let m' = smut (rsucc m) eq in
      let rec p () =
        [ m0.snode; m1.snode; m2.snode; m3.snode; m4.snode; m5.snode ]
      and u c =
        let v = f (sval m0) (sval m1) (sval m2) (sval m3) (sval m4) (sval m5) in
        supdate v m' c
      in
      Node.add_dep m0.snode m'.snode;
      Node.add_dep m1.snode m'.snode;
      Node.add_dep m2.snode m'.snode;
      Node.add_dep m3.snode m'.snode;
      Node.add_dep m4.snode m'.snode;
      Node.add_dep m5.snode m'.snode;
      signal m' p u
  | Const v0, Const v1, Const v2, Const v3, Const v4, Const v5->
      Const (f v0 v1 v2 v3 v4 v5)
  | s0, s1, s2, s3, s4, s5 -> app ~eq (l5 ~eq:( == ) f s0 s1 s2 s3 s4) s5

  module Bool = struct
    let stdlib_not = not
    let one = Const true
    let zero = Const false
    let eq : bool -> bool -> bool = ( = )
    let not s = l1 ~eq stdlib_not s
    let ( && ) s s' = l2 ~eq ( && ) s s'
    let ( || ) s s' = l2 ~eq ( || ) s s'

    let edge s = changes s
    let edge_detect edge = function
    | Const _ -> Never
    | Smut m ->
        let m' = emut (rsucc m.snode) in
        let rec p () = [ m.snode ]
        and u c = if (sval m) = edge then eupdate () m' c in
        end_of_step_add_dep ~stop_if_stopped:true m m';
        event m' p u

    let rise s = edge_detect true s
    let fall s = edge_detect false s
    let flip b = function
    | Never -> Const b
    | Emut m ->
        let m' = smut (rsucc m.enode) ( = ) in
        let rec p () = [ m.enode ]
        and u c = match !(m.ev) with
        | None -> ()
        | Some _ -> supdate (stdlib_not (sval m')) m' c
        in
        E.add_dep m m'.snode;
        signal ~i:b m' p u
  end

  module Int = struct
    let zero = Const 0
    let one = Const 1
    let minus_one = Const (-1)
    let eq : int -> int -> bool = ( = )
    let ( ~- ) s = l1 ~eq ( ~- ) s
    let succ s = l1 ~eq succ s
    let pred s = l1 ~eq pred s
    let ( + ) s s' = l2 ~eq ( + ) s s'
    let ( - ) s s' = l2 ~eq ( - ) s s'
    let ( * ) s s' = l2 ~eq ( * ) s s'
    let ( mod ) s s' = l2 ~eq ( mod ) s s'
    let abs s = l1 ~eq abs s
    let max_int = const max_int
    let min_int = const min_int
    let ( land ) s s' = l2 ~eq ( land ) s s'
    let ( lor ) s s' = l2 ~eq ( lor ) s s'
    let ( lxor ) s s' = l2 ~eq ( lxor ) s s'
    let lnot s = l1 ~eq lnot s
    let ( lsl ) s s' = l2 ~eq ( lsl ) s s'
    let ( lsr ) s s' = l2 ~eq ( lsr ) s s'
    let ( asr ) s s' = l2 ~eq ( asr ) s s'
  end

  module Float = struct
    let zero = Const 0.
    let one = Const 1.
    let minus_one = Const (-1.)
    let eq : float -> float -> bool = ( = )
    let ( ~-. ) s = l1 ~eq ( ~-. ) s
    let ( +. ) s s' = l2 ~eq ( +. ) s s'
    let ( -. ) s s' = l2 ~eq ( -. ) s s'
    let ( *. ) s s' = l2 ~eq ( *. ) s s'
    let ( /. ) s s' = l2 ~eq ( /. ) s s'
    let ( ** ) s s' = l2 ~eq ( ** ) s s'
    let sqrt s = l1 ~eq sqrt s
    let exp s = l1 ~eq exp s
    let log s = l1 ~eq log s
    let log10 s = l1 ~eq log10 s
    let cos s = l1 ~eq cos s
    let sin s = l1 ~eq sin s
    let tan s = l1 ~eq tan s
    let acos s = l1 ~eq acos s
    let asin s = l1 ~eq asin s
    let atan s = l1 ~eq atan s
    let atan2 s s' = l2 ~eq atan2 s s'
    let cosh s = l1 ~eq cosh s
    let sinh s = l1 ~eq sinh s
    let tanh s = l1 ~eq tanh s
    let ceil s = l1 ~eq ceil s
    let floor s = l1 ~eq floor s
    let abs_float s = l1 ~eq abs_float s
    let mod_float s s' = l2 ~eq mod_float s s'
    let frexp s = l1 ~eq:( = ) frexp s
    let ldexp s s' = l2 ~eq ldexp s s'
    let modf s = l1 ~eq:( = ) modf s
    let float s = l1 ~eq float s
    let float_of_int s = l1 ~eq float_of_int s
    let truncate s = l1 ~eq:Int.eq truncate s
    let int_of_float s = l1 ~eq:Int.eq int_of_float s
    let infinity = const infinity
    let neg_infinity = const neg_infinity
    let nan = const nan
    let max_float = const max_float
    let min_float = const min_float
    let epsilon_float = const epsilon_float
    let classify_float s = l1 ~eq:( = ) classify_float s
  end

  module Pair = struct
    let pair ?eq s s' = l2 ?eq (fun x y -> x, y) s s'
    let fst ?eq s = l1 ?eq fst s
    let snd ?eq s = l1 ?eq snd s
  end

  module Option = struct
    let none = Const None
    let some s =
      let eq = match eq_fun s with
      | None -> None
      | Some eq ->
          let eq v v' = match v, v' with
          | Some v, Some v' -> eq v v'
          | _ -> assert false
          in
          Some eq
      in
      map ?eq (fun v -> Some v) s

    let value ?(eq = ( = )) ~default s = match s with
    | Const (Some v) -> Const v
    | Const None ->
        begin match default with
        | `Always d -> d
        | `Init d ->
            begin match d with
            | Const d -> Const d
            | Smut md ->
                match Step.find_unfinished [md.snode] with
                | c when c == Step.nil -> Const (sval md)
                | c ->
                    let m' = smut (rsucc md.snode) eq in
                    let rec p () = [ md.snode ]
                    and u c =
                      Node.rem_dep md.snode m'.snode;
                      supdate (sval md) m' c;
                      Node.stop m'.snode
                    in
                    Node.add_dep md.snode m'.snode;
                    signal m' p u
            end
        end
    | Smut m ->
        match default with
        | `Init (Const d) -> fmap ~eq (fun v -> v) d s
        | `Always (Const d) -> map ~eq (function None -> d | Some v -> v) s
        | `Init (Smut md) ->
            begin match Step.find_unfinished [md.snode] with
            | c when c == Step.nil ->
                let m' = smut (rsucc m.snode) eq in
                let rec p () = [ m.snode ]
                and u c = match sval m with
                | Some v -> supdate v m' c | None -> ()
                in
                Node.add_dep m.snode m'.snode;
                signal ~i:(sval md) m' p u
            | c ->
                let m' = smut (rsucc2 m.snode md.snode) eq in
                let rec p () = [ m.snode ] in (* subsequent updates *)
                let u c = match sval m with
                | Some v -> supdate v m' c | None -> ()
                in
                let rec p_first () = [ m.snode; md.snode ] in (* first update *)
                let u_first c =
                  Node.rem_dep md.snode m'.snode;
                  begin match sval m with
                  | None -> supdate (sval md) m' c
                  | Some v -> supdate v m' c
                  end;
                  Node.bind m'.snode p u
                in
                Node.add_dep m.snode m'.snode;
                Node.add_dep md.snode m'.snode;
                signal m' p_first u_first
            end
        | `Always (Smut md) ->
            let m' = smut (rsucc2 m.snode md.snode) eq in
            let rec p () = [ m.snode; md.snode ] in
            let u c = match sval m with
            | Some v -> supdate v m' c
            | None -> supdate (sval md) m' c
            in
            Node.add_dep m.snode m'.snode;
            Node.add_dep md.snode m'.snode;
            signal m' p u
  end

  module Compare = struct
    let eq = Bool.eq
    let ( = ) s s' = l2 ~eq ( = ) s s'
    let ( <> ) s s' = l2 ~eq ( <> ) s s'
    let ( < ) s s' = l2 ~eq ( < ) s s'
    let ( > ) s s' = l2 ~eq ( > ) s s'
    let ( <= ) s s' = l2 ~eq ( <= ) s s'
    let ( >= ) s s' = l2 ~eq ( >= ) s s'
    let compare s s' = l2 ~eq:Int.eq compare s s'
    let ( == ) s s' = l2 ~eq ( == ) s s'
    let ( != ) s s' = l2 ~eq ( != ) s s'
  end

  (* Combinator specialization *)

  module type EqType = sig
    type 'a t
    val equal : 'a t -> 'a t -> bool
  end

  module type S = sig
    type 'a v
    val create : 'a v -> 'a v signal * (?step:step -> 'a v -> unit)
    val equal : 'a v signal -> 'a v signal -> bool
    val hold : 'a v -> 'a v event -> 'a v signal
    val app : ('a -> 'b v) signal -> 'a signal -> 'b v signal
    val map : ('a -> 'b v) -> 'a signal -> 'b v signal
    val filter : ('a v -> bool) -> 'a v -> 'a v signal -> 'a v signal
    val fmap : ('a -> 'b v option) -> 'b v -> 'a signal -> 'b v signal
    val when_ : bool signal -> 'a v -> 'a v signal -> 'a v signal
    val dismiss : 'b event -> 'a v -> 'a v signal -> 'a v signal
    val accum : ('a v -> 'a v) event -> 'a v -> 'a v signal
    val fold : ('a v -> 'b -> 'a v) -> 'a v -> 'b event -> 'a v signal
    val merge : ('a v -> 'b -> 'a v) -> 'a v -> 'b signal list -> 'a v signal
    val switch : 'a v signal signal -> 'a v signal
    val bind : 'b signal -> ('b -> 'a v signal) -> 'a v signal
    val fix : 'a v -> ('a v signal -> 'a v signal * 'b) -> 'b
    val l1 : ('a -> 'b v) -> ('a signal -> 'b v signal)
    val l2 : ('a -> 'b -> 'c v) -> ('a signal -> 'b signal -> 'c v signal)
    val l3 : ('a -> 'b -> 'c -> 'd v) -> ('a signal -> 'b signal -> 'c signal
                                          -> 'd v signal)
    val l4 : ('a -> 'b -> 'c -> 'd -> 'e v) ->
      ('a signal -> 'b signal -> 'c signal -> 'd signal -> 'e v signal)
    val l5 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f v) ->
      ('a signal -> 'b signal -> 'c signal -> 'd signal -> 'e signal ->
       'f v signal)
    val l6 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g v) ->
      ('a signal -> 'b signal -> 'c signal -> 'd signal -> 'e signal ->
       'f signal -> 'g v signal)
  end

  module Make (Eq : EqType) = struct
    type 'a v = 'a Eq.t
    let eq = Eq.equal
    let create v = create ~eq v
    let equal s s' = equal ~eq s s'
    let hold v e = hold ~eq v e
    let app sf sv = app ~eq sf sv
    let map f s =  map ~eq f s
    let filter pred i = filter ~eq pred i
    let fmap fm i = fmap ~eq fm i
    let when_ c i s = when_ ~eq c i s
    let dismiss c s = dismiss ~eq c s
    let accum ef i = accum ~eq ef i
    let fold f i = fold ~eq f i
    let merge f a sl = merge ~eq f a sl
    let switch s = switch ~eq s
    let bind s sf = bind ~eq s sf
    let fix f = fix ~eq f
    let l1 = map
    let l2 f s s' = l2 ~eq f s s'
    let l3 f s0 s1 s2 = l3 ~eq f s0 s1 s2
    let l4 f s0 s1 s2 s3 = l4 ~eq f s0 s1 s2 s3
    let l5 f s0 s1 s2 s3 s4 = l5 ~eq f s0 s1 s2 s3 s4
    let l6 f s0 s1 s2 s3 s4 s5 = l6 ~eq f s0 s1 s2 s3 s4 s5
  end

  module Special = struct
    module Sb = Make (struct type 'a t = bool let equal = Bool.eq end)
    module Si = Make (struct type 'a t = int let equal = Int.eq end)
    module Sf = Make (struct type 'a t = float let equal = Float.eq end)
  end
end

(*---------------------------------------------------------------------------
   Copyright (c) 2009 The react programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
