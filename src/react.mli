(*---------------------------------------------------------------------------
   Copyright (c) 2009 The react programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Declarative events and signals.

    React is a module for functional reactive programming (frp).  It
    provides support to program with time varying values : declarative
    {{!E}events} and {{!S}signals}. React
    doesn't define any primitive event or signal, this lets the client
    choose the concrete timeline.

    Consult the {{!sem}semantics}, the {{!basics}basics} and
    {{!ex}examples}. Open the module to use it, this defines only two
    types and modules in your scope. *)

(** {1:interface Interface} *)

type 'a event
(** The type for events of type ['a]. *)

type 'a signal
(** The type for signals of type ['a]. *)

type step
(** The type for update steps. *)

(** Event combinators.

    Consult their {{!evsem}semantics.} *)
module E : sig
  (** {1:prim Primitive and basics} *)

  type 'a t = 'a event
  (** The type for events with occurrences of type ['a]. *)

  val never : 'a event
  (** A never occuring event. For all t, \[[never]\]{_t} [= None]. *)

  val create : unit -> 'a event * (?step:step -> 'a -> unit)
  (** [create ()] is a primitive event [e] and a [send] function. The
      function [send] is such that:
      {ul
      {- [send v] generates an occurrence [v] of [e] at the time it is called
         and triggers an {{!steps}update step}.}
      {- [send ~step v] generates an occurence [v] of [e] on the step [step]
         when [step] is {{!Step.execute}executed}.}
      {- [send ~step v] raises [Invalid_argument] if it was previously
         called with a step and this step has not executed yet or if
         the given [step] was already executed.}}

      {b Warning.} [send] must not be executed inside an update step. *)

  val retain : 'a event -> (unit -> unit) -> [ `R of (unit -> unit) ]
  (** [retain e c] keeps a reference to the closure [c] in [e] and
      returns the previously retained value. [c] will {e never} be
      invoked.

      {b Raises.} [Invalid_argument] on {!E.never}. *)

  val stop : ?strong:bool -> 'a event -> unit
  (** [stop e] stops [e] from occuring. It conceptually becomes
      {!never} and cannot be restarted. Allows to
      disable {{!sideeffects}effectful} events.

      The [strong] argument should only be used on platforms
      where weak arrays have a strong semantics (i.e. JavaScript).
      See {{!strongstop}details}.

      {b Note.} If executed in an {{!steps}update step}
      the event may still occur in the step. *)

  val equal : 'a event -> 'a event -> bool
  (** [equal e e'] is [true] iff [e] and [e'] are equal. If both events are
      different from {!never}, physical equality is used. *)

  val trace : ?iff:bool signal -> ('a -> unit) -> 'a event -> 'a event
  (** [trace iff tr e] is [e] except [tr] is invoked with e's
      occurence when [iff] is [true] (defaults to [S.const true]).
      For all t where \[[e]\]{_t} [= Some v] and \[[iff]\]{_t} =
      [true], [tr] is invoked with [v]. *)

 (** {1:transf Transforming and filtering} *)

  val once : 'a event -> 'a event
  (** [once e] is [e] with only its next occurence.
      {ul
      {- \[[once e]\]{_t} [= Some v] if \[[e]\]{_t} [= Some v] and
      \[[e]\]{_<t} [= None].}
      {- \[[once e]\]{_t} [= None] otherwise.}} *)

  val drop_once : 'a event -> 'a event
  (** [drop_once e] is [e] without its next occurrence.
      {ul
      {- \[[drop_once e]\]{_t} [= Some v] if \[[e]\]{_t} [= Some v] and
      \[[e]\]{_<t} [= Some _].}
      {- \[[drop_once e]\]{_t} [= None] otherwise.}} *)

  val app : ('a -> 'b) event -> 'a event -> 'b event
  (** [app ef e] occurs when both [ef] and [e] occur
      {{!simultaneity}simultaneously}.
      The value is [ef]'s occurence applied to [e]'s one.
      {ul
      {- \[[app ef e]\]{_t} [= Some v'] if \[[ef]\]{_t} [= Some f] and
      \[[e]\]{_t} [= Some v] and [f v = v'].}
      {- \[[app ef e]\]{_t} [= None] otherwise.}} *)

  val map : ('a -> 'b) -> 'a event -> 'b event
  (** [map f e] applies [f] to [e]'s occurrences.
      {ul
      {- \[[map f e]\]{_t} [= Some (f v)] if \[[e]\]{_t} [= Some v].}
      {- \[[map f e]\]{_t} [= None] otherwise.}} *)

  val stamp : 'b event -> 'a -> 'a event
  (** [stamp e v] is [map (fun _ -> v) e]. *)

  val filter : ('a -> bool) -> 'a event -> 'a event
  (** [filter p e] are [e]'s occurrences that satisfy [p].
      {ul
      {- \[[filter p e]\]{_t} [= Some v] if \[[e]\]{_t} [= Some v] and
      [p v = true]}
      {- \[[filter p e]\]{_t} [= None] otherwise.}} *)

  val fmap : ('a -> 'b option) -> 'a event -> 'b event
  (** [fmap fm e] are [e]'s occurrences filtered and mapped by [fm].
      {ul
      {- \[[fmap fm e]\]{_t} [= Some v] if [fm] \[[e]\]{_t} [= Some v]}
      {- \[[fmap fm e]\]{_t} [= None] otherwise.}} *)

  val diff : ('a -> 'a -> 'b) -> 'a event -> 'b event
  (** [diff f e] occurs whenever [e] occurs except on the next occurence.
      Occurences are [f v v'] where [v] is [e]'s current
      occurrence and [v'] the previous one.
      {ul
      {- \[[diff f e]\]{_t} [= Some r] if \[[e]\]{_t} [= Some v],
      \[[e]\]{_<t} [= Some v'] and [f v v' = r].}
      {- \[[diff f e]\]{_t} [= None] otherwise.}} *)

  val changes : ?eq:('a -> 'a -> bool) -> 'a event -> 'a event
  (** [changes eq e] is [e]'s occurrences with occurences equal to
      the previous one dropped. Equality is tested with [eq] (defaults to
      structural equality).
      {ul
      {- \[[changes eq e]\]{_t} [= Some v] if \[[e]\]{_t} [= Some v]
      and either \[[e]\]{_<t} [= None] or \[[e]\]{_<t} [= Some v'] and
      [eq v v' = false].}
      {- \[[changes eq e]\]{_t} [= None] otherwise.}} *)

  val on : bool signal -> 'a event -> 'a event
  (** [on c e] is the occurrences of [e] when [c] is [true].
      {ul
      {- \[[on c e]\]{_t} [= Some v]
         if \[[c]\]{_t} [= true] and \[[e]\]{_t} [= Some v].}
      {- \[[on c e]\]{_t} [= None] otherwise.}} *)

  val when_ : bool signal -> 'a event -> 'a event
  (** @deprecated Use {!on}. *)

  val dismiss : 'b event -> 'a event -> 'a event
  (** [dismiss c e] is the occurences of [e] except the ones when [c] occurs.
      {ul
      {- \[[dimiss c e]\]{_t} [= Some v]
         if \[[c]\]{_t} [= None] and \[[e]\]{_t} [= Some v].}
      {- \[[dimiss c e]\]{_t} [= None] otherwise.}} *)

  val until : 'a event -> 'b event -> 'b event
  (** [until c e] is [e]'s occurences until [c] occurs.
      {ul
      {- \[[until c e]\]{_t} [= Some v] if \[[e]\]{_t} [= Some v] and
         \[[c]\]{_<=t} [= None]}
      {- \[[until c e]\]{_t} [= None] otherwise.}} *)

  (** {1:accum Accumulating} *)

  val accum : ('a -> 'a) event -> 'a -> 'a event
  (** [accum ef i] accumulates a value, starting with [i], using [e]'s
      functional occurrences.
      {ul
      {- \[[accum ef i]\]{_t} [= Some (f i)] if \[[ef]\]{_t} [= Some f]
      and \[[ef]\]{_<t} [= None].
      }
      {- \[[accum ef i]\]{_t} [= Some (f acc)] if \[[ef]\]{_t} [= Some f]
      and \[[accum ef i]\]{_<t} [= Some acc].}
      {- \[[accum ef i]\] [= None] otherwise.}} *)

  val fold : ('a -> 'b -> 'a) -> 'a -> 'b event -> 'a event
  (** [fold f i e] accumulates [e]'s occurrences with [f] starting with [i].
      {ul
      {- \[[fold f i e]\]{_t} [= Some (f i v)] if
      \[[e]\]{_t} [= Some v] and \[[e]\]{_<t} [= None].}
      {- \[[fold f i e]\]{_t} [= Some (f acc v)] if
      \[[e]\]{_t} [= Some v] and \[[fold f i e]\]{_<t} [= Some acc].}
      {- \[[fold f i e]\]{_t} [= None] otherwise.}} *)

  (** {1:combine Combining} *)

  val select : 'a event list -> 'a event
  (** [select el] is the occurrences of every event in [el].
      If more than one event occurs {{!simultaneity}simultaneously}
      the leftmost is taken and the others are lost.
      {ul
      {- \[[select el]\]{_ t} [=] \[[List.find (fun e -> ]\[[e]\]{_t}
      [<> None) el]\]{_t}.}
      {- \[[select el]\]{_ t} [= None] otherwise.}}  *)

  val merge : ('a -> 'b -> 'a) -> 'a -> 'b event list -> 'a event
      (** [merge f a el] merges the {{!simultaneity}simultaneous}
    occurrences of every event in [el] using [f] and the accumulator [a].

    \[[merge f a el]\]{_ t}
    [= List.fold_left f a (List.filter (fun o -> o <> None)
           (List.map] \[\]{_t}[ el))]. *)

  val switch : 'a event -> 'a event event -> 'a event
  (** [switch e ee] is [e]'s occurrences until there is an
      occurrence [e'] on [ee], the occurrences of [e'] are then used
      until there is a new occurrence on [ee], etc..
      {ul
      {- \[[switch e ee]\]{_ t} [=] \[[e]\]{_t} if \[[ee]\]{_<=t} [= None].}
      {- \[[switch e ee]\]{_ t} [=] \[[e']\]{_t} if \[[ee]\]{_<=t}
    [= Some e'].}} *)

  val fix : ('a event -> 'a event * 'b) -> 'b
  (** [fix ef] allows to refer to the value an event had an
      infinitesimal amount of time before.

      In [fix ef], [ef] is called with an event [e] that represents
      the event returned by [ef] delayed by an infinitesimal amount of
      time.  If [e', r = ef e] then [r] is returned by [fix] and [e]
      is such that :
      {ul
      {- \[[e]\]{_ t} [=] [None] if t = 0 }
      {- \[[e]\]{_ t} [=] \[[e']\]{_t-dt} otherwise}}

      {b Raises.} [Invalid_argument] if [e'] is directly a delayed event (i.e.
      an event given to a fixing function). *)

  (** {1 Lifting}

      Lifting combinators. For a given [n] the semantics is:
      {ul
      {- \[[ln f e1 ... en]\]{_t} [= Some (f v1 ... vn)] if for all
         i : \[[ei]\]{_t} [= Some vi].}
      {- \[[ln f e1 ... en]\]{_t} [= None] otherwise.}} *)

  val l1 : ('a -> 'b) -> 'a event -> 'b event
  val l2 : ('a -> 'b -> 'c) -> 'a event -> 'b event -> 'c event
  val l3 : ('a -> 'b -> 'c -> 'd) -> 'a event -> 'b event -> 'c event ->
    'd event
  val l4 : ('a -> 'b -> 'c -> 'd -> 'e) -> 'a event -> 'b event -> 'c event ->
    'd event -> 'e event
  val l5 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'a event -> 'b event ->
    'c event -> 'd event -> 'e event -> 'f event
  val l6 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) -> 'a event -> 'b event ->
    'c event -> 'd event -> 'e event -> 'f event -> 'g event

  (** {1:stdlib_support Stdlib support} *)

  (** Events with option occurences. *)
  module Option : sig
    val some : 'a event -> 'a option event
    (** [some e] is [map (fun v -> Some v) e]. *)

    val value : ?default:'a signal -> 'a option event -> 'a event
    (** [value default e] either silences [None] occurences if [default] is
        unspecified or replaces them by the value of [default] at the occurence
        time.
        {ul
        {- \[[value ~default e]\]{_t}[ = v] if \[[e]\]{_t} [= Some (Some v)].}
        {- \[[value ?default:None e]\]{_t}[ = None] if \[[e]\]{_t} = [None].}
        {- \[[value ?default:(Some s) e]\]{_t}[ = v]
           if \[[e]\]{_t} = [None] and \[[s]\]{_t} [= v].}} *)
  end
end

(** Signal combinators.

    Consult their {{!sigsem}semantics.}  *)
module S : sig
  (** {1:prim Primitive and basics} *)

  type 'a t = 'a signal
  (** The type for signals of type ['a]. *)

  val const : 'a -> 'a signal
  (** [const v] is always [v], \[[const v]\]{_t} [= v]. *)

  val create : ?eq:('a -> 'a -> bool) -> 'a ->
    'a signal * (?step:step -> 'a -> unit)
  (** [create i] is a primitive signal [s] set to [i] and a
      [set] function. The function [set] is such that:
      {ul
      {- [set v] sets the signal's value to [v] at the time it is called and
         triggers an {{!steps}update step}.}
      {- [set ~step v] sets the signal's value to [v] at the time it is
         called and updates it dependencies when [step] is
         {{!Step.execute}executed}}
      {- [set ~step v] raises [Invalid_argument] if it was previously
         called with a step and this step has not executed yet or if
         the given [step] was already executed.}}
      {b Warning.} [set] must not be executed inside an update step. *)

  val value : 'a signal -> 'a
  (** [value s] is [s]'s current value.

      {b Warning.} If executed in an {{!steps}update
      step} may return a non up-to-date value or raise [Failure] if
      the signal is not yet initialized. *)

  val retain : 'a signal -> (unit -> unit) -> [ `R of (unit -> unit) ]
  (** [retain s c] keeps a reference to the closure [c] in [s] and
      returns the previously retained value. [c] will {e never} be
      invoked.

      {b Raises.} [Invalid_argument] on constant signals. *)

  (**/**)
  val eq_fun : 'a signal -> ('a -> 'a -> bool) option
  (**/**)

  val stop : ?strong:bool -> 'a signal -> unit
  (** [stop s], stops updating [s]. It conceptually becomes {!const}
      with the signal's last value and cannot be restarted. Allows to
      disable {{!sideeffects}effectful} signals.

      The [strong] argument should only be used on platforms
      where weak arrays have a strong semantics (i.e. JavaScript).
      See {{!strongstop}details}.

      {b Note.} If executed in an update step the signal may
      still update in the step. *)

  val equal : ?eq:('a -> 'a -> bool) -> 'a signal -> 'a signal -> bool
  (** [equal s s'] is [true] iff [s] and [s'] are equal. If both
      signals are {!const}ant [eq] is used between their value
      (defauts to structural equality). If both signals are not
      {!const}ant, physical equality is used.*)

  val trace : ?iff:bool t -> ('a -> unit) -> 'a signal -> 'a signal
  (** [trace iff tr s] is [s] except [tr] is invoked with [s]'s
      current value and on [s] changes when [iff] is [true] (defaults
      to [S.const true]). For all t where \[[s]\]{_t} [= v] and (t = 0
      or (\[[s]\]{_t-dt}[= v'] and [eq v v' = false])) and
      \[[iff]\]{_t} = [true], [tr] is invoked with [v]. *)

  (** {1 From events} *)

  val hold : ?eq:('a -> 'a -> bool) -> 'a -> 'a event -> 'a signal
  (** [hold i e] has the value of [e]'s last occurrence or [i] if there
      wasn't any.
      {ul
      {- \[[hold i e]\]{_t} [= i] if \[[e]\]{_<=t} [= None]}
      {- \[[hold i e]\]{_t} [= v] if \[[e]\]{_<=t} [= Some v]}} *)

 (** {1:tr Transforming and filtering} *)

  val app : ?eq:('b -> 'b -> bool) -> ('a -> 'b) signal -> 'a signal ->
    'b signal
  (** [app sf s] holds the value of [sf] applied
      to the value of [s], \[[app sf s]\]{_t}
      [=] \[[sf]\]{_t} \[[s]\]{_t}. *)

  val map : ?eq:('b -> 'b -> bool) -> ('a -> 'b) -> 'a signal -> 'b signal
  (** [map f s] is [s] transformed by [f], \[[map f s]\]{_t} = [f] \[[s]\]{_t}.
   *)

  val filter : ?eq:('a -> 'a -> bool) -> ('a -> bool) -> 'a -> 'a signal ->
    'a signal
  (** [filter f i s] is [s]'s values that satisfy [p]. If a value does not
      satisfy [p] it holds the last value that was satisfied or [i] if
      there is none.
      {ul
      {- \[[filter p s]\]{_t} [=] \[[s]\]{_t} if [p] \[[s]\]{_t}[ = true].}
      {- \[[filter p s]\]{_t} [=] \[[s]\]{_t'} if [p] \[[s]\]{_t}[ = false]
          and t' is the greatest t' < t with [p] \[[s]\]{_t'}[ = true].}
      {- \[[filter p e]\]{_t} [= i] otherwise.}} *)

  val fmap : ?eq:('b -> 'b -> bool) -> ('a -> 'b option) -> 'b -> 'a signal ->
    'b signal
  (** [fmap fm i s] is [s] filtered and mapped by [fm].
      {ul
      {- \[[fmap fm i s]\]{_t} [=] v if [fm] \[[s]\]{_t}[ = Some v].}
      {- \[[fmap fm i s]\]{_t} [=] \[[fmap fm i s]\]{_t'} if [fm]
         \[[s]\]{_t} [= None] and t' is the greatest t' < t with [fm]
         \[[s]\]{_t'} [<> None].}
      {- \[[fmap fm i s]\]{_t} [= i] otherwise.}} *)

  val diff : ('a -> 'a -> 'b) -> 'a signal -> 'b event
  (** [diff f s] is an event with occurrences whenever [s] changes from
      [v'] to [v] and [eq v v'] is [false] ([eq] is the signal's equality
      function).  The value of the occurrence is [f v v'].
      {ul
      {- \[[diff f s]\]{_t} [= Some d]
      if \[[s]\]{_t} [= v] and \[[s]\]{_t-dt} [= v'] and [eq v v' = false]
      and [f v v' = d].}
      {- \[[diff f s]\]{_t} [= None] otherwise.}} *)

  val changes : 'a signal -> 'a event
  (** [changes s] is [diff (fun v _ -> v) s]. *)

  val sample : ('b -> 'a -> 'c) -> 'b event -> 'a signal -> 'c event
  (** [sample f e s] samples [s] at [e]'s occurrences.
      {ul
      {- \[[sample f e s]\]{_t} [= Some (f ev sv)] if \[[e]\]{_t} [= Some ev]
         and  \[[s]\]{_t} [= sv].}
      {- \[[sample e s]\]{_t} [= None] otherwise.}} *)

  val on : ?eq:('a -> 'a -> bool) -> bool signal -> 'a -> 'a signal ->
    'a signal
  (** [on c i s] is the signal [s] whenever [c] is [true].
      When [c] is [false] it holds the last value [s] had when
      [c] was the last time [true] or [i] if it never was.
      {ul
      {- \[[on c i s]\]{_t} [=] \[[s]\]{_t} if \[[c]\]{_t} [= true]}
      {- \[[on c i s]\]{_t} [=] \[[s]\]{_t'} if \[[c]\]{_t} [= false]
         where t' is the greatest t' < t with \[[c]\]{_t'} [= true].}
      {- \[[on c i s]\]{_t} [=] [i] otherwise.}} *)

  val when_ : ?eq:('a -> 'a -> bool) -> bool signal -> 'a -> 'a signal ->
    'a signal
  (** @deprecated Use {!on}. *)

  val dismiss : ?eq:('a -> 'a -> bool) -> 'b event -> 'a -> 'a signal ->
    'a signal
  (** [dismiss c i s] is the signal [s] except changes when [c] occurs
      are ignored. If [c] occurs initially [i] is used.
      {ul
      {- \[[dismiss c i s]\]{_t} [=] \[[s]\]{_t'}
         where t' is the greatest t' <= t with \[[c]\]{_t'} [= None] and
         \[[s]\]{_t'-dt} [<>] \[[s]\]{_t'}}
       {- \[[dismiss_ c i s]\]{_0} [=] [v] where [v = i] if
    \[[c]\]{_0} [= Some _] and [v =] \[[s]\]{_0} otherwise.}} *)

  (** {1:acc Accumulating} *)

  val accum : ?eq:('a -> 'a -> bool) -> ('a -> 'a) event -> 'a -> 'a signal
  (** [accum e i] is [S.hold i (]{!E.val-accum}[ e i)]. *)

  val fold : ?eq:('a -> 'a -> bool) -> ('a -> 'b -> 'a) -> 'a -> 'b event ->
    'a signal
  (** [fold f i e] is [S.hold i (]{!E.fold}[ f i e)]. *)

  (** {1:combine Combining} *)

  val merge : ?eq:('a -> 'a -> bool) -> ('a -> 'b -> 'a) -> 'a ->
    'b signal list -> 'a signal
      (** [merge f a sl] merges the value of every signal in [sl]
    using [f] and the accumulator [a].

    \[[merge f a sl]\]{_ t}
    [= List.fold_left f a (List.map] \[\]{_t}[ sl)]. *)

  val switch : ?eq:('a -> 'a -> bool) -> 'a signal signal -> 'a signal
  (** [switch ss] is the inner signal of [ss].
      {ul
      {- \[[switch ss]\]{_ t} [=] \[\[[ss]\]{_t}\]{_t}.}} *)

  val bind : ?eq:('b -> 'b -> bool) -> 'a signal -> ('a -> 'b signal) ->
    'b signal
  (** [bind s sf] is [switch (map ~eq:( == ) sf s)]. *)

  val fix : ?eq:('a -> 'a -> bool) -> 'a -> ('a signal -> 'a signal * 'b) -> 'b
  (** [fix i sf] allow to refer to the value a signal had an
      infinitesimal amount of time before.

      In [fix sf], [sf] is called with a signal [s] that represents
      the signal returned by [sf] delayed by an infinitesimal amount
      time. If [s', r = sf s] then [r] is returned by [fix] and [s]
      is such that :
      {ul
      {- \[[s]\]{_ t} [=] [i] for t = 0. }
      {- \[[s]\]{_ t} [=] \[[s']\]{_t-dt} otherwise.}}

      [eq] is the equality used by [s].

      {b Raises.} [Invalid_argument] if [s'] is directly a delayed signal (i.e.
      a signal given to a fixing function).

      {b Note.} Regarding values depending on the result [r] of
      [s', r = sf s] the following two cases need to be distinguished :
      {ul
      {- After [sf s] is applied, [s'] does not depend on
         a value that is in a step and [s] has no dependents in a step (e.g
         in the simple case where [fix] is applied outside a step).

         In that case if the initial value of [s'] differs from [i],
         [s] and its dependents need to be updated and a special
         update step will be triggered for this. Values
         depending on the result [r] will be created only after this
         special update step has finished (e.g. they won't see
         the [i] of [s] if [r = s]).}
      {- Otherwise, values depending on [r] will be created in the same
         step as [s] and [s'] (e.g. they will see the [i] of [s] if [r = s]).}}
   *)

 (** {1:lifting Lifting}

     Lifting combinators. For a given [n] the semantics is :

     \[[ln f a1] ... [an]\]{_t} = f \[[a1]\]{_t} ... \[[an]\]{_t} *)

  val l1 : ?eq:('b -> 'b -> bool) -> ('a -> 'b) -> ('a signal -> 'b signal)
  val l2 : ?eq:('c -> 'c -> bool) ->
  ('a -> 'b -> 'c) -> ('a signal -> 'b signal -> 'c signal)
  val l3 : ?eq:('d -> 'd -> bool) ->
  ('a -> 'b -> 'c -> 'd) -> ('a signal -> 'b signal -> 'c signal -> 'd signal)
  val l4 : ?eq:('e -> 'e -> bool) ->
  ('a -> 'b -> 'c -> 'd -> 'e) ->
    ('a signal -> 'b signal -> 'c signal -> 'd signal -> 'e signal)
  val l5 : ?eq:('f -> 'f -> bool) ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f) ->
    ('a signal -> 'b signal -> 'c signal -> 'd signal -> 'e signal ->
      'f signal)
  val l6 : ?eq:('g -> 'g -> bool) ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) ->
    ('a signal -> 'b signal -> 'c signal -> 'd signal -> 'e signal ->
      'f signal -> 'g signal)

  (** The following modules lift some of [Stdlib] functions and
      operators. *)

  module Bool : sig
    val zero : bool signal
    val one : bool signal
    val not : bool signal -> bool signal
    val ( && ) : bool signal -> bool signal -> bool signal
    val ( || ) : bool signal -> bool signal -> bool signal

    val edge : bool signal -> bool event
    (** [edge s] is [changes s]. *)

    val rise : bool signal -> unit event
    (** [rise s] is [E.fmap (fun b -> if b then Some () else None) (edge s)].*)

    val fall : bool signal -> unit event
    (** [fall s] is [E.fmap (fun b -> if b then None else Some ()) (edge s)].*)

    val flip : bool -> 'a event -> bool signal
    (** [flip b e] is a signal whose boolean value flips each time
        [e] occurs. [b] is the initial signal value.
        {ul
        {- \[[flip b e]\]{_0} [= not b] if \[[e]\]{_0} [= Some _]}
        {- \[[flip b e]\]{_t} [= b] if \[[e]\]{_<=t} [= None]}
        {- \[[flip b e]\]{_t} [=] [not] \[[flip b e]\]{_t-dt}
           if \[[e]\]{_t} [= Some _]}}
*)
  end

  module Int : sig
    val zero : int signal
    val one : int signal
    val minus_one : int signal
    val ( ~- ) : int signal -> int signal
    val succ : int signal -> int signal
    val pred : int signal -> int signal
    val ( + ) : int signal -> int signal -> int signal
    val ( - ) : int signal -> int signal -> int signal
    val ( * ) : int signal -> int signal -> int signal
    val ( mod ) : int signal -> int signal -> int signal
    val abs : int signal -> int signal
    val max_int : int signal
    val min_int : int signal
    val ( land ) : int signal -> int signal -> int signal
    val ( lor ) : int signal -> int signal -> int signal
    val ( lxor ) : int signal -> int signal -> int signal
    val lnot : int signal -> int signal
    val ( lsl ) : int signal -> int signal -> int signal
    val ( lsr ) : int signal -> int signal -> int signal
    val ( asr ) : int signal -> int signal -> int signal
  end

  module Float : sig
    val zero : float signal
    val one : float signal
    val minus_one : float signal
    val ( ~-. ) : float signal -> float signal
    val ( +. ) : float signal -> float signal -> float signal
    val ( -. ) : float signal -> float signal -> float signal
    val ( *. ) : float signal -> float signal -> float signal
    val ( /. ) : float signal -> float signal -> float signal
    val ( ** ) : float signal -> float signal -> float signal
    val sqrt : float signal -> float signal
    val exp : float signal -> float signal
    val log : float signal -> float signal
    val log10 : float signal -> float signal
    val cos : float signal -> float signal
    val sin : float signal -> float signal
    val tan : float signal -> float signal
    val acos : float signal -> float signal
    val asin : float signal -> float signal
    val atan : float signal -> float signal
    val atan2 : float signal -> float signal -> float signal
    val cosh : float signal -> float signal
    val sinh : float signal -> float signal
    val tanh : float signal -> float signal
    val ceil : float signal -> float signal
    val floor : float signal -> float signal
    val abs_float : float signal -> float signal
    val mod_float : float signal -> float signal -> float signal
    val frexp : float signal -> (float * int) signal
    val ldexp : float signal -> int signal -> float signal
    val modf : float signal -> (float * float) signal
    val float : int signal -> float signal
    val float_of_int : int signal -> float signal
    val truncate : float signal -> int signal
    val int_of_float : float signal -> int signal
    val infinity : float signal
    val neg_infinity : float signal
    val nan : float signal
    val max_float : float signal
    val min_float : float signal
    val epsilon_float : float signal
    val classify_float : float signal -> fpclass signal
  end

  module Pair : sig
    val pair : ?eq:(('a * 'b) -> ('a * 'b) -> bool)->
      'a signal -> 'b signal -> ('a * 'b) signal
    val fst : ?eq:('a -> 'a -> bool) -> ('a * 'b) signal -> 'a signal
    val snd : ?eq:('a -> 'a -> bool) -> ('b * 'a) signal -> 'a signal
  end

  module Option : sig
    val none : 'a option signal
    (** [none] is [S.const None]. *)

    val some : 'a signal -> 'a option signal
    (** [some s] is [S.map ~eq (fun v -> Some v) None], where [eq] uses
        [s]'s equality function to test the [Some v]'s equalities. *)

    val value : ?eq:('a -> 'a -> bool) ->
      default:[`Init of 'a signal | `Always of 'a signal ] ->
      'a option signal -> 'a signal
    (** [value default s] is [s] with only its [Some v] values.
        Whenever [s] is [None], if [default] is [`Always dv] then
        the current value of [dv] is used instead. If [default]
        is [`Init dv] the current value of [dv] is only used
        if there's no value at creation time, otherwise the last
        [Some v] value of [s] is used.
        {ul
        {- \[[value ~default s]\]{_t} [= v] if \[[s]\]{_t} [= Some v]}
        {- \[[value ~default:(`Always d) s]\]{_t} [=] \[[d]\]{_t}
          if \[[s]\]{_t} [= None]}
        {- \[[value ~default:(`Init d) s]\]{_0} [=] \[[d]\]{_0}
          if \[[s]\]{_0} [= None]}
        {- \[[value ~default:(`Init d) s]\]{_t} [=]
           \[[value ~default:(`Init d) s]\]{_t'}
          if \[[s]\]{_t} [= None] and t' is the greatest t' < t
          with \[[s]\]{_t'} [<> None] or 0 if there is no such [t'].}} *)
  end

  module Compare : sig
    val ( = ) : 'a signal -> 'a signal -> bool signal
    val ( <> ) : 'a signal -> 'a signal -> bool signal
    val ( < ) : 'a signal -> 'a signal -> bool signal
    val ( > ) : 'a signal -> 'a signal -> bool signal
    val ( <= ) : 'a signal -> 'a signal -> bool signal
    val ( >= ) : 'a signal -> 'a signal -> bool signal
    val compare : 'a signal -> 'a signal -> int signal
    val ( == ) : 'a signal -> 'a signal -> bool signal
    val ( != ) : 'a signal -> 'a signal -> bool signal
  end

  (** {1:special Combinator specialization}

      Given an equality function [equal] and a type [t], the functor
      {!Make} automatically applies the [eq] parameter of the combinators.
      The outcome is combinators whose {e results} are signals with
      values in [t].

      Basic types are already specialized in the module {!Special}, open
      this module to use them.  *)

  (** Input signature of {!Make} *)
  module type EqType = sig
    type 'a t
    val equal : 'a t -> 'a t -> bool
  end

  (** Output signature of {!Make} *)
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
    val l3 : ('a -> 'b -> 'c -> 'd v) -> ('a signal -> 'b signal ->
      'c signal -> 'd v signal)
    val l4 : ('a -> 'b -> 'c -> 'd -> 'e v) ->
      ('a signal -> 'b signal -> 'c signal -> 'd signal -> 'e v signal)
    val l5 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f v) ->
  ('a signal -> 'b signal -> 'c signal -> 'd signal -> 'e signal ->
    'f v signal)
    val l6 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g v) ->
  ('a signal -> 'b signal -> 'c signal -> 'd signal -> 'e signal ->
    'f signal -> 'g v signal)
  end

  (** Functor specializing the combinators for the given signal value type *)
  module Make (Eq : EqType) : S with type 'a v = 'a Eq.t


  (** Specialization for booleans, integers and floats.

      Open this module to use it. *)
  module Special : sig

     (** Specialization for booleans. *)
     module Sb : S with type 'a v = bool

     (** Specialization for integers. *)
     module Si : S with type 'a v = int

     (** Specialization for floats. *)
     module Sf : S with type 'a v = float
  end
end

(** Update steps.

    Update functions returned by {!S.create} and {!E.create}
    implicitely create and execute update steps when used without
    specifying their [step] argument.

    Using explicit {!step} values with these functions gives more control on
    the time when the update step is perfomed and allows to perform
    simultaneous {{!primitives}primitive} signal updates and event
    occurences. See also the documentation about {{!steps}update steps} and
    {{!simultaneity}simultaneous events}. *)
module Step : sig

  (** {1 Steps} *)

  type t = step
  (** The type for update steps. *)

  val create : unit -> step
  (** [create ()] is a new update step. *)

  val execute : step -> unit
  (** [execute step] executes the update step.

      @raise Invalid_argument if [step] was already executed. *)
end

(** {1:sem Semantics}

    The following notations are used to give precise meaning to the
    combinators. It is important to note that in these semantic
    descriptions the origin of time t = 0 is {e always} fixed at
    the time at which the combinator creates the event or the signal and
    the semantics of the dependents is evaluated relative to this timeline.

    We use dt to denote an infinitesimal amount of time.
    {2:evsem Events}

    An event is a value with discrete occurrences over time.

    The semantic function \[\] [: 'a event -> time -> 'a option] gives
    meaning to an event [e] by mapping it to a function of time
    \[[e]\] returning [Some v] whenever the event occurs with value
    [v] and [None] otherwise. We write \[[e]\]{_t} the evaluation of
    this {e semantic} function at time t.

    As a shortcut notation we also define \[\]{_<t} [: 'a event -> 'a option]
    (resp. \[\]{_<=t}) to denote the last occurrence, if any, of an
    event before (resp. before or at) [t]. More precisely :
    {ul
    {- \[[e]\]{_<t} [=] \[[e]\]{_t'} with t' the greatest t' < t
      (resp. [<=]) such that
       \[[e]\]{_t'} [<> None].}
    {- \[[e]\]{_<t} [= None] if there is no such t'.}}

    {2:sigsem Signals}

    A signal is a value that varies continuously over time. In
    contrast to {{!evsem}events} which occur at specific point
    in time, a signal has a value at every point in time.

    The semantic function \[\] [: 'a signal -> time -> 'a] gives
    meaning to a signal [s] by mapping it to a function of time
    \[[s]\] that returns its value at a given time. We write \[[s]\]{_t}
    the evaluation of this {e semantic} function at time t.
    {3:sigeq Equality}

    Most signal combinators have an optional [eq] parameter that
    defaults to structural equality. [eq] specifies the equality
    function used to detect changes in the value of the resulting
    signal. This function is needed for the efficient update of
    signals and to deal correctly with signals that perform
    {{!sideeffects}side effects}.

    Given an equality function on a type the combinators can be automatically
    {{!S.special}specialized} via a functor.

    {3:sigcont Continuity}

    Ultimately signal updates depend on
    {{!primitives}primitives} updates. Thus a signal can
    only approximate a real continuous signal. The accuracy of the
    approximation depends on the variation rate of the real signal and
    the primitive's update frequency.

    {1:basics Basics}

    {2:primitives Primitive events and signals}

    React doesn't define primitive events and signals, they must be
    created and updated by the client.

    Primitive events are created with {!E.create}. This function
    returns a new event and an update function that generates an
    occurrence for the event at the time it is called. The following
    code creates a primitive integer event [x] and generates three
    occurrences with value [1], [2], [3]. Those occurrences are printed
    on stdout by the effectful event [pr_x].
{[open React;;

let x, send_x = E.create ()
let pr_x = E.map print_int x
let () = List.iter send_x [1; 2; 3]]}
    Primitive signals are created with {!S.create}. This function
    returns a new signal and an update function that sets the signal's value
    at the time it is called. The following code creates an
    integer signal [x] initially set to [1] and updates it three time with
    values [2], [2], [3]. The signal's values are printed on stdout by the
    effectful signal [pr_x]. Note that only updates that change
    the signal's value are printed, hence the program prints [123], not [1223].
    See the discussion on
    {{!sideeffects}side effects} for more details.

{[open React;;

let x, set_x = S.create 1
let pr_x = S.map print_int x
let () = List.iter set_x [2; 2; 3]]}
    The {{!clock}clock} example shows how a realtime time
    flow can be defined.

   {2:steps Update steps}

   The {!E.create} and {!S.create} functions return update functions
   used to generate primitive event occurences and set the value of
   primitive signals. Upon invocation as in the preceding section
   these functions immediatly create and invoke an update step.
   The {e update step} automatically updates events and signals that
   transitively depend on the updated primitive. The dependents of a
   signal are updated iff the signal's value changed according to its
   {{!sigeq}equality function}.

   The update functions have an optional [step] argument. If they are
   given a concrete [step] value created with {!Step.create}, then it
   updates the event or signal but doesn't update its dependencies. It
   will only do so whenever [step] is executed with
   {!Step.execute}. This allows to make primitive event occurences and
   signal changes simultaneous. See next section for an example.

    {2:simultaneity Simultaneous events}

    {{!steps}Update steps} are made under a
    {{:http://dx.doi.org/10.1016/0167-6423(92)90005-V}synchrony hypothesis} :
    the update step takes no time, it is instantenous. Two event occurrences
    are {e simultaneous} if they occur in the same update step.

    In the code below [w], [x] and [y] will always have simultaneous
    occurrences. They {e may} have simulatenous occurences with [z]
    if [send_w] and [send_z] are used with the same update step.

{[let w, send_w = E.create ()
let x = E.map succ w
let y = E.map succ x
let z, send_z = E.create ()

let () =
  let () = send_w 3 (* w x y occur simultaneously, z doesn't occur *) in
  let step = Step.create () in
  send_w ~step 3;
  send_z ~step 4;
  Step.execute step (* w x z y occur simultaneously *)
]}

    {2:update The update step and thread safety}

    {{!primitives}Primitives} are the only mean to drive the reactive
    system and they are entirely under the control of the client. When
    the client invokes a primitive's update function without the
    [step] argument or when it invokes {!Step.execute} on a [step]
    value, React performs an update step.

    To ensure correctness in the presence of threads, update steps
    must be executed in a critical section. Let uset([p]) be the set
    of events and signals that need to be updated whenever the
    primitive [p] is updated.  Updating two primitives [p] and [p']
    concurrently is only allowed if uset([p]) and uset([p']) are
    disjoint. Otherwise the updates must be properly serialized.

    Below, concurrent, updates to [x] and [y] must be serialized (or
    performed on the same step if it makes sense semantically), but z
    can be updated concurently to both [x] and [y].

{[open React;;

let x, set_x = S.create 0
let y, send_y = E.create ()
let z, set_z = S.create 0
let max_xy = S.l2 (fun x y -> if x > y then x else y) x (S.hold 0 y)
let succ_z = S.map succ z]}

    {2:sideeffects Side effects}

    Effectful events and signals perform their side effect
    exactly {e once} in each {{!steps}update step} in which there
    is an update of at least one of the event or signal it depends on.

    Remember that a signal updates in a step iff its
    {{!sigeq}equality function} determined that the signal
    value changed. Signal initialization is unconditionally considered as
    an update.

    It is important to keep references on effectful events and
    signals. Otherwise they may be reclaimed by the garbage collector.
    The following program prints only a [1].
{[let x, set_x = S.create 1
let () = ignore (S.map print_int x)
let () = Gc.full_major (); List.iter set_x [2; 2; 3]]}
    {2:lifting Lifting}

    Lifting transforms a regular function to make it act on signals.
    The combinators
    {!S.const} and {!S.app} allow to lift functions of arbitrary arity n,
    but this involves the inefficient creation of n-1 intermediary
    closure signals. The fixed arity {{!S.lifting}lifting
    functions} are more efficient. For example :
{[let f x y = x mod y
let fl x y = S.app (S.app ~eq:(==) (S.const f) x) y (* inefficient *)
let fl' x y = S.l2 f x y                            (* efficient *)
]}
    Besides, some of [Stdlib]'s functions and operators are
    already lifted and availables in submodules of {!S}. They can be
    be opened in specific scopes. For example if you are dealing with
    float signals you can open {!S.Float}.
{[open React
open React.S.Float

let f t = sqrt t *. sin t (* f is defined on float signals *)
...
open Stdlib (* back to Stdlib floats *)
]}
   If you are using OCaml 3.12 or later you can also use the [let open]
   construct
{[let open React.S.Float in
let f t = sqrt t *. sin t in (* f is defined on float signals *)
...
]}

  {2:recursion Mutual and self reference}

  Mutual and self reference among time varying values occurs naturally
  in programs. However a mutually recursive definition of two signals
  in which both need the value of the other at time t to define
  their value at time t has no least fixed point. To break this
  tight loop one signal must depend on the value the other had at time
  t-dt where dt is an infinitesimal delay.

  The fixed point combinators {!E.fix} and {!S.fix} allow to refer to
  the value an event or signal had an infinitesimal amount of time
  before. These fixed point combinators act on a function [f] that takes
  as argument the infinitesimally delayed event or signal that [f]
  itself returns.

  In the example below [history s] returns a signal whose value
  is the history of [s] as a list.
{[let history ?(eq = ( = )) s =
  let push v = function
    | [] -> [ v ]
    | v' :: _ as l when eq v v' -> l
    | l -> v :: l
  in
  let define h =
    let h' = S.l2 push s h in
    h', h'
  in
  S.fix [] define]}
  When a program has infinitesimally delayed values a
  {{!primitives}primitive} may trigger more than one update
  step. For example if a signal [s] is infinitesimally delayed, then
  its update in a step [c] will trigger a new step [c'] at the end
  of the step in which the delayed signal of [s] will have the value
  [s] had in [c]. This means that the recursion occuring between a
  signal (or event) and its infinitesimally delayed counterpart must
  be well-founded otherwise this may trigger an infinite number
  of update steps, like in the following examples.
{[let start, send_start = E.create ()
let diverge =
  let define e =
    let e' = E.select [e; start] in
    e', e'
  in
  E.fix define

let () = send_start ()        (* diverges *)

let diverge =                 (* diverges *)
  let define s =
    let s' = S.Int.succ s in
    s', s'
  in
  S.fix 0 define]}
  For technical reasons, delayed events and signals (those given to
  fixing functions) are not allowed to directly depend on each
  other. Fixed point combinators will raise [Invalid_argument] if
  such dependencies are created. This limitation can be
  circumvented by mapping these values with the identity.

  {2:strongstop Strong stops}

  Strong stops should only be used on platforms where weak arrays have
  a strong semantics (i.e. JavaScript). You can safely ignore that
  section and the [strong] argument of {!E.stop} and {!S.stop}
  if that's not the case.

  Whenever {!E.stop} and {!S.stop} is called with [~strong:true] on a
  reactive value [v], it is first stopped and then it walks over the
  list [prods] of events and signals that it depends on and
  unregisters itself from these ones as a dependent (something that is
  normally automatically done when [v] is garbage collected since
  dependents are stored in a weak array). Then for each element of
  [prod] that has no dependents anymore and is not a primitive it
  stops them aswell and recursively.

  A stop call with [~strong:true] is more involved. But it allows to
  prevent memory leaks when used judiciously on the leaves of the
  reactive system that are no longer used.

  {b Warning.} It should be noted that if direct references are kept
  on an intermediate event or signal of the reactive system it may
  suddenly stop updating if all its dependents were strongly stopped. In
  the example below, [e1] will {e never} occur:
{[let e, e_send = E.create ()
let e1 = E.map (fun x -> x + 1) e (* never occurs *)
let () =
  let e2 = E.map (fun x -> x + 1) e1 in
  E.stop ~strong:true e2
]}
  This can be side stepped by making an artificial dependency to keep
  the reference:
{[let e, e_send = E.create ()
let e1 = E.map (fun x -> x + 1) e (* may still occur *)
let e1_ref = E.map (fun x -> x) e1
let () =
  let e2 = E.map (fun x -> x + 1) e1 in
  E.stop ~strong:true e2
]}

  {1:ex Examples}

  {2:clock Clock}

  The following program defines a primitive event [seconds] holding
  the UNIX time and occuring on every second. An effectful event
  converts these occurences to local time and prints them on stdout
  along with an
  {{:http://www.ecma-international.org/publications/standards/Ecma-048.htm}ANSI
  escape sequence} to control the cursor position.
{[let pr_time t =
  let tm = Unix.localtime t in
  Printf.printf "\x1B[8D%02d:%02d:%02d%!"
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

open React;;

let seconds, run =
  let e, send = E.create () in
  let run () =
    while true do send (Unix.gettimeofday ()); Unix.sleep 1 done
  in
  e, run

let printer = E.map pr_time seconds

let () = run ()]}
*)

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
