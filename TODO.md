
# New event combinators 

```ocaml
E.Option.some : 'a option event -> 'a event 
(** [some e] is [E.fmap (fun v -> v) e] *)

S.Option.some : 'a -> 'a option signal -> 'a signal
```



# New signal combinators. 

To avoid uses of S.value we need better ways to access a 
signal's current value and inject it in an efficient 
way in the graph.

```ocaml
S.freeze : 'a signal -> 'a signal 
(** [freeze s]_{t} = [s]_{t'} where t' is freeze's creation time. *)
```

See if we can return a const and if what happens when used with 
bind and/or provide an alternative S.bind for bootstraping.



