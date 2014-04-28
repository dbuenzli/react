
# Internal 

In signals there's a pattern that occurs quite a few times where we add a 
dependency only at the end of a cycle. There's quite some code replication
this should be factored out.

# New event combinators

* E.merge but only on simultanous occs ? 
* Review Bool.flip init.

# Signal init. 

Instead of having bare values why not always have signals ? 
This would undermine the temptation of using S.value.


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





