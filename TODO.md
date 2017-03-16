# Exceptions

* Make steps resistant to exceptions ? There's more than one solution here
  one is to discard the step and unschedule all nodes. Another would be
  to catch them an trap them like in Fut.


# New event combinators

* E.merge but only on simultanous occs ?
* E.Bool.flip

# Signal init.

Instead of having bare values why not always have signals ?
This would undermine the temptation of using S.value.

# Stopped nodes

Stopped nodes could be detected and considered as constant by
smart constructors.

# Multisample

Current combinators are not good for sampling multiple signals,
which is generally useful in conjunction with accum. TODO
maybe not in fact see list selector. Just compute the as a signal.
But maybe not always natural ?

# Recursive defs

Investigate the case when dynamics can replace signals with constants
one which could make a direct dep on the delay noded (and hence
raise). Doesn't seem possible but I suspect I saw this once.

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
