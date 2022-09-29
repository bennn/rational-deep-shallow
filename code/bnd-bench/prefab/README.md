
- generative = baseline failure
- applicative = prefab attempt, failed
- opaque = prefab attempt, fails because opaques are generative
- a2 = adaptor with the opaque def and nothing else, works!
  don't need prefab either
  ... should be fine in the wild,
      but need to rename all constructor calls to `make-X` or whatever
- unsafe = best of all? lets see what happens with constructors

mf idea: exploit unsoundness of repeat struct defs
- what if struct created in untyped code?
- 


