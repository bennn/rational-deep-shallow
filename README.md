# go here

https://github.com/bennn/gfd-oopsla-2023


- - -

old stuff below

# strategies

Current success critieria:
  - >2   typed modules
  - <=2x overhead


1. Greedy strategy, maximize performance at each step
   - [bnd] find the slowest boundary:
     + UD => DD
     + US => SS
     + SD => DD
   - [prf] find the module with the highest total% time:
     + U => S
     + S => D
   - TODO [prf] with self% time

2. Busy strategy (Lazy), add types as a last resort
   - [bnd] sort boundaries by TT > UU, then by slowdown
     + SD => DD
     + UD => SD (S types are easier than D types, less work)
     + US => UD
   - [prf] find slowest typed module, then fall back to untyped:
     + S => D
     + U => S

3. TwoShot strategy, always add types, jump between D and S twice
  - [bnd]
    - (allowed once) if any D modules and overhead >10x, change all D to S
    - (allowed once, after prev jump) if no other steps, change all S to D
    - otherwise find slowest boundary:
      + UD => DD
      + US => SS
      + DS => DD
  - [prf]
    - (allowed once) if any D modules and overhead >10x, change all D to S
    - (allowed once, after prev jump) if no other steps, change all S to D
    - otherwise find slowest module
      + U => D // U => S depending on stage
      + ???

