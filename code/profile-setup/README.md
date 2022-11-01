profile-setup
===

Tools for collecting profile data = boundary profile and statistical profile.

- `bmg.rkt` = Main script. Manage runs.
- `boundary-profile.rkt` = Modified contract profiler. Put next to main script.
- `raco.rkt`, `render-json.rkt` = Changes for statistical profiler. Move to
  your Racket installation.
- `unsafe.rkt` = Extra file for require-typed-check. Move to your copy of the
  package.
