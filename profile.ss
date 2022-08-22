#!/usr/bin/env scheme-script

(import (chezscheme))

(begin
  (parameterize ((compile-profile 'source) (compile-imported-libraries #t)) (load "raytracer.scm"))
  (parameterize ((compile-profile 'source)) (compile-library "raytracer.scm"))
  (import (raytracer))
  (display "Running trace!\n")
  (run-trace)
  (display "Done.\n")
  (profile-dump-html))
