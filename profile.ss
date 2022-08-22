#!/usr/bin/env scheme-script

(import (chezscheme))

(parameterize ((compile-profile 'source) (compile-imported-libraries #t)) (compile-library "raytracer.scm"))
(import (raytracer))
(display "Running trace!\n")
(run-trace)
(display "Done.\n")
(profile-dump-html)
