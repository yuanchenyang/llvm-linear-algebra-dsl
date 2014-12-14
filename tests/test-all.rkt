#lang racket

(require rackunit)
(require rackunit/text-ui)
(require "test-backend.rkt")
(require "test-frontend.rkt")
(require "test-dependencies.rkt")

(define all-tests
  (test-suite
   "All tests"
   backend-tests
   dependencies-tests
   frontend-tests))

(run-tests all-tests)
