(defpackage #:urheilujuhla-tests
  (:use #:common-lisp #:urheilujuhla #:lift)
  (:export #:run))

(in-package :urheilujuhla-tests)

(deftestsuite sparkline ()
  ((expected-output "▁▃▅█")
   (good-list '(1 2 3 4))
   (good-vector #(1 2 3 4))

   (expected-nil-output "▁X▅█")
   (nil-in-list '(1 nil 3 4))
   (nil-in-vector #(1 nil 3 4)))
  (:equality-test #'string-equal))


;; Good input
(addtest (sparkline) good-list
  (ensure-same (urheilujuhla::sparkline good-list)
	       expected-output))

(addtest (sparkline) good-vector
  (ensure-same (urheilujuhla::sparkline good-vector)
	       expected-output))

;; With nil
(addtest (sparkline) nil-in-list
  (ensure-same (urheilujuhla::sparkline nil-in-list)
	       expected-nil-output))

(addtest (sparkline) nil-in-vector
  (ensure-same (urheilujuhla::sparkline nil-in-vector)
	       expected-nil-output))
