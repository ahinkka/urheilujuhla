(defpackage #:urheilujuhla-tests
  (:use #:common-lisp #:urheilujuhla #:lift)
  (:export #:run-all-tests #:run-all-tests-non-interactive))

(in-package :urheilujuhla-tests)


(deftestsuite urheilujuhla-suite () ())

;;;
;;; Testing sparkline-function
;;;
(deftestsuite sparkline (urheilujuhla-suite)
  ((expected-output "▁▃▅█")
   (good-list '(1 2 3 4))
   (good-vector #(1 2 3 4))

   (expected-nil-output "▁X▅█")
   (nil-in-list '(1 nil 3 4))
   (nil-in-vector #(1 nil 3 4))

   (expected-all-nil-output "N/A")
   (all-nil-list '(nil nil nil nil))
   (all-nil-vector #(nil nil nil nil)))
   
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

;; All nils
(addtest (sparkline) all-nil-list
  (ensure-same (urheilujuhla::sparkline all-nil-list)
	       expected-all-nil-output))

(addtest (sparkline) all-nil-vector
  (ensure-same (urheilujuhla::sparkline all-nil-vector)
	       expected-all-nil-output))


;;;
;;; Testing thread setup and teardown
;;;
(deftestsuite thread-control (urheilujuhla-suite)
  ())

;;;
;;; Helpers
;;;
(defun run-all-tests ()
  (run-tests :suite (find-testsuite 'urheilujuhla-suite)))

(defun run-all-tests-non-interactive ()
  (run-all-tests)
  (terpri)
  (format t "~a~%" lift:*test-result*)

  (when
      (or 
       (not (null (lift:failures lift:*test-result*)))
       (not (null (lift:errors lift:*test-result*))))
    (describe (lift:failures lift:*test-result*))
    (describe (lift:errors lift:*test-result*))
    (error "Test failures or errors encountered."))

  lift:*test-result*)
