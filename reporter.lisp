(defpackage #:rove-codewars/reporter
  (:use #:cl
        #:str
        #:rove/core/stats
        #:rove/core/result
        #:rove/reporter
        #:rove/misc/stream
        #:rove/utils/reporter))
(in-package #:rove-codewars/reporter)

(defun escape-lf (s)
  (replace-all (string #\newline) "<:LF:>" s))

; TODO Test duration. Assertions have durations so report the sum?
(defclass codewars-reporter (reporter)
  ((level :initform 0
          :accessor reporter-level)))

(defmethod test-begin ((reporter codewars-reporter) test-name &optional count)
  (declare (ignore count))
  (call-next-method)
  (incf (reporter-level reporter))
  ; Use level to skip "Testing System ..." and the package name
  (when (and test-name (> (reporter-level reporter) 2))
    ; HACK rove doesn't seem to provide a way to check if the test contains another test (from `testing`)
    ;      so we're using `DESCRIBE` to avoid potentially nesting `IT` which is invalid.
    (let ((stream (reporter-stream reporter))
          (group (if (> (reporter-level reporter) 3) "IT" "DESCRIBE")))
      (format stream "~%<~a::>~a~%" group test-name))))

(defmethod test-finish ((reporter codewars-reporter) test-name)
  (declare (ignore test-name))
  (multiple-value-bind (passedp context) (call-next-method)
    (declare (ignore context))
    (when (> (reporter-level reporter) 2)
      (format (reporter-stream reporter) "~%<COMPLETEDIN::>~%"))
    (decf (reporter-level reporter))
    passedp))

(defmethod record ((reporter codewars-reporter) (object passed-assertion))
  (call-next-method)
  (format (reporter-stream reporter) "~%<PASSED::>Test Passed~%"))

(defmethod record ((reporter codewars-reporter) (object failed-assertion))
  (call-next-method)
  (let ((*print-circle* t)
        (*print-assertion* t))
    ; TODO Show assertion-stacks
    (format (reporter-stream reporter)
            "~%<FAILED::>~a~%"
            (escape-lf
              (format nil "~a~%~a" (assertion-description object) (prin1-to-string object))))))

(defmethod record ((reporter codewars-reporter) (object pending-assertion))
  (call-next-method)
  (format (reporter-stream reporter) "~%<LOG::Pending>~a~%" (assertion-description object)))

(rove/reporter/registry:add-reporter :codewars 'codewars-reporter)
