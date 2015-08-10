;;;; Conversion from provisional designation to UMPNS

(defpackage :info.isoraqathedh.umpns
  (:use :cl)
  (:nicknames :umnps ; <= Common Typo
   :umpns))
(in-package :umpns)

(defun packed-form-p (string)
  "Checks if a string is a packed form or not."
  (cl-ppcre:scan "^[IJK][0-9][0-9][A-HJ-Y][A-Za-z0-9][0-9][A-HJ-Z]$" string))

(defun packed-form-year (packed-form)
  "Retrieves the year of discovery from the packed form."
  (+ (ecase (char packed-form 0)
       (#\I 1800)
       (#\J 1900)
       (#\K 2000))
     (parse-integer (subseq packed-form 1 3))))

(defun packed-form-half-month (packed-form)
  "Retrieves the half-month of discovery from the packed form."
  (char packed-form 3))

(defun packed-form-serial-number (packed-form)
  "Retrieves the serial number of discovery from the packed form."
  (+
   1 ;; One-based numbering
   (position (char packed-form 6) "ABCDEFGHJKLMNOPQRSTUVWXYZ")
   ;; Least significant letter
   (* 25 (position (char packed-form 5) "0123456789"))
   ;; Middle significant digit
   (* 250 ;; Most significant alphanum
      (position
       (char packed-form 4)
       "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijlmnopqrstuvwxyz"))))
