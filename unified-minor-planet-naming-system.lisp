;;;; Conversion from provisional designation to UMPNS

(defpackage :info.isoraqathedh.umpns
  (:use :cl :iter :sqlite)
  (:nicknames :umnps ; <= Common Typo
   :umpns))
(in-package :umpns)

;; Connect to the database
(defvar *db* (connect
              (uiop:native-namestring
               (asdf:system-relative-pathname :umpns "mpcorb.db"))))

;; Some parsing stuff
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
  (position (char packed-form 3) "ABCDEFGHJKLMNOPQRSTUVWXY"))

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

(defun parse-packed-form (packed-form)
  "Turns a packed-form string into a list of two things: 
half-month of discovery and serial number."
  (list :discovery-half-month
        (let ((half-month (packed-form-half-month packed-form)))
          (encode-timestamp
           0 0 0 0 ; These values are irrelevant
           (if (evenp half-month) 1 16)
           (1+ (floor (/ half-month 2)))
           (packed-form-year packed-form)))
           :timezone +utc-zone+))
        :serial-number (packed-form-serial-number packed-form)))

(defun line-get-eccentricity (line)    (read-from-string (subseq line 71 80)))
(defun line-get-semi-major-axis (line) (read-from-string (subseq line 93 104)))
(defun line-get-inclination (line)     (read-from-string (subseq line 60 69)))

(defun aliases-line-number (alias-line)
  (+ (* 10000 (position (char alias-line 0) "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"))
     (parse-integer alias-line :start 1 :end 5)))

(defun aliases-line-alias (alias-line)
  (let ((packed-form-list-start 6) (packed-form-length 7))
    (iter
     (for caret-position
          from (+ packed-form-list-start packed-form-length)
          to (length alias-line)
          by packed-from-length)
     (for fragment = (subseq alias-line (- caret-position packed-form-length) caret-position))
     (when (packed-form-p fragment)
       (collect (parse-packed-form fragment))))))

(defun parse-alias-line (alias-line)
  (cons (aliases-line-number alias-line) (aliases-line-alias alias-line)))

(defun perihelion (semi-major-axis eccentricity)
  (* semi-major-axis (- 1 eccentricity)))

