(defpackage #:info.isoraqathedh.umpns.asdf
  (:use #:cl #:asdf))
(in-package #:info.isoraqathedh.umpns.asdf)

(defsystem umpns
  :name "UMPNS-based Minor Planet Namer"
  :version "0.1"
  :license "MIT"
  :components ((:file "umpns")
               (:file "database-ops")
               (:file "read-from-database")
               (:file "dump-to-database"))
  :depends-on (:cl-ppcre :sqlite :iterate :local-time))
