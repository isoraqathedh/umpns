(defpackage #:info.isoraqathedh.umpns.asdf
  (:use #:cl #:asdf))
(in-package #:info.isoraqathedh.umpns.asdf)

(defsystem my-project
  :name "UMPNS-based Minor Planet Namer"
  :version "0.1"
  :license "MIT"
  :components ((:file "unified-minor-planet-naming-system")
               (:module "sub-directory"
                :components (:file "subdir-lisp-source-file")))
  :depends-on (:dependency-a
               :dependency-b))