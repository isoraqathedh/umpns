;;;; Unified Minor Planet Naming System (UMPNS) as applied to the solar system
;;;; Section: Database connection and configuration

(in-package :umpns)

;;; Database + Minor Planet number -> alias
(defun minor-planet-props (number)
  (execute-one-row-m-v *db* "SELECT \"year\", \"half-month\", \"number\" FROM \"aliases\" 
WHERE \"minor-planet-number\" = ? ORDER BY \"year\", \"half-month\" " number))
