;;;; Unified Minor Planet Naming System (UMPNS) as applied to the solar system
;;;; Section: Database connection and configuration

(in-package :umpns)

(defvar *db* (connect
              (uiop:native-namestring
               (asdf:system-relative-pathname :umpns "mpcorb.db"))))

(defun exec (query &rest params)
  (etypecase query
    (string (apply #'sqlite:execute-non-query *db* query params))
    (sqlite:sqlite-statement
     (loop for i from 1
           for param in params
           do (sqlite:bind-parameter query i param))
     (sqlite:step-statement query)
     (sqlite:reset-statement query))))

(defun setup-database ()
  (exec
   "CREATE TABLE \"planets\"
       (\"minor-planet-number\" INTEGER PRIMARY KEY NOT NULL,
        \"semimajor-axis\" REAL NOT NULL,
        \"year\" INTEGER,
        \"half-month\" INTEGER,
        \"eccentricity\" REAL NOT NULL,
        \"inclination\" REAL NOT NULL,
        \"serial-number\" INTEGER,
        \"vanity-name\" TEXT
        \"new-code\" TEXT)")
  (exec
   "CREATE TABLE \"aliases\"
       (\"year\" INTEGER NOT NULL,
        \"half-month\" INTEGER NOT NULL,
        \"number\" INTEGER NOT NULL,
        \"minor-planet-number\" REFERENCES \"planets\"(\"minor-planet-number\"))")
  
  (exec "CREATE UNIQUE INDEX \"aliases-index\" ON \"aliases\"(\"year\", \"half-month\", \"number\")")
  (exec "CREATE INDEX \"aliases-minor-planet-number\" ON \"aliases\"(\"minor-planet-number\")")
  (exec "CREATE INDEX \"aliases-year\" ON \"aliases\"(\"year\")")
  (exec "CREATE INDEX \"aliases-half-month\" ON \"aliases\"(\"year\",\"half-month\")"))

(defun clear-database ()
  (exec "DELETE FROM \"aliases\"")
  (exec "DELETE FROM \"planets\"")
  (exec "VACUUM"))
