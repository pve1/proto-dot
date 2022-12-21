(asdf:defsystem #:proto-dot
  :description "A library for building custom 'dot' macros, e.g. (dot employee company name) => \"Initech\"."
  :author "Peter von Etter"
  :license  "LGPL-3.0"
  :version "0.0.1"
  :components ((:file "tools")
               (:file "proto-dot"))
  :depends-on (#:alexandria))
