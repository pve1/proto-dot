(asdf:defsystem #:proto-dot
  :description ""
  :author "Peter von Etter"
  :license  "LLGPL"
  :version "0.0.1"
  :components ((:file "tools")
               (:file "proto-dot"))
  :depends-on (#:alexandria))
