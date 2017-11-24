(asdf:defsystem #:cl-chess
  :serial t
  :description "A chess GUI written in CL"
  :author "Michael Babich"
  :license "MIT"
  :depends-on (:pngload
               :uiop
               :zombie-raptor)
  :components ((:file "chess-engine")))