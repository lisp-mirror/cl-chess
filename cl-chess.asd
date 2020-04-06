(asdf:defsystem #:cl-chess
  :description "A chess GUI written in CL"
  :version "0"
  :author "Michael Babich"
  :maintainer "Michael Babich"
  :license "MIT"
  :class :package-inferred-system
  :depends-on (:bordeaux-threads
               :pngload
               :uiop
               :zombie-raptor
               :zr-utils
               :cl-chess/all))
