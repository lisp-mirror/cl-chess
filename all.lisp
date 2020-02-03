(uiop:define-package #:cl-chess/all
  (:nicknames #:cl-chess)
  (:use #:cl
        #:cl-chess/util)
  (:use-reexport #:cl-chess/board
                 #:cl-chess/client
                 #:cl-chess/game
                 #:cl-chess/graphics
                 #:cl-chess/uci))
