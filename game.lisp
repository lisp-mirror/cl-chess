(defpackage #:cl-chess/game
  (:use #:cl
        #:zombie-raptor
        #:zr-utils
        #:cl-chess/board
        #:cl-chess/uci)
  (:import-from #:bordeaux-threads
                #:lock
                #:make-lock
                #:with-lock-held)
  (:export #:game-configuration
           #:game-status
           #:make-game-configuration
           #:make-game-status
           #:make-headless-match
           #:make-uci-client
           #:with-game-configuration
           #:with-game-status))

(in-package #:cl-chess/game)

(defgeneric make-uci-client (game-status side-1 side-2 game-config))

(defstruct game-status
  (move (make-move) :type move)
  (done? nil :type (or boolean keyword))
  (move-lock (make-lock) :type lock)
  (status-lock (make-lock) :type lock))

(define-accessor-macro with-game-status #.(symbol-name '#:game-status-))

(define-function print-turn ((half-turn (integer 0 400))
                             (debug-stream (maybe stream)))
  (when (zerop (mod half-turn 2))
    (print-chess-engine-output "DEBUG"
                               (format nil "Turn ~D" (1+ (ash half-turn -1)))
                               debug-stream)))

;;; todo: Handle more than 200 turns.
(defstruct game-configuration
  (turns        200 :type (integer 1 200))
  (threads        4 :type (integer 1 8192))
  (seconds       10 :type (integer 1 *))
  (debug-stream nil :type (maybe stream)))

(define-accessor-macro with-game-configuration #.(symbol-name '#:game-configuration-))

;;; todo: Add a human player class or struct. Currently this requires
;;; both sides to be defined by chess-engine-profile. For a side that
;;; is not a chess-engine, accept input from an outside source (with
;;; locks) for that player. This allows the human player(s) to be side
;;; 1, side 2, or both.
;;;
;;; todo: If a side's controller changes, "end" the game and call this
;;; again. Allow an in-progress start with an existing position string
;;; (or board) and a half-turn not at 0. This means build up a
;;; position string even for human vs. human in case a CPU takes over
;;; a side later on.
;;;
;;; todo: Ponder when one AI is playing against a human.
(defmethod make-uci-client ((game-status game-status)
                            (profile-1 chess-engine-profile)
                            (profile-2 chess-engine-profile)
                            (game-config game-configuration))
  (lambda ()
    (with-game-status ((current-move move) done? move-lock status-lock)
        game-status
      (with-game-configuration (turns threads seconds debug-stream)
          game-config
        (multiple-value-bind (chess-engine-1 chess-engine-2)
            (make-chess-engine-pair profile-1 profile-2)
          (initialize-chess-engines chess-engine-1 chess-engine-2 threads)
          (unwind-protect
               (do ((half-turn 0 (1+ half-turn))
                    (board (make-board))
                    (move (make-move))
                    (chess-engines (vector chess-engine-1 chess-engine-2))
                    (position-string-and-position (make-position-string-and-position)))
                   ((with-lock-held (status-lock) done?))
                 (print-turn half-turn debug-stream)
                 (let ((checkmate? (half-turn (aref chess-engines (mod half-turn 2))
                                              move
                                              position-string-and-position
                                              seconds)))
                   (with-lock-held (move-lock)
                     (replace current-move move))
                   (with-lock-held (status-lock)
                     (cond (done? nil)
                           (checkmate? (setf done? :checkmate))
                           ((>= (1+ half-turn) (* 2 turns)) (setf done? :out-of-turns))))
                   (unless checkmate? (update-board board move))))
            (let ((outcome (with-lock-held (status-lock) done?)))
              (quit-chess-engines chess-engine-1 chess-engine-2)
              (print-chess-engine-output "DEBUG"
                                         (format nil "Final outcome: ~A" (if outcome outcome :crash))
                                         debug-stream))))))))

(defun make-headless-match (name-1 name-2 &key
                                            debug-stream
                                            (debug-info 2)
                                            (turns 200)
                                            (threads 4)
                                            (seconds 10))
  "
Creates a headless match between two UCI chess engines with
convenient, compact syntax that's ideal for testing.

Note: If there is no debug-stream, then there will be no apparent
output.
"
  (make-uci-client (make-game-status)
                   (make-chess-engine-profile :name name-1
                                              :debug-stream debug-stream
                                              :debug-info debug-info)
                   (make-chess-engine-profile :name name-2
                                              :debug-stream debug-stream
                                              :debug-info debug-info)
                   (make-game-configuration :turns turns
                                            :threads threads
                                            :seconds seconds
                                            :debug-stream debug-stream)))
