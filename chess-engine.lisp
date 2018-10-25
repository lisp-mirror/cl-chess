(defpackage #:chess-engine
  (:use #:cl
        #:zombie-raptor
        #:cl-chess/board
        #:cl-chess/graphics
        #:cl-chess/uci)
  (:import-from #:bordeaux-threads
                #:lock
                #:make-lock
                #:with-lock-held
                #:make-thread)
  (:export #:chess-engine
           #:chess-game-replay))

(in-package #:chess-engine)

(defgeneric make-uci-client (game-status side-1 side-2 game-config))

(define-function (make-chess-gui :inline t) (width height script-function init-function &key fullscreen)
  (let ((settings (make-settings :title "CL Chess"
                                 :width width
                                 :height height
                                 :fullscreen fullscreen
                                 :app-name "cl-chess"
                                 :msaa 4
                                 :debug nil))
        (controls (make-controls :key-actions (key-actions)
                                 :key-bindings (key-bindings)
                                 :mouse-actions (mouse-actions))))
    (make-game :settings settings
               :shader-data (shader-data*)
               :textures (list (textures))
               :models (make-instance 'models :models (square-model))
               :controls controls
               :init-function init-function
               :script-function script-function)))

(define-function (chess-game-replay :check-type t)
    ((moves vector)
     &key
     (seconds 2 (integer 0))
     (debug-stream nil (maybe stream))
     (width 1280 (integer 200))
     (height 720 (integer 200)))
  (values (make-chess-gui width
                          height
                          (let ((i 0)
                                (board (make-board)))
                            (lambda (&key hud-ecs tick &allow-other-keys)
                              (when (and (= tick (* (1+ i) 100 seconds)) (< i (length moves)))
                                (let ((move (aref moves i)))
                                  (update-board board move)
                                  (update-visual-board hud-ecs move)
                                  (incf i)))))
                          #'make-chess-graphics)
          moves))

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

(defstruct game-configuration
  (turns        200 :type (integer -1 200))
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

(define-function update-visual-board* (&key hud-ecs (state nil game-status) &allow-other-keys)
  (with-game-status (move move-lock) state
    (with-lock-held (move-lock)
      (unless (null-move? move)
        (update-visual-board hud-ecs move)
        (replace move #.(make-move))))))

(define-function make-init-function ((profile-1 chess-engine-profile)
                                     (profile-2 chess-engine-profile)
                                     (config game-configuration))
  (lambda (&key ecs hud-ecs mesh-keys width height)
    (let ((game-status (make-game-status)))
      (make-thread (make-uci-client game-status profile-1 profile-2 config))
      (make-chess-graphics :ecs ecs
                           :hud-ecs hud-ecs
                           :mesh-keys mesh-keys
                           :width width
                           :height height)
      (values nil
              game-status
              (lambda ()
                (with-game-status (done? status-lock) game-status
                  (with-lock-held (status-lock)
                    (unless done?
                      (setf done? :gui-quit)))))))))

;;; todo: Record moves in algebraic notation
;;;
;;; todo: Handle draws and other edge cases.
;;;
;;; todo: restore the moves log, and somehow find a way to return it
;;;
;;; todo: allow for pondering when playing against a human
;;;
;;; todo: replace the position history with a FEN board if there are
;;; more than 200 turns
;;;
;;; Note: Having an infinite number of turns (i.e. -1 turns) is not
;;; recommended until the edge cases are handled, e.g. draws.
(define-function (chess-engine :check-type t)
    (&key
     (engine-name-1 "stockfish" string)
     (engine-name-2 "stockfish" string)
     (threads 8 (integer 1 8192))
     (seconds 10 (integer 1))
     (turns 3 (integer -1 200))
     (debug-stream nil (maybe stream))
     (debug-info 2 (integer 0 3))
     (width 1280 (integer 200))
     (height 720 (integer 200)))
  (make-chess-gui width
                  height
                  #'update-visual-board*
                  (make-init-function (make-chess-engine-profile :name engine-name-1
                                                                 :debug-stream debug-stream
                                                                 :debug-info debug-info)
                                      (make-chess-engine-profile :name engine-name-2
                                                                 :debug-stream debug-stream
                                                                 :debug-info debug-info)
                                      (make-game-configuration :threads (max (1- threads) 1)
                                                               :turns turns
                                                               :seconds seconds
                                                               :debug-stream debug-stream))))
