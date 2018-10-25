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

(define-function make-uci-client ((game-status game-status)
                                  (engine-name-1 string)
                                  (engine-name-2 string)
                                  (threads (integer 1 8192))
                                  (turns (integer -1 200))
                                  (seconds (integer 1))
                                  (debug-stream (maybe stream))
                                  (debug-info (integer 0 3)))
  (lambda ()
    (with-game-status ((current-move move) done? move-lock status-lock)
        game-status
      (multiple-value-bind (chess-engine-1 chess-engine-2)
          (make-chess-engine-pair (make-chess-engine-profile :name engine-name-1
                                                             :debug-stream debug-stream
                                                             :debug-info debug-info)
                                  (make-chess-engine-profile :name engine-name-2
                                                             :debug-stream debug-stream
                                                             :debug-info debug-info))
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
                                       debug-stream)))))))

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
                  (lambda (&key hud-ecs state &allow-other-keys)
                    (declare (game-status state))
                    (with-game-status (move move-lock) state
                      (with-lock-held (move-lock)
                        (unless (null-move? move)
                          (update-visual-board hud-ecs move)
                          (replace move #.(make-move))))))
                  (lambda (&key ecs hud-ecs mesh-keys width height)
                    (let ((game-status (make-game-status)))
                      (make-thread (make-uci-client game-status
                                                    engine-name-1
                                                    engine-name-2
                                                    (max (1- threads) 1)
                                                    turns
                                                    seconds
                                                    debug-stream
                                                    debug-info))
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
                                      (setf done? :gui-quit))))))))))
