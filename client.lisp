(defpackage #:cl-chess/client
  (:use #:cl
        #:zombie-raptor
        #:cl-chess/board
        #:cl-chess/game
        #:cl-chess/graphics
        #:cl-chess/uci)
  (:import-from #:bordeaux-threads
                #:make-lock
                #:with-lock-held
                #:make-thread)
  (:export #:chess-engine-gui
           #:chess-game-replay))

(in-package #:cl-chess/client)

;;; todo: correctly handle promotions and checkmates

;;; todo: When passing in time, also pass in units. Support
;;; milliseconds, seconds, and minutes and convert to
;;; milliseconds. Does this mean multiple units passed in or does this
;;; mean seconds/minutes are no longer integers?

(define-function (make-chess-gui :inline t) (width height script-function init-function &key fullscreen)
  "
Makes a chess GUI, i.e. a chess game client. This does not include the
chess AI.
"
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
    (make-game :name :cl-chess-client
               :settings settings
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
     (height 720 (integer 200))
     fullscreen)
  "
Launches a chess GUI that plays out a prerecorded game described by
the given moves vector.
"
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
                          #'make-chess-graphics
                          :fullscreen fullscreen)
          moves))

(define-function update-visual-board* (&key hud-ecs (state nil game-status) &allow-other-keys)
  (with-game-status (move move-lock) state
    (with-lock-held (move-lock)
      (unless (null-move? move)
        (update-visual-board hud-ecs move)
        (replace move #.(make-move))))))

(define-function make-init-function ((profile-1 chess-engine-profile)
                                     (profile-2 chess-engine-profile)
                                     (config game-configuration))
  "Returns the function that is called when the game is started."
  ;; Init function
  (lambda (&key ecs hud-ecs mesh-keys width height)
    (let ((game-status (make-game-status))
          (ecs-labels nil))
      (flet ((exit-function ()
               (with-game-status (done? status-lock) game-status
                 (with-lock-held (status-lock)
                   (unless done?
                     (setf done? :gui-quit))))))
        ;; UCI client thread
        (make-thread (make-uci-client game-status profile-1 profile-2 config))
        ;; Graphics init
        (make-chess-graphics :ecs ecs
                             :hud-ecs hud-ecs
                             :mesh-keys mesh-keys
                             :width width
                             :height height)
        (values ecs-labels
                game-status
                #'exit-function)))))

;;; TODO: Handle the end of game results (such as draws and
;;; checkmates) properly. This is not trivial because it requires
;;; implementing chess's official rules so that the moves can be
;;; verified, especially the rules for draws.
(define-function (chess-engine-gui :check-type t)
    (&key
     (engine-name-1 "stockfish" string)
     (engine-name-2 "stockfish" string)
     ;; TODO: Allow boolean. If NIL, then 1 thread. If T, then get the
     ;; number of CPU threads if possible, e.g. via calling nproc in
     ;; Linux.
     (threads 8 (integer 1 8192))
     (seconds 10 (integer 1))
     (turns 3 (integer 1 200))
     (debug-stream nil (maybe stream))
     (debug-info 2 (integer 0 3))
     (width 1280 (integer 200))
     (height 720 (integer 200))
     fullscreen)
  "
Launches a chess GUI with the specified configuration. For now, AI
against AI is all that is supported and the AI (chess engine) must be
a separate process communicating via the UCI protocol.
"
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
                                                               :debug-stream debug-stream))
                  :fullscreen fullscreen))
