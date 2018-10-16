(defpackage #:chess-engine
  (:use #:cl
        #:zombie-raptor
        #:cl-chess/board
        #:cl-chess/graphics)
  (:import-from #:bordeaux-threads
                #:lock
                #:make-lock
                #:with-lock-held
                #:make-thread)
  (:import-from #:uiop
                #:launch-program
                #:process-info-input
                #:process-info-output
                #:wait-process)
  (:import-from #:uiop/launch-program
                #:process-info)
  (:export #:chess-engine
           #:chess-game-replay))

(in-package #:chess-engine)

(deftype move ()
  `(simple-string 5))

(defconstant +move-length+ 5)
(defconstant +possible-promotions+ 16)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-function (make-move :inline t) ()
    (replace (make-string 5 :initial-element #\Nul) "0000")))

(define-function (promotion? :inline t) (move)
  (not (char= (char move 4) #\Nul)))

(define-function (null-move? :inline t) (move)
  (string= #.(make-move) move))

(defstruct (chess-engine (:constructor %make-chess-engine))
  (process nil :type process-info :read-only t)
  (input   nil :type stream       :read-only t)
  (output  nil :type stream       :read-only t)
  (debug   nil :type (maybe stream))
  (name    nil :type string       :read-only t)
  (prompt  nil :type string       :read-only t))

(define-function (make-chess-engine :inline t) (&key process name prompt debug-stream)
  (%make-chess-engine :process process
                      :input (process-info-input process)
                      :output (process-info-output process)
                      :debug debug-stream
                      :name name
                      :prompt prompt))

(define-accessor-macro with-chess-engine #.(symbol-name '#:chess-engine-))

(define-function (print-chess-engine-output :inline t) (name line debug-stream)
  (when debug-stream
    (format debug-stream "~A : ~A~%" name line)))

(defun run-command (command process-input &optional (prompt "> ") debug-stream end)
  (when debug-stream
    (write-string prompt debug-stream)
    (write-string command debug-stream :end end)
    (terpri debug-stream))
  (write-line command process-input :end end)
  (force-output process-input))

(defmacro do-read-char ((char stream &key eof no-hang) &body body)
  (let ((read-char (if no-hang 'read-char-no-hang 'read-char))
        (end-condition `(eql ,eof ,char)))
    `(do ((,char (,read-char ,stream nil ,eof)
                 (,read-char ,stream nil ,eof)))
         (,(if no-hang
               `(or (null ,char) ,end-condition)
               end-condition))
         ,@body)))

(define-function read-opening-message ((chess-engine chess-engine))
  (with-chess-engine (output name debug) chess-engine
    (when debug (format debug "~A : " name))
    (do-read-char (char output :eof :eof :no-hang t)
      (when debug (write-char char debug)))))

(define-function chess-engine-leftover-output ((chess-engine chess-engine))
  (with-chess-engine (output name debug) chess-engine
    (let ((first? t))
      (do-read-char (char output :eof :eof)
        (when first?
          (when debug (format debug "~A : " name))
          (setf first? nil))
        (when debug (write-char char debug))))))

;;; todo: kill process if uciok is never received
(define-function (initialize-uci :inline t) (name input output prompt debug-stream)
  (run-command "uci" input prompt debug-stream)
  ;; todo: parse the metadata, especially the options
  (do ((line (read-line output nil)
             (read-line output nil)))
      ((or (eql :eof line)
           (string= "uciok" line))
       (print-chess-engine-output name line debug-stream))
    (print-chess-engine-output name line debug-stream)))

(define-function (set-option :inline t) (name value input prompt debug-stream)
  (run-command (format nil "setoption name ~A~@[ value ~A~]" name value) input prompt debug-stream))

;;; todo: verify that the chess engine says "readyok"
(define-function (ready? :inline t) (name input output prompt debug-stream)
  (run-command "isready" input prompt debug-stream)
  (print-chess-engine-output name (read-line output) debug-stream))

(define-function (new-game :inline t) (name input output prompt debug-stream)
  (run-command "ucinewgame" input prompt debug-stream)
  (ready? name input output prompt debug-stream))

(define-function (go-ponder :inline t) (input prompt debug-stream)
  (run-command "go ponder" input prompt debug-stream))

(define-function (go-move :inline t) (seconds input prompt debug-stream)
  (run-command (format nil "go movetime ~D000" seconds) input prompt debug-stream))

(define-function (ponder-hit :inline t) (input prompt debug-stream)
  (run-command "ponderhit" input prompt debug-stream))

(define-function (stop :inline t) (input prompt debug-stream)
  (run-command "stop" input prompt debug-stream))

(defun initialize-chess-engine (chess-engine threads)
  (with-chess-engine (input output name prompt debug)
      chess-engine
    (read-opening-message chess-engine)
    (initialize-uci name input output prompt debug)
    (set-option "Threads" threads input prompt debug)
    (ready? name input output prompt debug)
    (new-game name input output prompt debug)))

(define-function quit-chess-engine ((chess-engine chess-engine))
  (with-chess-engine (process input prompt debug)
      chess-engine
    (run-command "quit" input prompt debug)
    (wait-process process)
    (chess-engine-leftover-output chess-engine)))

(define-function quit-chess-engines ((chess-engine-1 chess-engine) (chess-engine-2 chess-engine))
  (quit-chess-engine chess-engine-1)
  (quit-chess-engine chess-engine-2))

(define-function update-position (chess-engine position-string (ponder-move (maybe move)) end)
  (with-chess-engine (input prompt debug)
      chess-engine
    (when ponder-move
      (setf (char position-string end) #\Space)
      (replace position-string ponder-move :start1 (1+ end)))
    (run-command position-string
                 input
                 prompt
                 debug
                 (if ponder-move
                     (+ end (if (promotion? ponder-move)
                                +move-length+
                                (1- +move-length+)))
                     end))
    (when ponder-move
      (fill position-string #\Nul :start end :end (+ end +move-length+)))))

;;; todo: fix determining the value of pondering
(defun chess-engine-move (chess-engine seconds debug-info)
  (with-chess-engine (input output name prompt debug)
      chess-engine
    (go-move seconds input prompt debug)
    (do ((line (read-line output nil :eof)
               (read-line output nil :eof))
         (checkmate? nil))
        ((or (eql :eof line)
             checkmate?
             (and (>= (length line) 8) (string= "bestmove" line :start2 0 :end2 8)))
         (if checkmate?
             (values :checkmate nil)
             (let ((ponder? (position #\Space line :start 9)))
               (print-chess-engine-output name line debug)
               (values (replace (make-move) (subseq line 9 ponder?))
                       (if ponder?
                           (if (and (> (length line) (+ 8 ponder?))
                                    (string= "ponder " line :start2 (1+ ponder?) :end2 (+ 8 ponder?)))
                               (subseq line (+ 8 ponder?))
                               (error "Invalid syntax in line: ~A" line))
                           nil)))))
      (let ((info? (and (>= (length line) 4) (string= "info" line :start2 0 :end2 4))))
        (when info?
          (let ((mate? (search "mate " line)))
            (when mate?
              (let* ((number-start (+ 5 mate?))
                     (number-end (position #\Space line :start number-start))
                     (mate-number (if number-end
                                      (parse-integer line :start number-start :end number-end)
                                      (parse-integer line :start number-start))))
                (when (<= mate-number 0)
                  (setf checkmate? t))))))
        (unless (or (not debug)
                    (and (not debug-info) info?))
          (format debug "~A : ~A~%" name line))))))

(define-function chess-engine-half-turn ((engine-active chess-engine)
                                         (move move)
                                         position-string
                                         position-string-position
                                         seconds
                                         debug-info)
  (update-position engine-active position-string nil position-string-position)
  (let ((move* (chess-engine-move engine-active seconds debug-info)))
    (check-type move* (or move (eql :checkmate)))
    (unless (eql move* :checkmate)
      (replace move move*))
    (setf (char position-string position-string-position) #\Space)
    (replace position-string move :start1 (1+ position-string-position))
    (eql move* :checkmate)))

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

(define-function make-uci-client ((game-status game-status)
                                  (engine-name-1 string)
                                  (engine-name-2 string)
                                  (threads (integer 3 8192))
                                  (turns (integer -1 200))
                                  (seconds (integer 1))
                                  (debug-stream (maybe stream))
                                  (debug-info boolean))
  (lambda ()
    (with-game-status ((current-move move) done? move-lock status-lock)
        game-status
      (let* ((mirror-match? (string= engine-name-1 engine-name-2))
             (chess-engine-1 (make-chess-engine :process (launch-program engine-name-1 :input :stream :output :stream)
                                                :name (if mirror-match? (concatenate 'string engine-name-1 "-1") engine-name-1)
                                                :prompt "1 > "
                                                :debug-stream debug-stream))
             (chess-engine-2 (make-chess-engine :process (launch-program engine-name-2 :input :stream :output :stream)
                                                :name (if mirror-match? (concatenate 'string engine-name-2 "-2") engine-name-2)
                                                :prompt "2 > "
                                                :debug-stream debug-stream)))
        (unwind-protect
             (progn
               (let ((threads (1- threads)))
                 (initialize-chess-engine chess-engine-1 threads)
                 (initialize-chess-engine chess-engine-2 threads))
               (let ((position-string-prefix "position startpos moves"))
                 (do ((half-turn 0 (1+ half-turn))
                      (board (make-board))
                      (move (make-move))
                      (chess-engines (vector chess-engine-1 chess-engine-2))
                      (position-string (replace (make-array (+ (length position-string-prefix) (+ (* 400 +move-length+)
                                                                                                  +possible-promotions+))
                                                            :element-type 'character
                                                            :initial-element #\Nul)
                                                position-string-prefix))
                      (position-string-position (length position-string-prefix)))
                     ((with-lock-held (status-lock) done?))
                   (when (zerop (mod half-turn 2))
                     (print-chess-engine-output "DEBUG"
                                                (format nil "Turn ~D" (1+ (ash half-turn -1)))
                                                debug-stream))
                   (let ((checkmate? (chess-engine-half-turn (aref chess-engines (mod half-turn 2))
                                                             move
                                                             position-string
                                                             position-string-position
                                                             seconds
                                                             debug-info)))
                     (with-lock-held (move-lock)
                       (replace current-move move))
                     (incf position-string-position
                           (if (promotion? move)
                               (1+ +move-length+)
                               +move-length+))
                     (if checkmate?
                         (with-lock-held (status-lock)
                           (unless done?
                             (setf done? :checkmate)))
                         (update-board board move))
                     (when (>= (1+ half-turn) (* 2 turns))
                       (with-lock-held (status-lock)
                         (unless done?
                           (setf done? :out-of-turns))))))))
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
     (threads 8 (integer 3 8192))
     (seconds 10 (integer 1))
     (turns 3 (integer -1 200))
     (debug-stream nil (maybe stream))
     (debug-info nil boolean)
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
                                                    threads
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
