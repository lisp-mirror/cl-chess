(defpackage #:chess-engine
  (:use #:cl
        #:zombie-raptor
        #:cl-chess/board
        #:cl-chess/graphics)
  (:import-from #:bordeaux-threads
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

;; fixme
;; move is length 4 or 5 with 5 being promotion
;; 0000 is the null move
;; Use \Nul when there is no promotion and resize all moves to 5
;; 0000\Nul is then the null move
;;
;; that means for the position string the increment is 5 or 6
;; depending on if there was a promotion or not
(deftype move ()
  `(simple-string 4))

(defconstant +move-length+ 4)
(defconstant +possible-promotions+ 16)

(define-function (make-move :inline t) ()
  (replace (make-string 5) "0000"))

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

(define-function (read-opening-message :inline t) (name output debug-stream)
  (when debug-stream (format debug-stream "~A : " name))
  (do ((char (read-char-no-hang output nil :eof)
             (read-char-no-hang output nil :eof)))
      ((or (null char) (eql :eof char)))
    (when debug-stream (write-char char debug-stream))))

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
    (read-opening-message name output debug)
    (initialize-uci name input output prompt debug)
    (set-option "Threads" threads input prompt debug)
    (ready? name input output prompt debug)
    (new-game name input output prompt debug)))

(define-function chess-engine-leftover-output ((chess-engine chess-engine))
  (with-chess-engine (output name debug)
      chess-engine
    (do ((line (read-line output nil :eof)
               (read-line output nil :eof)))
        ((eql line :eof))
      (print-chess-engine-output name line debug))))

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
                 (if ponder-move (+ end 5) end))
    (when ponder-move
      (fill position-string #\Nul :start end :end (+ end 5)))))

;;; todo: what is move when the move is a promotion? is it length 5?
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
             (values "CHECKMATE" nil)
             (let ((ponder? (position #\Space line :start 9)))
               (print-chess-engine-output name line debug)
               (values (subseq line 9 ponder?)
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

(defun chess-engine-ponder-end (chess-engine success debug-info)
  (with-chess-engine (input output name prompt debug)
      chess-engine
    (if success
        (ponder-hit input prompt debug)
        (stop input prompt debug))
    ;; Note: technically, the info is generated while it's pondering
    ;; and merely read here after stop
    (do ((line (read-line output nil :eof)
               (read-line output nil :eof)))
        ((or (eql :eof line)
             (and (>= (length line) 8) (string= "bestmove" line :start2 0 :end2 8)))
         (print-chess-engine-output name line debug))
      (let ((info? (and (>= (length line) 4) (string= "info" line :start2 0 :end2 4))))
        (unless (or (not debug)
                    (and (not debug-info) info?))
          (format debug "~A : ~A~%" name line))))))

(define-function chess-engine-half-turn ((engine-active chess-engine)
                                         (engine-pondering chess-engine)
                                         position-string
                                         position-string-position
                                         (ponder-move (maybe move))
                                         seconds
                                         debug-info)
  (let ((move nil)
        (new-ponder-move nil))
    (update-position engine-active position-string nil position-string-position)
    (when ponder-move
      (update-position engine-pondering position-string ponder-move position-string-position)
      (with-chess-engine (input prompt debug)
          engine-pondering
        (go-ponder input prompt debug)))
    (setf (values move new-ponder-move)
          (chess-engine-move engine-active seconds debug-info))
    (check-type move move)
    (check-type new-ponder-move (maybe move) ponder-move)
    (setf (char position-string position-string-position) #\Space)
    (replace position-string move :start1 (1+ position-string-position) :end1 (+ 5 position-string-position))
    (when ponder-move
      (chess-engine-ponder-end engine-pondering (string= move ponder-move) debug-info))
    (values move new-ponder-move (string= move "CHECKMATE"))))

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

;;; todo: Record moves in algebraic notation
;;;
;;; todo: Handle draws and other edge cases.
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
     (debug-stream nil (or boolean stream))
     (debug-info nil boolean)
     (width 1280 (integer 200))
     (height 720 (integer 200)))
  (let* ((move-lock (make-lock))
         (engine-lock (make-lock))
         (status-lock (make-lock))
         (mirror-match? (string= engine-name-1 engine-name-2))
         (chess-engine-1 (make-chess-engine :process (launch-program engine-name-1 :input :stream :output :stream)
                                            :name (if mirror-match? (concatenate 'string engine-name-1 "-1") engine-name-1)
                                            :prompt "1 > "
                                            :debug-stream debug-stream))
         (chess-engine-2 (make-chess-engine :process (launch-program engine-name-2 :input :stream :output :stream)
                                            :name (if mirror-match? (concatenate 'string engine-name-2 "-2") engine-name-2)
                                            :prompt "2 > "
                                            :debug-stream debug-stream))
         (done? nil)
         (current-move (make-string 4 :initial-element #\Nul)))
    (flet ((quit-if-necessary (status-lock engine-lock final-status)
             (with-lock-held (status-lock)
               (unless done?
                 (setf done? final-status)
                 (with-lock-held (engine-lock)
                   (quit-chess-engines chess-engine-1 chess-engine-2))))))
      (make-thread (lambda ()
                     (let ((threads (floor (1- threads) 2)))
                       (initialize-chess-engine chess-engine-1 threads)
                       (initialize-chess-engine chess-engine-2 threads))
                     (let ((position-string-prefix "position startpos moves"))
                       (do ((half-turn 0 (1+ half-turn))
                            (board (make-board))
                            (move nil)
                            (moves (make-array 400 :fill-pointer 0))
                            (ponder-move "e2e4")
                            (chess-engines (vector chess-engine-1 chess-engine-2))
                            (position-string (replace (make-array (+ (length position-string-prefix) (+ (* 400 (1+ +move-length+))
                                                                                                        +possible-promotions+))
                                                                  :element-type 'character
                                                                  :initial-element #\Nul)
                                                      position-string-prefix))
                            (position-string-position (length position-string-prefix)
                                                      ;; fixme: this can't move forward a constant amount because
                                                      ;; there may or may not be a promotion
                                                      (+ (1+ +move-length+) position-string-position))
                            (checkmate? nil))
                           ((or (>= half-turn (* 2 turns)) checkmate? (with-lock-held (status-lock) done?))
                            (quit-if-necessary status-lock engine-lock (if checkmate? :checkmate :out-of-turns))
                            (print-chess-engine-output "DEBUG"
                                                       (format nil
                                                               "Final outcome: ~A"
                                                               (with-lock-held (status-lock)
                                                                 done?))
                                                       debug-stream))
                         (when (zerop (mod half-turn 2))
                           (print-chess-engine-output "DEBUG"
                                                      (format nil "Turn ~D" (1+ (ash half-turn -1)))
                                                      debug-stream))
                         (with-lock-held (engine-lock)
                           (setf (values move ponder-move checkmate?)
                                 (chess-engine-half-turn (aref chess-engines (mod half-turn 2))
                                                         (aref chess-engines (mod (1+ half-turn) 2))
                                                         position-string
                                                         position-string-position
                                                         ponder-move
                                                         seconds
                                                         debug-info)))
                         (with-lock-held (move-lock)
                           (replace current-move move))
                         (vector-push move moves)
                         (unless checkmate?
                           (update-board board move))))))
      (make-chess-gui width
                      height
                      (lambda (&key hud-ecs &allow-other-keys)
                        (with-lock-held (move-lock)
                          (unless (position #\Nul current-move)
                            (update-visual-board hud-ecs current-move)
                            (fill current-move #\Nul))))
                      (lambda (&key ecs hud-ecs mesh-keys width height)
                        (make-chess-graphics :ecs ecs
                                             :hud-ecs hud-ecs
                                             :mesh-keys mesh-keys
                                             :width width
                                             :height height)
                        (values nil
                                nil
                                (lambda ()
                                  (quit-if-necessary status-lock engine-lock :gui-quit))))))))
