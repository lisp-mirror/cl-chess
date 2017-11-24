(defpackage #:chess-engine
  (:use #:cl)
  (:import-from #:bordeaux-threads
                #:make-lock
                #:with-lock-held)
  (:import-from #:pngload
                #:data
                #:height
                #:load-file
                #:width)
  (:import-from #:uiop
                #:launch-program
                #:make-pathname*
                #:merge-pathnames*
                #:process-info-input
                #:process-info-output
                #:wait-process)
  (:import-from #:zombie-raptor
                #:character-pipe
                #:define-shader
                #:define-shader-data
                #:empty?
                #:entity-component-system
                #:flat-index
                #:key-actions
                #:key-bindings
                #:main-data-directory
                #:make-basic-entity
                #:make-fps-camera-entity
                #:make-settings
                #:make-square
                #:make-window
                #:model
                #:mouse-actions
                #:shape
                #:texture
                #:vec))

(in-package #:chess-engine)

;;; Chess board

(defun make-board ()
  (make-array '(8 8)
              :element-type 'character
              :initial-contents '((#\r #\n #\b #\q #\k #\b #\n #\r)
                                  (#\p #\p #\p #\p #\p #\p #\p #\p)
                                  (#\Nul #\Nul #\Nul #\Nul #\Nul #\Nul #\Nul #\Nul)
                                  (#\Nul #\Nul #\Nul #\Nul #\Nul #\Nul #\Nul #\Nul)
                                  (#\Nul #\Nul #\Nul #\Nul #\Nul #\Nul #\Nul #\Nul)
                                  (#\Nul #\Nul #\Nul #\Nul #\Nul #\Nul #\Nul #\Nul)
                                  (#\P #\P #\P #\P #\P #\P #\P #\P)
                                  (#\R #\N #\B #\Q #\K #\B #\N #\R))))

(deftype board ()
  '(simple-array character (8 8)))

(defun print-board (board &optional unicode (stream t))
  (declare (board board))
  (dotimes (i 8)
    (write-char #\Space stream)
    (dotimes (j 8)
      (let ((piece (let ((piece (aref board i j)))
                     (if (not (char= #\Nul piece))
                         piece
                         #\Space))))
        (write-char (if unicode
                        (ecase piece
                          (#\P (code-char #x2659))
                          (#\p (code-char #x265F))
                          (#\N (code-char #x2658))
                          (#\n (code-char #x265E))
                          (#\B (code-char #x2657))
                          (#\b (code-char #x265D))
                          (#\R (code-char #x2656))
                          (#\r (code-char #x265C))
                          (#\Q (code-char #x2655))
                          (#\q (code-char #x265B))
                          (#\K (code-char #x2654))
                          (#\k (code-char #x265A))
                          (#\Space (if (or (and (zerop (mod j 2)) (not (zerop (mod i 2))))
                                           (and (not (zerop (mod j 2))) (zerop (mod i 2))))
                                       (code-char #x2592)
                                       #\Space)))
                        piece)
                    stream))
      (write-char #\Space stream))
    (terpri stream)))

;;; note: This is just the board part of the FEN representation
(defun print-fen-board (board &optional (stream t))
  (declare (board board))
  (dotimes (i 8)
    (let ((counter 0))
      (declare ((integer 0 8) counter))
      (dotimes (j 8)
        (let ((piece (aref board i j)))
          (if (not (char= #\Nul piece))
              (progn
                (unless (zerop counter)
                  (format stream "~D" counter)
                  (setf counter 0))
                (write-char piece stream))
              (incf counter))))
      (unless (zerop counter)
        (format stream "~D" counter)))
    (unless (= i 7)
      (format stream "/")))
  (terpri stream))

(declaim (inline %chess-board-ref))
(defun %chess-board-ref (board char-0 char-1)
  (aref board
        (ecase char-1
          (#\1 (- 8 1))
          (#\2 (- 8 2))
          (#\3 (- 8 3))
          (#\4 (- 8 4))
          (#\5 (- 8 5))
          (#\6 (- 8 6))
          (#\7 (- 8 7))
          (#\8 (- 8 8)))
        (ecase char-0
          (#\a 0)
          (#\b 1)
          (#\c 2)
          (#\d 3)
          (#\e 4)
          (#\f 5)
          (#\g 6)
          (#\h 7))))

(defun (setf %chess-board-ref) (new-value board char-0 char-1)
  (setf (aref board
              (ecase char-1
                (#\1 (- 8 1))
                (#\2 (- 8 2))
                (#\3 (- 8 3))
                (#\4 (- 8 4))
                (#\5 (- 8 5))
                (#\6 (- 8 6))
                (#\7 (- 8 7))
                (#\8 (- 8 8)))
              (ecase char-0
                (#\a 0)
                (#\b 1)
                (#\c 2)
                (#\d 3)
                (#\e 4)
                (#\f 5)
                (#\g 6)
                (#\h 7)))
        new-value))

;;; todo: Verify that the castling is legal
(defun update-board (board best-moves)
  (declare (board board))
  (let ((move (vector-pop best-moves)))
    (if (= (length move) 4)
        (progn (setf (%chess-board-ref board (char move 2) (char move 3))
                     (%chess-board-ref board (char move 0) (char move 1))
                     (%chess-board-ref board (char move 0) (char move 1))
                     #\Null)
               ;; The four castling scenarios in regular chess
               (cond ((string= move "e1g1")
                      (setf (%chess-board-ref board #\f #\1)
                            (%chess-board-ref board #\h #\1)
                            (%chess-board-ref board #\h #\1)
                            #\Null))
                     ((string= move "e1c1")
                      (setf (%chess-board-ref board #\d #\1)
                            (%chess-board-ref board #\a #\1)
                            (%chess-board-ref board #\a #\1)
                            #\Null))
                     ((string= move "e8g8")
                      (setf (%chess-board-ref board #\f #\8)
                            (%chess-board-ref board #\h #\8)
                            (%chess-board-ref board #\h #\8)
                            #\Null))
                     ((string= move "e8c8")
                      (setf (%chess-board-ref board #\d #\8)
                            (%chess-board-ref board #\a #\8)
                            (%chess-board-ref board #\a #\8)
                            #\Null))))
        (error "Not a supported move to parse."))
    (vector-push move best-moves)
    board))

;;; Chess engine (UCI)

(defun run-command (command process-input &optional (prompt "> ") debug-stream)
  (when debug-stream
    (write-string prompt debug-stream)
    (write-string command debug-stream)
    (terpri debug-stream))
  (write-line command process-input)
  (force-output process-input))

(defun run-command* (command process-input argument-vector &optional final-argument (prompt "> ") debug-stream)
  (when debug-stream
    (write-string prompt debug-stream)
    (write-string command debug-stream))
  (write-string command process-input)
  (dotimes (i (length argument-vector))
    (when debug-stream
      (write-char #\Space debug-stream)
      (write-string (aref argument-vector i) debug-stream))
    (write-char #\Space process-input)
    (write-string (aref argument-vector i) process-input))
  (when final-argument
    (when debug-stream
      (write-char #\Space debug-stream)
      (write-string final-argument debug-stream))
    (write-char #\Space process-input)
    (write-string final-argument process-input))
  (when debug-stream
    (terpri debug-stream))
  (terpri process-input)
  (force-output process-input))

(defun quit-chess-engine (process &optional (prompt "> ") debug-stream)
  (let ((process-input (process-info-input process)))
    (run-command "quit" process-input prompt debug-stream)
    (wait-process process)))

(defun chess-engine-initialize (engine-name chess-engine-process threads &optional (prompt "> ") debug-stream)
  (let ((input (process-info-input chess-engine-process))
        (output (process-info-output chess-engine-process)))
    ;; Sends it the UCI command and handles the results, as well as
    ;; anything that was output before the UCI command was sent, if
    ;; anything.
    (run-command "uci" input prompt debug-stream)
    ;; todo: kill process if uciok is never received
    (do ((line (read-line output nil)
               (read-line output nil)))
        ((or (eql :eof line)
             (string= "uciok" line))
         (when debug-stream
           (format debug-stream "~A : ~A~%" engine-name line)))
      (when debug-stream
        (format debug-stream "~A : ~A~%" engine-name line)))
    (run-command (format nil "setoption name Threads value ~D" threads) input prompt debug-stream)
    (run-command "isready" input prompt debug-stream)
    ;; wait for a response to isready
    (let ((ready? (read-line output)))
      (when debug-stream
        (format debug-stream "~A : ~A~%" engine-name ready?)))
    (run-command "ucinewgame" input prompt debug-stream)
    (run-command "isready" input prompt debug-stream)
    ;; wait for a response to isready
    (let ((ready? (read-line output)))
      (when debug-stream
        (format debug-stream "~A : ~A~%" engine-name ready?)))))

(defun chess-engine-update-position (chess-engine-process best-moves &optional ponder-move (prompt "> ") debug-stream)
  (run-command* "position startpos moves" (process-info-input chess-engine-process) best-moves ponder-move prompt debug-stream))

(defun chess-engine-move (engine-name chess-engine-process best-moves seconds &optional (prompt "> ") debug-stream debug-info)
  (let ((chess-engine-input (process-info-input chess-engine-process))
        (chess-engine-output (process-info-output chess-engine-process)))
    (run-command (format nil "go movetime ~D000" seconds) chess-engine-input prompt debug-stream)
    (do ((line (read-line chess-engine-output nil :eof)
               (read-line chess-engine-output nil :eof)))
        ((or (eql :eof line)
             (and (>= (length line) 8) (string= "bestmove" (subseq line 0 8))))
         (when debug-stream
           (format debug-stream "~A : ~A~%" engine-name line))
         (with-input-from-string (command line :start 9)
           ;; Reads the actual best move
           (let ((ponder? t))
             (vector-push (with-output-to-string (out-string)
                            (do ((char (read-char command nil :eof) (read-char command nil :eof)))
                                ((or (eql char :eof) (char= char #\Space))
                                 (when (eql char :eof) (setf ponder? nil)))
                              (write-char char out-string)))
                          best-moves)
             ;; This assumes the next word is ponder if it exists.
             (if ponder?
                 (progn
                   ;; Assumes the rest of the line is the ponder command
                   (do ((char (read-char command) (read-char command)))
                       ((char= char #\Space)))
                   (with-output-to-string (out-string) (read-line command)))
                 nil))))
      (let ((info? (and (>= (length line) 4) (string= "info" (subseq line 0 4)))))
        (unless (or (not debug-stream)
                    (and (not debug-info) info?))
          (format debug-stream "~A : ~A~%" engine-name line))))))

(defun chess-command-ponder-start (chess-engine-process &optional (prompt "> ") debug-stream)
  (run-command "go ponder" (process-info-input chess-engine-process) prompt debug-stream))

(defun chess-engine-ponder-end (engine-name chess-engine-process success &optional (prompt "> ") debug-stream debug-info)
  (let ((chess-engine-input (process-info-input chess-engine-process))
        (chess-engine-output (process-info-output chess-engine-process)))
    (run-command (if success "ponderhit" "stop") chess-engine-input prompt debug-stream)
    ;; Note: technically, the info is generated while it's pondering
    ;; and merely read here after stop
    (do ((line (read-line chess-engine-output nil :eof)
               (read-line chess-engine-output nil :eof)))
        ((or (eql :eof line)
             (and (>= (length line) 8) (string= "bestmove" (subseq line 0 8))))
         (when debug-stream
           (format debug-stream "~A : ~A~%" engine-name line)))
      (let ((info? (and (>= (length line) 4) (string= "info" (subseq line 0 4)))))
        (unless (or (not debug-stream)
                    (and (not debug-info) info?))
          (format debug-stream "~A : ~A~%" engine-name line))))))

(defun chess-engine-leftover-output (engine-name chess-engine-process debug-stream)
  (let ((output (process-info-output chess-engine-process)))
    (do ((line (read-line output nil :eof)
               (read-line output nil :eof)))
        ((eql line :eof))
      (when debug-stream
        (format debug-stream "~A : ~A~%" engine-name line)))))

(defun chess-engine-half-turn (process-active name-active prompt-active
                               process-pondering name-pondering prompt-pondering
                               best-moves ponder-move
                               seconds
                               debug-stream
                               debug-info)
  (let ((new-ponder-move nil))
    (chess-engine-update-position process-active best-moves nil prompt-active debug-stream)
    (when ponder-move
      (chess-engine-update-position process-pondering best-moves ponder-move prompt-pondering debug-stream)
      (chess-command-ponder-start process-pondering prompt-pondering debug-stream))
    (setf new-ponder-move (chess-engine-move name-active process-active best-moves seconds prompt-active debug-stream debug-info))
    (let ((move (vector-pop best-moves)))
      (vector-push move best-moves)
      (when ponder-move
        (chess-engine-ponder-end name-pondering process-pondering (string= move ponder-move) prompt-pondering debug-stream debug-info)))
    new-ponder-move))

;;; Visuals

(progn
  (defconstant +xxd+ 0)
  (defconstant +xxl+ 1)
  (defconstant +bdd+ 2)
  (defconstant +bdl+ 3)
  (defconstant +bld+ 4)
  (defconstant +bll+ 5)
  (defconstant +kdd+ 6)
  (defconstant +kdl+ 7)
  (defconstant +kld+ 8)
  (defconstant +kll+ 9)
  (defconstant +ndd+ 10)
  (defconstant +ndl+ 11)
  (defconstant +nld+ 12)
  (defconstant +nll+ 13)
  (defconstant +pdd+ 14)
  (defconstant +pdl+ 15)
  (defconstant +pld+ 16)
  (defconstant +pll+ 17)
  (defconstant +qdd+ 18)
  (defconstant +qdl+ 19)
  (defconstant +qld+ 20)
  (defconstant +qll+ 21)
  (defconstant +rdd+ 22)
  (defconstant +rdl+ 23)
  (defconstant +rld+ 24)
  (defconstant +rll+ 25))

(define-shader (vert :vertex-shader)
    ((:in (position :vec3) :location 0)
     (:in (normal :vec3) :location 1)
     (:in (color :vec3) :location 2)
     (:in (texcoord :vec3) :location 3)
     (:uniform (view-matrix :mat4))
     (:uniform (model-matrix :mat4))
     (:uniform (projection-matrix :mat4))
     (:uniform (normal-matrix :mat4)))
    ((:out (frag-position :vec3) (:vec3 (:* model-matrix (:vec4 position 1f0))))
     (:out (frag-color :vec3) color)
     (:out (frag-texcoord :vec3) texcoord)
     (:out (gl-position :vec4) (:* projection-matrix
                                   view-matrix
                                   model-matrix
                                   (:vec4 position 1f0)))))

(define-shader (frag :fragment-shader)
    ((:in (frag-position :vec3))
     (:in (frag-color :vec3))
     (:in (frag-texcoord :vec3))
     (:uniform (loaded-texture :sampler-2d-array)))
    ((:out (out-color :vec4) (:vec4 object-color 1f0)))
  (:define (texture-color :vec3) (:vec3 (:texture loaded-texture frag-texcoord)))
  ;; (:define (object-color :vec3) (:mix frag-color texture-color 0.5f0))
  (:define (object-color :vec3) texture-color))

(define-shader-data shader-data
    ((default :vertex vert :fragment frag))
  frag
  vert)

(declaim (inline %make-square))
(defun %make-square (texture-layer)
  (let* ((*read-default-float-format* 'single-float)
         (texture-layer (coerce texture-layer 'single-float))
         (vertex-data '(-0.5 -0.5 0.0 0.0 0.0 1.0 0.5 0.5 0.5 0.0 0.0 0.0
                         0.5 -0.5 0.0 0.0 0.0 1.0 0.5 0.5 0.5 1.0 0.0 0.0
                         0.5  0.5 0.0 0.0 0.0 1.0 0.5 0.5 0.5 1.0 1.0 0.0
                        -0.5  0.5 0.0 0.0 0.0 1.0 0.5 0.5 0.5 0.0 1.0 0.0))
         (vertex-array (make-array 48
                                   :element-type 'single-float
                                   :initial-contents vertex-data)))
    (setf (aref vertex-array 11) texture-layer
          (aref vertex-array 23) texture-layer
          (aref vertex-array 35) texture-layer
          (aref vertex-array 47) texture-layer)
    (make-instance 'model
                   :vertex-array vertex-array
                   :element-array (make-array 6
                                              :element-type 'fixnum
                                              :initial-contents '(0 1 2 2 3 0))
                   :program :default
                   :texture :chess-square
                   :attribute-sizes #(3 3 3))))

(defun square-model ()
  (let ((case (readtable-case *readtable*))
        (names #("xxd" "xxl" "bdd" "bdl" "bld" "bll" "kdd" "kdl" "kld" "kll"
                 "ndd" "ndl" "nld" "nll" "pdd" "pdl" "pld" "pll" "qdd" "qdl"
                 "qld" "qll" "rdd" "rdl" "rld" "rll")))
    (loop for i from 0 below 26
       collect
         (cons (intern (concatenate 'string
                                    #.(symbol-name '#:square-)
                                    ;; (format nil "~D" i)
                                    (if (eql case :upcase)
                                        (string-upcase (elt names i))
                                        (elt names i)))
                       :keyword)
               (%make-square i)))))

(defun load-png (name)
  (load-file (merge-pathnames* (make-pathname* :directory `(:relative "png")
                                               :name name
                                               :type "png")
                               (main-data-directory "cl-chess"))
             :flip-y t
             :flatten t))

(defun textures ()
  (let ((chess-texture (make-array (* 26 64 64 3) :element-type '(unsigned-byte 8)))
        (texture-offset 0))
    (dotimes (i (* 64 64))
      (setf (aref chess-texture (+ texture-offset 0 (* i 3))) #xd1
            (aref chess-texture (+ texture-offset 1 (* i 3))) #x8b
            (aref chess-texture (+ texture-offset 2 (* i 3))) #x47))
    (incf texture-offset (* 64 64 3))
    (dotimes (i (* 64 64))
      (setf (aref chess-texture (+ texture-offset 0 (* i 3))) #xff
            (aref chess-texture (+ texture-offset 1 (* i 3))) #xce
            (aref chess-texture (+ texture-offset 2 (* i 3))) #x9e))
    (incf texture-offset (* 64 64 3))
    (dolist (file '("bdd" "bdl" "bld" "bll" "kdd" "kdl" "kld" "kll" "ndd" "ndl"
                    "nld" "nll" "pdd" "pdl" "pld" "pll" "qdd" "qdl" "qld" "qll"
                    "rdd" "rdl" "rld" "rll"))
      (let ((png (load-png file)))
        (unless (and (= 64 (height png))
                     (= 64 (width png)))
          (error "The images must be 64x64"))
        (let ((texture-data (data png)))
          (check-type texture-data (simple-array (unsigned-byte 8) (12288)))
          (dotimes (i (length texture-data))
            (setf (aref chess-texture (+ texture-offset i)) (aref texture-data i))))
        (incf texture-offset (* 64 64 3))))
    (list (make-instance 'texture
                         :name :chess-square
                         :height 64
                         :width 64
                         :depth 26
                         :dimension 3
                         :texel-size 3
                         :data chess-texture))))

(declaim (inline %make-square-entity))
(defun %make-square-entity (hud-ecs mesh-keys location scale texture-layer)
  (make-basic-entity hud-ecs
                     mesh-keys
                     texture-layer
                     :location location
                     :scale scale
                     :falling? nil))

(declaim (inline %light?))
(defun %light? (x y)
  (or (and (zerop (mod y 2)) (not (zerop (mod x 2))))
      (and (not (zerop (mod y 2))) (zerop (mod x 2)))))

(defun make-chess-graphics (&key ecs hud-ecs labels mesh-keys width height)
  (declare (ignore labels width))
  ;; Sets the board and camera
  (let* ((scale (/ height 20f0))
         (square-scale (vec scale scale 1f0)))
    (dotimes (j 8)
      (let ((y (* (- (coerce j 'single-float) 3.5f0) scale)))
        (dotimes (i 8)
          (let ((x (* (- (coerce i 'single-float) 3.5f0) scale)))
            (%make-square-entity hud-ecs mesh-keys (vec x y 0f0) square-scale
                                 (if (%light? i j)
                                     :square-xxl
                                     :square-xxd))))))
    (make-fps-camera-entity ecs :location (vec 0f0 0f0 0f0)))
  ;; Sets the pieces
  (psetf (shape hud-ecs 00) +rld+
         (shape hud-ecs 01) +nll+
         (shape hud-ecs 02) +bld+
         (shape hud-ecs 03) +qll+
         (shape hud-ecs 04) +kld+
         (shape hud-ecs 05) +bll+
         (shape hud-ecs 06) +nld+
         (shape hud-ecs 07) +rll+
         (shape hud-ecs 08) +pll+
         (shape hud-ecs 09) +pld+
         (shape hud-ecs 10) +pll+
         (shape hud-ecs 11) +pld+
         (shape hud-ecs 12) +pll+
         (shape hud-ecs 13) +pld+
         (shape hud-ecs 14) +pll+
         (shape hud-ecs 15) +pld+
         (shape hud-ecs 63) +rdd+
         (shape hud-ecs 62) +ndl+
         (shape hud-ecs 61) +bdd+
         (shape hud-ecs 60) +kdl+
         (shape hud-ecs 59) +qdd+
         (shape hud-ecs 58) +bdl+
         (shape hud-ecs 57) +ndd+
         (shape hud-ecs 56) +rdl+
         (shape hud-ecs 55) +pdl+
         (shape hud-ecs 54) +pdd+
         (shape hud-ecs 53) +pdl+
         (shape hud-ecs 52) +pdd+
         (shape hud-ecs 51) +pdl+
         (shape hud-ecs 50) +pdd+
         (shape hud-ecs 49) +pdl+
         (shape hud-ecs 48) +pdd+))

(declaim (inline %char-to-coords))
(defun %char-to-coords (char-0 char-1)
  (values (ecase char-0
            (#\a 0)
            (#\b 1)
            (#\c 2)
            (#\d 3)
            (#\e 4)
            (#\f 5)
            (#\g 6)
            (#\h 7))
          (ecase char-1
            (#\1 0)
            (#\2 1)
            (#\3 2)
            (#\4 3)
            (#\5 4)
            (#\6 5)
            (#\7 6)
            (#\8 7))))

(declaim (inline %char-to-flat-index))
(defun %char-to-flat-index (char-0 char-1)
  (flat-index 1
              8
              (ecase char-0
                (#\a 0)
                (#\b 1)
                (#\c 2)
                (#\d 3)
                (#\e 4)
                (#\f 5)
                (#\g 6)
                (#\h 7))
              (ecase char-1
                (#\1 0)
                (#\2 1)
                (#\3 2)
                (#\4 3)
                (#\5 4)
                (#\6 5)
                (#\7 6)
                (#\8 7))))

;;; todo: Verify that the castling is legal
(defun update-visual-board (hud-ecs move)
  (declare (entity-component-system hud-ecs))
  (if (= (length move) 4)
      (multiple-value-bind (start-x start-y)
          (%char-to-coords (char move 0) (char move 1))
        (multiple-value-bind (end-x end-y)
            (%char-to-coords (char move 2) (char move 3))
          (let ((start-light? (%light? start-x start-y))
                (end-light? (%light? end-x end-y))
                (starting-piece (shape hud-ecs (flat-index 1 8 start-x start-y))))
            (progn (setf (shape hud-ecs (flat-index 1 8 end-x end-y))
                         (if (or (and start-light? end-light?)
                                 (not (or start-light? end-light?)))
                             starting-piece
                             (if start-light?
                                 (1- starting-piece)
                                 (1+ starting-piece)))
                         (shape hud-ecs (flat-index 1 8 start-x start-y))
                         (if start-light? +xxl+ +xxd+))
                   ;; The four castling scenarios in regular chess
                   (cond ((string= move "e1g1")
                          (setf (shape hud-ecs (%char-to-flat-index #\f #\1))
                                +rll+
                                (shape hud-ecs (%char-to-flat-index #\h #\1))
                                +xxl+))
                         ((string= move "e1c1")
                          (setf (shape hud-ecs (%char-to-flat-index #\d #\1))
                                +rll+
                                (shape hud-ecs (%char-to-flat-index #\a #\1))
                                +xxd+))
                         ((string= move "e8g8")
                          (setf (shape hud-ecs (%char-to-flat-index #\f #\8))
                                +rdd+
                                (shape hud-ecs (%char-to-flat-index #\h #\8))
                                +xxd+))
                         ((string= move "e8c8")
                          (setf (shape hud-ecs (%char-to-flat-index #\d #\8))
                                +rdd+
                                (shape hud-ecs (%char-to-flat-index #\a #\8))
                                +xxl+)))))))
      (error "Not a supported move to parse."))
  move)

(declaim (inline %send-update-board-message))
(defun %send-update-board-message (pipe pipe-lock best-moves)
  (let ((move (vector-pop best-moves)))
    (with-lock-held (pipe-lock)
      (write-line move pipe))
    (vector-push move best-moves)))

;;; todo: Record moves in algebraic notation
;;;
;;; todo: Handle the end of the game instead of just going a certain
;;; number of turns
(defun chess-engine (&key (engine-name "stockfish") (threads 8) (seconds 10) (turns 3) debug-stream debug-info (width 1280) (height 720))
  (let* ((pipe-lock (make-lock))
         (pipe (make-instance 'character-pipe))
         (settings (make-settings :title "CL Chess"
                                  :width width
                                  :height height
                                  :fullscreen nil
                                  :app-name "cl-chess"
                                  :msaa 4
                                  :debug nil))
         (window (make-window :settings settings
                              :shader-data (shader-data)
                              :textures (textures)
                              :models (square-model)
                              :mouse-actions (mouse-actions)
                              :key-actions (key-actions)
                              :key-bindings (key-bindings)
                              :init-function #'make-chess-graphics
                              :script-function (lambda (&key ecs hud-ecs labels time)
                                                 (declare (ignore ecs labels time))
                                                 (with-lock-held (pipe-lock)
                                                   (when (not (empty? pipe))
                                                     (do ((command (read-line pipe nil :eof) (read-line pipe nil :eof)))
                                                         ((eql command :eof))
                                                       (update-visual-board hud-ecs command)))))))
         (process-1 (launch-program engine-name :input :stream :output :stream))
         (process-2 (launch-program engine-name :input :stream :output :stream))
         (engine-name-1 (concatenate 'string engine-name "-1"))
         (engine-name-2 (concatenate 'string engine-name "-2"))
         (prompt-1 "1 > ")
         (prompt-2 "2 > ")
         (board (make-board))
         ;; fixme: find a more efficient way to store moves
         (best-moves (make-array 400 :fill-pointer 0))
         (ponder-move "e2e4")
         (threads (floor (1- threads) 2)))
    (unwind-protect
         (progn
           (chess-engine-initialize engine-name-1 process-1 threads prompt-1 debug-stream)
           (chess-engine-initialize engine-name-2 process-2 threads prompt-2 debug-stream)
           (dotimes (i turns)
             (setf ponder-move (chess-engine-half-turn process-1 engine-name-1 prompt-1
                                                       process-2 engine-name-2 prompt-2
                                                       best-moves ponder-move
                                                       seconds
                                                       debug-stream
                                                       debug-info))
             (update-board board best-moves)
             (%send-update-board-message pipe pipe-lock best-moves)
             (setf ponder-move (chess-engine-half-turn process-2 engine-name-2 prompt-2
                                                       process-1 engine-name-1 prompt-1
                                                       best-moves ponder-move
                                                       seconds
                                                       debug-stream
                                                       debug-info))
             (update-board board best-moves)
             (%send-update-board-message pipe pipe-lock best-moves))
           (values window best-moves))
      ;; Quits the chess engine.
      (quit-chess-engine process-1 prompt-1 debug-stream)
      (chess-engine-leftover-output engine-name-1 process-1 debug-stream)
      (quit-chess-engine process-2 prompt-2 debug-stream)
      (chess-engine-leftover-output engine-name-2 process-2 debug-stream))))
