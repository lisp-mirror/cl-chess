;;; todo: bit board

(defpackage #:chess-engine
  (:use #:cl #:zombie-raptor)
  (:import-from #:bordeaux-threads
                #:make-lock
                #:with-lock-held)
  (:import-from #:pngload
                #:data
                #:height
                #:load-file
                #:width)
  (:import-from #:static-vectors
                #:make-static-vector)
  (:import-from #:uiop
                #:launch-program
                #:make-pathname*
                #:merge-pathnames*
                #:process-info-input
                #:process-info-output
                #:wait-process)
  (:export #:chess-engine
           #:chess-game-replay))

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

(define-function print-board ((board board) &optional unicode (stream t))
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
(define-function print-fen-board ((board board) &optional (stream t))
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

(define-function (%chess-board-ref :inline t) (board char-0 char-1)
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

(define-function ((setf %chess-board-ref) :inline t) (new-value board char-0 char-1)
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
(define-function update-board ((board board) move)
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
      (error "Not a supported move: ~A" move))
  board)

;;; Chess engine (UCI)

(defun run-command (command process-input &optional (prompt "> ") debug-stream)
  (when debug-stream
    (write-string prompt debug-stream)
    (write-string command debug-stream)
    (terpri debug-stream))
  (write-line command process-input)
  (force-output process-input))

(defun run-command* (command process-input &key (prompt "> ") debug-stream end)
  (when debug-stream
    (write-string prompt debug-stream)
    (write-string command debug-stream :end end)
    (terpri debug-stream))
  (write-line command process-input :end end)
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

(defun chess-engine-update-position (chess-engine-process position-string &key ponder-move (prompt "> ") debug-stream end)
  (declare ((or null (simple-array character (4))) ponder-move))
  (when ponder-move
    (setf (char position-string end) #\Space)
    (dotimes (i 4)
      (setf (char position-string (1+ (+ end i))) (char ponder-move i))))
  (run-command* position-string (process-info-input chess-engine-process) :end (if ponder-move (+ end 5) end) :prompt prompt :debug-stream debug-stream)
  (when ponder-move
    (dotimes (i 5)
      (setf (char position-string (+ end i)) #\Null))))

;;; todo: what is move when the move is a promotion? is it length 5?
(defun chess-engine-move (engine-name chess-engine-process seconds &optional (prompt "> ") debug-stream debug-info)
  (let ((chess-engine-input (process-info-input chess-engine-process))
        (chess-engine-output (process-info-output chess-engine-process)))
    (run-command (format nil "go movetime ~D000" seconds) chess-engine-input prompt debug-stream)
    (do ((line (read-line chess-engine-output nil :eof)
               (read-line chess-engine-output nil :eof))
         (checkmate? nil))
        ((or (eql :eof line)
             checkmate?
             (and (>= (length line) 8) (string= "bestmove" line :start2 0 :end2 8)))
         (if checkmate?
             (values "CHECKMATE" nil)
             (let ((ponder? (position #\Space line :start 9)))
               (when debug-stream
                 (format debug-stream "~A : ~A~%" engine-name line))
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
             (and (>= (length line) 8) (string= "bestmove" line :start2 0 :end2 8)))
         (when debug-stream
           (format debug-stream "~A : ~A~%" engine-name line)))
      (let ((info? (and (>= (length line) 4) (string= "info" line :start2 0 :end2 4))))
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
                               position-string position-string-position
                               ponder-move
                               seconds
                               debug-stream
                               debug-info)
  (declare ((or null (simple-array character (4))) ponder-move))
  (let ((move nil)
        (new-ponder-move nil))
    (chess-engine-update-position process-active position-string :prompt prompt-active :debug-stream debug-stream :end position-string-position)
    (when ponder-move
      (chess-engine-update-position process-pondering position-string :ponder-move ponder-move :prompt prompt-pondering :debug-stream debug-stream :end position-string-position)
      (chess-command-ponder-start process-pondering prompt-pondering debug-stream))
    (setf (values move new-ponder-move)
          (chess-engine-move name-active process-active seconds prompt-active debug-stream debug-info))
    (check-type move (simple-array character (4)))
    (check-type new-ponder-move (or null (simple-array character (4))) ponder-move)
    (setf (char position-string position-string-position) #\Space)
    (dotimes (i 4)
      (setf (char position-string (1+ (+ position-string-position i))) (char move i)))
    (when ponder-move
      (chess-engine-ponder-end name-pondering process-pondering (string= move ponder-move) prompt-pondering debug-stream debug-info))
    (values move new-ponder-move (string= move "CHECKMATE"))))

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
     (:uniform (projection-matrix :mat4)))
    ((:out (frag-position :vec3) (:vec3 (:* model-matrix (:vec4 position 1f0))))
     (:out (frag-color :vec3) color)
     (:out (frag-texcoord :vec2) (:st texcoord))
     (:out (frag-layer :float :flat) (:p texcoord))
     (:out (gl-position :vec4) (:* projection-matrix
                                   view-matrix
                                   model-matrix
                                   (:vec4 position 1f0)))))

(define-shader (frag :fragment-shader)
    ((:in (frag-position :vec3))
     (:in (frag-color :vec3))
     (:in (frag-texcoord :vec2))
     (:in (frag-layer :float :flat))
     (:uniform (loaded-texture :sampler-2D-array)))
    ((:out (out-color :vec4) (:vec4 object-color 1f0)))
  (:define (texture-color :vec3) (:vec3 (:texture loaded-texture (:vec3 frag-texcoord frag-layer))))
  (:define (object-color :vec3) texture-color))

(define-shader-data shader-data*
    ((default :vertex vert :fragment frag))
  frag
  vert)

(define-function (%make-square :inline t) (texture-layer &key name)
  (let* ((texture-layer (float* texture-layer))
         (vertex-data '(-0.5f0 -0.5f0 0.0f0 0.0f0 0.0f0 1.0f0 0.0f0 0.0f0 0.0f0 0.0f0 0.0f0 0.0f0
                        0.5f0 -0.5f0 0.0f0 0.0f0 0.0f0 1.0f0 0.0f0 0.0f0 0.0f0 1.0f0 0.0f0 0.0f0
                        0.5f0 0.5f0 0.0f0 0.0f0 0.0f0 1.0f0 0.0f0 0.0f0 0.0f0 1.0f0 1.0f0 0.0f0
                        -0.5f0 0.5f0 0.0f0 0.0f0 0.0f0 1.0f0 0.0f0 0.0f0 0.0f0 0.0f0 1.0f0 0.0f0))
         (vertex-array (make-static-vector 48
                                           :element-type 'single-float
                                           :initial-contents vertex-data)))
    (declare ((simple-array single-float (48)) vertex-array))
    (setf (aref vertex-array 11) texture-layer
          (aref vertex-array 23) texture-layer
          (aref vertex-array 35) texture-layer
          (aref vertex-array 47) texture-layer)
    (make-instance 'model
                   :name name
                   :vertex-array vertex-array
                   :element-array (make-static-vector 6
                                                      :element-type '(unsigned-byte 16)
                                                      :initial-contents '(0 1 2 2 3 0))
                   :program :default
                   :texture :chess-square
                   :attribute-sizes #(3 3 3))))

(defun square-model ()
  (let ((case (readtable-case *readtable*))
        (names #("xxd" "xxl" "bdd" "bdl" "bld" "bll" "kdd" "kdl" "kld" "kll"
                 "ndd" "ndl" "nld" "nll" "pdd" "pdl" "pld" "pll" "qdd" "qdl"
                 "qld" "qll" "rdd" "rdl" "rld" "rll")))
    (loop :for name :across names
          :for i :of-type fixnum := 0 :then (1+ i)
          :collect
          (%make-square i :name (intern (concatenate 'string
                                                     #.(symbol-name '#:square-)
                                                     (if (eql case :upcase)
                                                         (string-upcase name)
                                                         name))
                                        :keyword)))))

(defun load-png (name)
  (load-file (merge-pathnames* (make-pathname* :directory `(:relative "png")
                                               :name name
                                               :type "png")
                               (main-data-directory "cl-chess"))
             :flip-y t
             :flatten t))

(defun textures ()
  (with-texture-data-make-texture (chess-texture :chess-square)
      (3 3 64 :depth 26 :initial-element 255)
    (let ((texture-offset 0))
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
          (incf texture-offset (* 64 64 3)))))))

(define-function (%make-square-entity :inline t) (hud-ecs mesh-keys location scale texture-layer)
  (make-basic-entity hud-ecs
                     mesh-keys
                     texture-layer
                     :location location
                     :scale scale
                     :falling? nil))

(define-function (%light? :inline t) (x y)
  (or (and (zerop (mod y 2)) (not (zerop (mod x 2))))
      (and (not (zerop (mod y 2))) (zerop (mod x 2)))))

(defun make-chess-graphics (&key ecs hud-ecs labels mesh-keys width height)
  (declare (ignore labels width))
  ;; Sets the board and camera
  (let* ((scale (min (/ height 10f0) 64f0))
         (square-scale (vec scale scale 1f0)))
    (dotimes (j 8)
      (let ((y (* (- (float* j) 3.5f0) scale)))
        (dotimes (i 8)
          (let ((x (* (- (float* i) 3.5f0) scale)))
            (%make-square-entity hud-ecs mesh-keys (vec x y 0f0) square-scale
                                 (if (%light? i j)
                                     :square-xxl
                                     :square-xxd))))))
    (make-fps-camera-entity ecs :location (vec 0f0 0f0 0f0)))
  ;; Sets the pieces
  (map nil
       (lambda (entity-id shape)
         (with-selection hud-ecs (id :id entity-id :changed? t)
             ((geometry (mesh-id mesh-id)))
           (setf mesh-id shape)))
       '#.(make-array 32
                      :element-type 'fixnum
                      :initial-contents (list 00 01 02 03 04
                                              05 06 07 08 09
                                              10 11 12 13 14
                                              15 63 62 61 60
                                              59 58 57 56 55
                                              54 53 52 51 50
                                              49 48))
       '#.(make-array 32
                      :element-type 'fixnum
                      :initial-contents (list +rld+ +nll+ +bld+
                                              +qll+ +kld+ +bll+
                                              +nld+ +rll+ +pll+
                                              +pld+ +pll+ +pld+
                                              +pll+ +pld+ +pll+
                                              +pld+ +rdd+ +ndl+
                                              +bdd+ +kdl+ +qdd+
                                              +bdl+ +ndd+ +rdl+
                                              +pdl+ +pdd+ +pdl+
                                              +pdd+ +pdl+ +pdd+
                                              +pdl+ +pdd+))))

(define-function (%char-to-coords :inline t) (char-0 char-1)
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

(define-function (%char-to-flat-index :inline t) (char-0 char-1)
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

(define-function (shape :inline t) (ecs entity-id)
  (with-selection ecs (id :id entity-id)
      ((geometry (mesh-id mesh-id)))
    mesh-id))

(define-function ((setf shape) :inline t) (new-shape ecs entity-id)
  (with-selection ecs (id :id entity-id :changed? t)
      ((geometry (mesh-id mesh-id)))
    (setf mesh-id new-shape)))

;;; todo: Verify that the castling is legal
(define-function update-visual-board ((hud-ecs entity-component-system) move)
  (declare (optimize (speed 3)))
  (check-type move (unsigned-byte 16))
  (let* ((start-x (ldb (byte 3 0) move))
         (start-y (ldb (byte 3 3) move))
         (end-x   (ldb (byte 3 6) move))
         (end-y   (ldb (byte 3 9) move))
         (start-light? (%light? start-x start-y))
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
           (cond ((= move #o0604) ; e1g1
                  (setf (shape hud-ecs (%char-to-flat-index #\f #\1))
                        +rll+
                        (shape hud-ecs (%char-to-flat-index #\h #\1))
                        +xxl+))
                 ((= move #o0204) ; e1c1
                  (setf (shape hud-ecs (%char-to-flat-index #\d #\1))
                        +rll+
                        (shape hud-ecs (%char-to-flat-index #\a #\1))
                        +xxd+))
                 ((= move #o7674) ; e8g8
                  (setf (shape hud-ecs (%char-to-flat-index #\f #\8))
                        +rdd+
                        (shape hud-ecs (%char-to-flat-index #\h #\8))
                        +xxd+))
                 ((= move #o7274) ; e8c8
                  (setf (shape hud-ecs (%char-to-flat-index #\d #\8))
                        +rdd+
                        (shape hud-ecs (%char-to-flat-index #\a #\8))
                        +xxl+)))))
  move)

(defun update-visual-board* (hud-ecs move)
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

(define-function (make-chess-gui :inline t) (width height script-function &key fullscreen)
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
               :init-function #'make-chess-graphics
               :script-function script-function)))

(define-function (split-ub16 :inline t) ((x uint16))
  (values (ldb (byte 8 0) x) (ldb (byte 8 8) x)))

(define-function (join-ub16 :inline t) ((x uint8) (y uint8))
  (+ x (ash y 8)))

(defun command-chars-to-command-ub16 (char-0 char-1 char-2 char-3)
  (declare (optimize (speed 3)))
  (multiple-value-bind (start-0 start-1)
      (%char-to-coords char-0 char-1)
    (multiple-value-bind (end-0 end-1)
        (%char-to-coords char-2 char-3)
      (+ start-0 (ash start-1 3) (ash end-0 6) (ash end-1 9)))))

(define-function (chess-game-replay :check-type t)
    ((moves vector)
     &key
     (seconds 2 (integer 0))
     (debug-stream nil (or boolean stream))
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
                                  (update-visual-board* hud-ecs move)
                                  (incf i))))))
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
  (let* ((pipe-lock (make-lock))
         (pipe (make-instance 'byte-pipe))
         (script-function (lambda (&key hud-ecs &allow-other-keys)
                            (with-lock-held (pipe-lock)
                              (do ((empty? (empty? pipe) (empty? pipe))
                                   (command-0 (read-byte pipe) (read-byte pipe))
                                   (command-1 (read-byte pipe) (read-byte pipe)))
                                  (empty?)
                                (update-visual-board hud-ecs (join-ub16 command-0 command-1))))))
         (window (make-chess-gui width height script-function))
         (mirror-match? (string= engine-name-1 engine-name-2))
         (process-1 (launch-program engine-name-1 :input :stream :output :stream))
         (process-2 (launch-program engine-name-2 :input :stream :output :stream))
         (engine-name-1 (if mirror-match? (concatenate 'string engine-name-1 "-1") engine-name-1))
         (engine-name-2 (if mirror-match? (concatenate 'string engine-name-2 "-2") engine-name-2))
         (prompt-1 "1 > ")
         (prompt-2 "2 > ")
         (board (make-board))
         (moves (make-array 400 :fill-pointer 0))
         (position-string (let* ((position-string (make-array (+ 23 (* 400 5)) :element-type 'character))
                                 (position-string-start "position startpos moves"))
                            (dotimes (i (length position-string-start))
                              (setf (char position-string i) (char position-string-start i)))
                            position-string))
         (position-string-position 23)
         (threads (floor (1- threads) 2)))
    (unwind-protect
         (progn
           (chess-engine-initialize engine-name-1 process-1 threads prompt-1 debug-stream)
           (chess-engine-initialize engine-name-2 process-2 threads prompt-2 debug-stream)
           (values window
                   moves
                   (do ((i 0 (1+ i))
                        (move nil)
                        (ponder-move "e2e4")
                        (position-string-position position-string-position (+ 10 position-string-position))
                        (checkmate? nil))
                       ((or (= i turns) checkmate?)
                        (if checkmate? "Checkmate!" "Out of turns!"))
                     (setf (values move ponder-move checkmate?)
                           (chess-engine-half-turn process-1 engine-name-1 prompt-1
                                                   process-2 engine-name-2 prompt-2
                                                   position-string position-string-position
                                                   ponder-move
                                                   seconds
                                                   debug-stream
                                                   debug-info))
                     (vector-push move moves)
                     (unless checkmate?
                       (update-board board move)
                       (let ((move* (command-chars-to-command-ub16 (char move 0)
                                                                   (char move 1)
                                                                   (char move 2)
                                                                   (char move 3))))
                         (multiple-value-bind (byte-0 byte-1) (split-ub16 move*)
                           (with-lock-held (pipe-lock)
                             (write-byte byte-0 pipe)
                             (write-byte byte-1 pipe))))
                       (setf (values move ponder-move checkmate?)
                             (chess-engine-half-turn process-2 engine-name-2 prompt-2
                                                     process-1 engine-name-1 prompt-1
                                                     position-string (+ 5 position-string-position)
                                                     ponder-move
                                                     seconds
                                                     debug-stream
                                                     debug-info))
                       (vector-push move moves)
                       (unless checkmate?
                         (update-board board move)
                         (let ((move* (command-chars-to-command-ub16 (char move 0)
                                                                     (char move 1)
                                                                     (char move 2)
                                                                     (char move 3))))
                           (multiple-value-bind (byte-0 byte-1) (split-ub16 move*)
                             (with-lock-held (pipe-lock)
                               (write-byte byte-0 pipe)
                               (write-byte byte-1 pipe)))))))))
      ;; Quits the chess engine.
      (quit-chess-engine process-1 prompt-1 debug-stream)
      (chess-engine-leftover-output engine-name-1 process-1 debug-stream)
      (quit-chess-engine process-2 prompt-2 debug-stream)
      (chess-engine-leftover-output engine-name-2 process-2 debug-stream))))
