(defpackage #:cl-chess/uci
  (:use #:cl
        #:zombie-raptor)
  (:import-from #:uiop
                #:launch-program
                #:process-alive-p
                #:process-info-input
                #:process-info-output
                #:wait-process)
  (:import-from #:uiop/launch-program
                #:process-info)
  (:export #:half-turn
           #:initialize-chess-engines
           #:make-chess-engine
           #:make-chess-engine-pair
           #:make-move
           #:make-position-string-and-position
           #:move
           #:null-move?
           #:print-chess-engine-output
           #:quit-chess-engines))

(in-package #:cl-chess/uci)

;;; Moves

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

;;; Position string

(defconstant +position-string-prefix-length+ (length "position startpos moves"))
(defconstant +position-string-length+ (+ +position-string-prefix-length+
                                         +possible-promotions+
                                         (* 400 +move-length+)))

(deftype position-string ()
  `(simple-string ,+position-string-length+))

(define-function (make-position-string :inline t) ()
  (replace (make-string +position-string-length+ :initial-element #\Nul)
           "position startpos moves"))

(defstruct position-string-and-position
  (string (make-position-string) :type position-string)
  (position +position-string-prefix-length+ :type fixnum))

(define-accessor-macro with-position-string-and-position #.(symbol-name '#:position-string-and-position-))

;;; Chess engine

(defstruct (chess-engine (:constructor %make-chess-engine))
  (process    nil :type process-info :read-only t)
  (input      nil :type stream       :read-only t)
  (output     nil :type stream       :read-only t)
  (debug      nil :type (maybe stream))
  (debug-info nil :type boolean)
  (name       nil :type string       :read-only t)
  (prompt     nil :type string       :read-only t))

(define-function (make-chess-engine :inline t) (&key process name prompt debug-stream debug-info)
  (%make-chess-engine :process process
                      :input (process-info-input process)
                      :output (process-info-output process)
                      :debug debug-stream
                      :debug-info debug-info
                      :name name
                      :prompt prompt))

(define-accessor-macro with-chess-engine #.(symbol-name '#:chess-engine-))

(define-function (print-chess-engine-output :inline t) (name line debug-stream)
  (when debug-stream
    (format debug-stream "~A : ~A~%" name line)))

;;; UCI

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

(define-function initialize-chess-engines ((chess-engine-1 chess-engine) (chess-engine-2 chess-engine) threads)
  (initialize-chess-engine chess-engine-1 threads)
  (initialize-chess-engine chess-engine-2 threads))

(define-function quit-chess-engine ((chess-engine chess-engine))
  (with-chess-engine (process input prompt debug)
      chess-engine
    (run-command "quit" input prompt debug)
    (wait-process process)
    (chess-engine-leftover-output chess-engine)))

(define-function quit-chess-engines ((chess-engine-1 chess-engine) (chess-engine-2 chess-engine))
  (when (process-alive-p (chess-engine-process chess-engine-1))
    (quit-chess-engine chess-engine-1))
  (when (process-alive-p (chess-engine-process chess-engine-2))
    (quit-chess-engine chess-engine-2)))

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

(defmacro do-space-separated-line ((line word start-position end-position) &body body)
  `(loop :for ,start-position :of-type (maybe fixnum) := 0
           :then (if ,end-position (1+ ,end-position) nil)
         :for ,end-position :of-type (maybe fixnum)
           := (if (null ,start-position)
                  nil
                  (position #\Space ,line :start ,start-position))
         :for ,word :of-type fixnum :from 0
         :until (null ,start-position)
         :do
            (progn ,@body)))

(defun chess-engine-move (chess-engine seconds)
  (with-chess-engine (input output name prompt debug debug-info)
      chess-engine
    (go-move seconds input prompt debug)
    (do ((line (read-line output nil :eof)
               (unless (or best-move? checkmate? (eql :eof line))
                 (read-line output nil :eof)))
         (line-type nil nil)
         (checkmate? nil)
         (best-move? nil)
         (ponder? nil))
        ((or best-move? checkmate? (eql :eof line))
         (if checkmate?
             (values :checkmate nil)
             (values best-move? ponder?)))
      (declare ((or boolean move) best-move? ponder?)
               ((maybe keyword) line-type checkmate?))
      (let ((mate? nil))
        (do-space-separated-line (line word start end)
          (if (zerop word)
              (cond ((string= line "info" :start1 start :end1 end)
                     (setf line-type :info))
                    ((string= line "bestmove" :start1 start :end1 end)
                     (setf line-type :best-move
                           best-move? t)))
              (case line-type
                (:best-move
                 (case= word
                   (1 (setf best-move? (replace (make-move) line :start2 start :end2 end)))
                   (2 (setf ponder? (string= line "ponder" :start1 start :end1 end)))
                   (3 (when ponder? (setf ponder? (replace (make-move) line :start2 start :end2 end))))))
                (:info
                 (if mate?
                     (when (<= (parse-integer line :start start :end end) 0)
                       (setf checkmate? t
                             mate? nil))
                     (setf mate? (string= line "mate" :start1 start :end1 end))))))))
      (when (or (eql t best-move?) (eql t ponder?))
        (error "Syntax error in UCI line: ~A~%" line))
      (unless (and (not debug-info) (eql :info line-type))
        (print-chess-engine-output name line debug)))))

(define-function half-turn ((engine-active chess-engine)
                            (move move)
                            (position-string position-string-and-position)
                            seconds)
  (with-position-string-and-position (string position) position-string
    (update-position engine-active string nil position)
    (multiple-value-bind (move* ponder) (chess-engine-move engine-active seconds)
      (declare ((or move (eql :checkmate)) move*)
               ((maybe move) ponder))
      (declare (ignore ponder))
      (unless (eql move* :checkmate)
        (replace move move*))
      (setf (char string position) #\Space)
      (replace string move :start1 (1+ position))
      (incf position
            (if (promotion? move) (1+ +move-length+) +move-length+))
      (eql move* :checkmate))))

(define-function (make-chess-engine-pair :inline t) (engine-name-1 engine-name-2 debug-stream debug-info)
  (let ((mirror-match? (string= engine-name-1 engine-name-2)))
    (values (make-chess-engine :process (launch-program engine-name-1 :input :stream :output :stream)
                               :name (if mirror-match? (concatenate 'string engine-name-1 "-1") engine-name-1)
                               :prompt "1 > "
                               :debug-stream debug-stream
                               :debug-info debug-info)
            (make-chess-engine :process (launch-program engine-name-2 :input :stream :output :stream)
                               :name (if mirror-match? (concatenate 'string engine-name-2 "-2") engine-name-2)
                               :prompt "2 > "
                               :debug-stream debug-stream
                               :debug-info debug-info))))
