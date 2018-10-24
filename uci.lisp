(defpackage #:cl-chess/uci
  (:use #:cl
        #:zombie-raptor)
  (:import-from #:uiop
                #:launch-program
                #:process-alive-p
                #:process-info-input
                #:process-info-output
                #:terminate-process
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
  (debug-info   2 :type (integer 0 3))
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

(defmacro do-read-char ((char stream &key eof no-hang end-var) &body body)
  (let* ((read-char (if no-hang 'read-char-no-hang 'read-char))
         (end-condition `(eql ,eof ,char))
         (end-condition (if no-hang `(or (null ,char) ,end-condition) end-condition))
         (end-condition (if end-var `(and ,end-var ,end-condition) end-condition)))
    `(do ((,char (,read-char ,stream nil ,eof)
                 (,read-char ,stream nil ,eof)))
         (,end-condition)
       ,@body)))

(defmacro do-space-separated-line ((line word start-position end-position &optional end-var)
                                   &body body)
  (let* ((end-condition `(null ,start-position))
         (end-condition (if end-var `(or ,end-condition ,end-var) end-condition)))
    `(loop ,@(if end-var `(:with ,end-var := nil) nil)
           :for ,start-position :of-type (maybe fixnum) := 0
             :then (if ,end-position (1+ ,end-position) nil)
           :for ,end-position :of-type (maybe fixnum)
             := (if (null ,start-position)
                    nil
                    (position #\Space ,line :start ,start-position))
           :for ,word :of-type fixnum :from 0
           :until ,end-condition
           :do
              (progn ,@body))))

(defun run-command (command process-input &optional (prompt "> ") debug-stream end)
  (when debug-stream
    (write-string prompt debug-stream)
    (write-string command debug-stream :end end)
    (terpri debug-stream))
  (write-line command process-input :end end)
  (force-output process-input))

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

(defstruct uci-option
  (name    nil :type (maybe string))
  (type    nil :type (maybe string))
  (default nil :type (maybe string))
  (min     nil :type (maybe string))
  (max     nil :type (maybe string))
  ;; fixme: multiple vars are allowed
  (var     nil :type (maybe string)))

(define-function initialize-uci ((chess-engine chess-engine))
  (with-chess-engine (process input output name prompt debug debug-info)
      chess-engine
    (run-command "uci" input prompt debug)
    (let ((id-name nil)
          (id-author nil))
      (loop :for line :of-type string := (do ((output? (listen output) (listen output))
                                              (time 0))
                                             (output? (read-line output))
                                           (sleep 1/1000)
                                           (incf time)
                                           (when (> time 5000)
                                             (terminate-process process)
                                             (error "Process ~A did not respond with \"uciok\"." name)))
            :for line-type    := nil
            :for line-subtype := nil
            :for name-start   := nil
            :for name-end     := nil
            :for uci-option   := nil
            :until (string= "uciok" line)
            :do
               (do-space-separated-line (line word start end done?)
                 (if (zerop word)
                     (setf line-type
                           (cond ((string= line "id" :start1 start :end1 end) :id)
                                 ((string= line "option" :start1 start :end1 end) :option)))
                     (case line-type
                       (:id
                        (case= word
                          (1 (cond ((string= line "name" :start1 start :end1 end)
                                    (setf line-subtype :name))
                                   ((string= line "author" :start1 start :end1 end)
                                    (setf line-subtype :author))
                                   (t (error "Syntax error in UCI line: ~A~%" line))))
                          (2 (case line-subtype
                               (:name (setf id-name (subseq line start)))
                               (:author (setf id-author (subseq line start))))
                             (setf done? t))))
                       (:option
                        (case= word
                          (1 (if (string= line "name" :start1 start :end1 end)
                                 (setf uci-option (make-uci-option))
                                 (error "Syntax error in UCI line: ~A~%" line)))
                          (2 (setf name-start start
                                   name-end end
                                   line-subtype :name))
                          (t (case line-subtype
                               (:name
                                (if (string= line "type" :start1 start :end1 end)
                                    (psetf (uci-option-name uci-option) (subseq line name-start name-end)
                                           line-subtype :type)
                                    (setf name-end end)))
                               (:type
                                (psetf (uci-option-type uci-option) (subseq line start end)
                                       line-subtype :parameter))
                               (:default
                                (psetf (uci-option-default uci-option) (subseq line start end)
                                       line-subtype :parameter))
                               (:min
                                (psetf (uci-option-min uci-option) (subseq line start end)
                                       line-subtype :parameter))
                               (:max
                                (psetf (uci-option-max uci-option) (subseq line start end)
                                       line-subtype :parameter))
                               ;; fixme: multiple vars are allowed
                               (:var
                                (psetf (uci-option-var uci-option) (subseq line start end)
                                       line-subtype :parameter))
                               (:parameter (setf line-subtype
                                                 (cond ((string= line "default" :start1 start :end1 end) :default)
                                                       ((string= line "min" :start1 start :end1 end) :min)
                                                       ((string= line "max" :start1 start :end1 end) :max)
                                                       ((string= line "var" :start1 start :end1 end) :var)))))))))))
               (unless (< debug-info 2)
                 (print-chess-engine-output name line debug))
            :when uci-option
              :collect uci-option
            :finally
               (unless id-name
                 (error "UCI error: id name is required"))
               (unless id-author
                 (error "UCI error: id author is required"))
               (print-chess-engine-output "DEBUG" (format nil "~A is ~A by ~A" name id-name id-author) debug)
               (print-chess-engine-output name line debug)))))

(define-function (set-option :inline t) (name value input prompt debug-stream)
  (run-command (format nil "setoption name ~A~@[ value ~A~]" name value) input prompt debug-stream))

(define-function ready? (name input output prompt debug process)
  "
Tries for approximately 5 seconds to receive the line \"readyok\" in
response to the command \"isready\".
"
  (run-command "isready" input prompt debug)
  (let ((i 0)
        (in-ready-ok? t)
        (ready-ok? nil)
        (start-time (get-internal-real-time)))
    (do-read-char (char output :no-hang t :end-var ready-ok?)
      (when (and debug char (zerop i)) (format debug "~A : " name))
      (cond ((null char)
             (sleep 1/1000))
            ((char= char #\Newline)
             (when debug (write-char char debug))
             (if (and in-ready-ok? (= 7 i))
                 (setf ready-ok? t)
                 (setf i 0)))
            (t
             (when debug (write-char char debug))
             (setf in-ready-ok?
                   (case char
                     (#\r (if (and in-ready-ok? (zerop i)) t nil))
                     (#\e (if (and in-ready-ok? (= 1 i)) t nil))
                     (#\a (if (and in-ready-ok? (= 2 i)) t nil))
                     (#\d (if (and in-ready-ok? (= 3 i)) t nil))
                     (#\y (if (and in-ready-ok? (= 4 i)) t nil))
                     (#\o (if (and in-ready-ok? (= 5 i)) t nil))
                     (#\k (if (and in-ready-ok? (= 6 i)) t nil))))
             (incf i)))
      (when (and (not ready-ok?)
                 (> (- (get-internal-real-time) start-time)
                    (* 5 internal-time-units-per-second)))
        (terminate-process process)
        (error "Process ~A did not respond with \"readyok\"." name)))))

(define-function (new-game :inline t) (name input output prompt debug-stream process)
  (run-command "ucinewgame" input prompt debug-stream)
  (ready? name input output prompt debug-stream process))

(define-function go-move (chess-engine
                          &key
                          (ponder? nil boolean)
                          (w-time nil (maybe (integer 0 *)))
                          (b-time nil (maybe (integer 0 *)))
                          (w-inc nil (maybe (integer 1 *)))
                          (b-inc nil (maybe (integer 1 *)))
                          (moves-to-go nil (maybe (integer 0 *)))
                          (depth nil (maybe (integer 1 *)))
                          (nodes nil (maybe (integer 1 *)))
                          (mate nil (maybe (integer 0 *)))
                          (move-time nil (maybe (integer 1 *)))
                          (infinite? nil boolean))
  "
Send the UCI move command to the chess engine. All time-related
commands are measured in milliseconds.

ponder? tells the engine to enter pondering mode if true.

w-time and b-time tell the chess engine how much time is left on the
clock for white and black respectively and w-inc and b-inc give the
white and black increments of added time per move. moves-to-go tells
the chess engine how many moves are remaining until the next time
control.

depth and nodes specifies how deep and how many nodes to search.

mate tells the engine to look for a mate in that many moves.

move-time tells the engine to take exactly that long.

infinite? tells the engine to wait until `stop' if true.

Note: search-moves has not yet been implemented, but might be
implemented in the future.
"
  (let ((moves-to-go (if (and moves-to-go (zerop moves-to-go))
                         nil
                         moves-to-go)))
    (with-chess-engine (input prompt debug)
        chess-engine
      (run-command (format nil
                           #.(concatenate 'string
                                          "go~:[~; ponder~]"
                                          "~@[ wtime ~D~]~@[ btime ~D~]~@[ winc ~D~]~@[ b-inc ~D~]~@[ movestogo ~D~]"
                                          "~@[ depth ~D~]~@[ nodes ~D~]~@[ mate ~D~]"
                                          "~@[ movetime ~D~]~:[~; infinite~]")
                           ponder?
                           w-time
                           b-time
                           w-inc
                           b-inc
                           moves-to-go
                           depth
                           nodes
                           mate
                           move-time
                           infinite?)
                   input
                   prompt
                   debug))))

(define-function (ponder-hit :inline t) (input prompt debug-stream)
  (run-command "ponderhit" input prompt debug-stream))

(define-function (stop :inline t) (input prompt debug-stream)
  (run-command "stop" input prompt debug-stream))

(defun initialize-chess-engine (chess-engine threads)
  (with-chess-engine (process input output name prompt debug)
      chess-engine
    (read-opening-message chess-engine)
    (let* ((options (initialize-uci chess-engine))
           (threads-option (find-if (lambda (uci-option)
                                      (string= (uci-option-name uci-option) "Threads"))
                                    options)))
      (if threads-option
          (let* ((min (parse-integer (uci-option-min threads-option)))
                 (max (parse-integer (uci-option-max threads-option)))
                 (threads* (min (max threads min) max)))
            (unless (= threads threads*)
              (print-chess-engine-output "DEBUG"
                                         (format nil
                                                 "Note: Requested ~D threads, but the value has to be ~D to stay in range."
                                                 threads
                                                 threads*)
                                         debug))
            (set-option "Threads" threads* input prompt debug))
          (print-chess-engine-output "DEBUG" "Note: Threads cannot be customized." debug)))
    (ready? name input output prompt debug process)
    (new-game name input output prompt debug process)))

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

(define-function (parse-move-line :inline t) ((line string) (move move) (ponder move))
  (let ((best-move? nil)
        (ponder? nil)
        (line-type nil)
        (checkmate? nil)
        (mate? nil))
    (declare ((or boolean move) best-move? ponder?)
             ((maybe keyword) line-type checkmate?)
             (boolean mate?))
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
               (1 (setf best-move? (replace move line :start2 start :end2 end)))
               (2 (setf ponder? (string= line "ponder" :start1 start :end1 end)))
               (3 (when ponder? (setf ponder? (replace ponder line :start2 start :end2 end))))))
            (:info
             (if mate?
                 (when (<= (parse-integer line :start start :end end) 0)
                   (setf checkmate? :checkmate
                         mate? nil))
                 (setf mate? (string= line "mate" :start1 start :end1 end)))))))
    (when (or (eql t best-move?) (eql t ponder?))
      (error "Syntax error in UCI line: ~A~%" line))
    (unless ponder?
      (replace ponder #.(make-move)))
    (values (or checkmate? best-move?) line-type)))

(define-function chess-engine-move ((chess-engine chess-engine)
                                    (seconds (integer 0 *))
                                    (move move)
                                    (ponder move))
  (with-chess-engine (input output name prompt debug debug-info)
      chess-engine
    (go-move chess-engine :move-time (* seconds 1000))
    (loop :for line  :of-type string := (read-line output)
          :for done? :of-type (or (maybe move) (eql :checkmate))
            := (multiple-value-bind (done? line-type)
                   (parse-move-line line move ponder)
                 (unless (and (< debug-info 3) (eql :info line-type))
                   (print-chess-engine-output name line debug))
                 done?)
          :until done?
          :finally (return (if (eql :checkmate done?) t nil)))))

(define-function half-turn ((engine-active chess-engine)
                            (move move)
                            (position-string position-string-and-position)
                            seconds)
  (with-position-string-and-position (string position) position-string
    (update-position engine-active string nil position)
    (let* ((ponder (make-move))
           (checkmate? (chess-engine-move engine-active seconds move ponder)))
      (setf (char string position) #\Space)
      (replace string move :start1 (1+ position))
      (incf position (if (promotion? move) (1+ +move-length+) +move-length+))
      checkmate?)))

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
