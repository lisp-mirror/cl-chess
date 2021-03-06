(defpackage #:cl-chess/uci
  (:use #:cl
        #:cl-chess/util
        #:zombie-raptor
        #:zr-utils)
  (:import-from #:alexandria
                #:array-index
                #:once-only)
  (:import-from #:uiop
                #:launch-program
                #:process-alive-p
                #:process-info-input
                #:process-info-output
                #:terminate-process
                #:wait-process)
  (:import-from #:uiop/launch-program
                #:process-info)
  (:export #:chess-engine-profile
           #:half-turn
           #:initialize-chess-engines
           #:make-chess-engine
           #:make-chess-engine-pair
           #:make-chess-engine-profile
           #:make-move
           #:make-position-string-and-position
           #:move
           #:null-move?
           #:print-chess-engine-output
           #:quit-chess-engines))

(in-package #:cl-chess/uci)

;;; UCI commands

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-function (keyword-to-uci-command :inline t) ((command keyword))
    (declare (optimize (speed 3)))
    "
Turns keywords into the equivalent literal strings of the UCI
commands. This supports direct equivalents of the UCI command names as
well as reasonable, idiomatic-sounding alternatives, usually by
inserting hyphens.
"
    ;; Note: The commands are in the same order as the spec to make it
    ;; easier to look up what they do.
    (ecase command
      ;; Client commands
      (:uci "uci")
      (:debug "debug")
      ((:isready :is-ready :readyp :ready-p :ready?) "isready")
      ((:setoption :set-option) "setoption")
      (:register "register")
      ((:ucinewgame :uci-newgame :uci-new-game) "ucinewgame")
      (:position "position")
      (:go "go")
      (:stop "stop")
      ((:ponderhit :ponder-hit) "ponderhit")
      (:quit "quit")
      ;; Server commands
      (:id "id")
      ((:uciok :uci-ok) "uciok")
      ((:readyok :ready-ok) "readyok")
      ((:bestmove :best-move) "bestmove")
      ((:copyprotection :copy-protection) "copyprotection")
      (:registration "registration")
      (:info "info")
      (:option "option"))))

;;; UCI move strings

(deftype move ()
  `(simple-string 5))

(defconstant +move-length+ 5)
(defconstant +possible-promotions+ 16)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-function (make-move :inline t) (&optional (string "0000"))
    (replace (make-string 5 :initial-element #\Nul) string)))

(define-function (promotion? :inline t) (move)
  (not (char= (char move 4) #\Nul)))

(define-function (null-move? :inline t) (move)
  (string= #.(make-move) move))

;;; Position string

;;; todo: Replace the position history with a FEN board if there are
;;; more than 200 turns. Make sure all other implicit turn limits are
;;; similarly handled so an infinite game is possible.

(defconstant +position-string-length+ (+ +possible-promotions+
                                         (* 400 +move-length+)))

(deftype position-string ()
  `(simple-string ,+position-string-length+))

(define-function (make-position-string :inline t) ()
  (make-string +position-string-length+ :initial-element #\Nul))

(defstruct position-string-and-position
  (string (make-position-string) :type position-string)
  (position 0 :type fixnum))

(define-accessor-macro with-position-string-and-position #.(symbol-name '#:position-string-and-position-))

;;; Chess engine

(defstruct (chess-engine (:constructor %make-chess-engine))
  (process    nil :type (maybe process-info) :read-only t)
  (input      nil :type stream               :read-only t)
  (output     nil :type stream               :read-only t)
  (debug      nil :type (maybe stream))
  (debug-info   2 :type (integer 0 3))
  (name       nil :type string               :read-only t)
  (prompt     nil :type string               :read-only t))

(define-accessor-macro with-chess-engine #.(symbol-name '#:chess-engine-))

(define-function (make-chess-engine :inline t) (&key process name prompt debug-stream debug-info)
  (multiple-value-bind (input output)
      (if process
          (values (process-info-input process)
                  (process-info-output process))
          (values (make-instance 'character-pipe)
                  (make-instance 'character-pipe)))
    (%make-chess-engine :process process
                        :input input
                        :output output
                        :debug debug-stream
                        :debug-info debug-info
                        :name name
                        :prompt prompt)))

(defstruct chess-engine-profile
  (name              nil :type string)
  (debug-stream      nil :type (maybe stream))
  (debug-info        2   :type (integer 0 3))
  (separate-process? t   :type boolean))

(define-accessor-macro with-chess-engine-profile #.(symbol-name '#:chess-engine-profile-))

(define-function (make-chess-engine* :inline t) ((chess-engine-profile chess-engine-profile)
                                                 &key
                                                 (side 1 (integer 1 2))
                                                 (mirror-match? nil boolean))
  (with-chess-engine-profile (name debug-stream debug-info separate-process?)
      chess-engine-profile
    (make-chess-engine :process (if separate-process?
                                    (launch-program name :input :stream :output :stream)
                                    nil)
                       :name (if mirror-match? (format nil "~A-~D" name side) name)
                       :prompt (format nil "~D > " side)
                       :debug-stream debug-stream
                       :debug-info debug-info)))

(define-function (print-chess-engine-output :inline t) (name line debug-stream)
  (when debug-stream
    (format debug-stream "~A : ~A~%" name line)))

;;; UCI commands

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun generate-command* (command)
    (typecase command
      (keyword (keyword-to-uci-command command))
      (list (destructuring-bind (command &rest rest)
                command
              (ecase command
                ((:setoption :set-option)
                 (multiple-value-bind (name value)
                     (if (and rest (member (car rest) `(:name :value)))
                         (destructuring-bind (&key name value)
                             rest
                           (unless name
                             (error "The field name is required in setoption"))
                           (values name value))
                         (destructuring-bind (name value)
                             rest
                           (values name value)))
                   `(:format "setoption name ~A~@[ value ~A~]~%" ,name ,value)))
                (:go `(:format
                       #.(concatenate 'string
                                      "go~:[~; ponder~]"
                                      "~@[ wtime ~D~]~@[ btime ~D~]~@[ winc ~D~]~@[ binc ~D~]~@[ movestogo ~D~]"
                                      "~@[ depth ~D~]~@[ nodes ~D~]~@[ mate ~D~]"
                                      "~@[ movetime ~D~]~:[~; infinite~]~%")
                       ,@rest))
                (:id
                 (destructuring-bind (field data)
                     rest
                   `(:format "id ~A ~A~%" ,field ,data)))
                (:position
                 (destructuring-bind (moves end)
                     rest
                   `(:write (write-string "position startpos moves")
                            (write-line ,moves ,end)))))))
      (t command)))

  (defun generate-command (command input prompt debug)
    "
Returns either a simple UCI command or a command with arguments
depending on what was passed into `with-uci-commands'.
"
    (let ((command (generate-command* command)))
      (flet ((write-command (stream)
               (typecase command
                 ;; If the command is a list, then either it is the
                 ;; last parts of a FORMAT command or it is a list of
                 ;; various WRITE commands.
                 (list (ecase (car command)
                         (:format `((format ,stream ,@(cdr command))))
                         (:write (mapcar (lambda (c)
                                           (destructuring-bind (command string &optional end) c
                                             (let ((end (if end `(:end ,end) nil)))
                                               `(,command ,string ,stream ,@end))))
                                         (cdr command)))))
                 (t `((write-line ,command ,stream))))))
        `(progn
           (when ,debug
             (write-string ,prompt ,debug)
             ,@(write-command debug))
           ,@(write-command input))))))

(defmacro with-uci-commands ((chess-engine) &body commands)
  "
Turns UCI expressed as s-expressions into writing UCI strings,
directed at the given chess-engine instance.
"
  (once-only (chess-engine)
    (alexandria:with-gensyms (input prompt debug)
      `(with-chess-engine ((,input input) (,prompt prompt) (,debug debug)) ,chess-engine
         ,@(mapcar (lambda (command)
                     (generate-command command input prompt debug))
                   commands)
         (force-output ,input)
         nil))))

;;; UCI client

(define-function read-opening-message ((chess-engine chess-engine))
  "
Reads the opening message. If debug, then write it. Otherwise, it's
just discarded because it doesn't provide any useful information to
the program itself.
"
  (with-chess-engine (output name debug) chess-engine
    (when debug (format debug "~A : " name))
    (read-case (output char :no-hang? t)
      (t (when (and debug char)
           (write-char char debug))))))

(define-function chess-engine-leftover-output ((chess-engine chess-engine))
  "
Processes any output that is left over when the chess engine has
finished running. This basically reads until EOF so nothing hangs.
"
  (with-chess-engine (output name debug) chess-engine
    (let ((first? t))
      (read-case (output char)
        (t (when first?
             (when debug (format debug "~A : " name))
             (setf first? nil))
           (when debug (write-char char debug)))))))

(defstruct uci-option
  (name    nil :type (maybe string))
  (type    nil :type (maybe string))
  (default nil :type (maybe string))
  (min     nil :type (maybe string))
  (max     nil :type (maybe string))
  ;; fixme: multiple vars are allowed
  (var     nil :type (maybe string)))

(define-function initialize-uci ((chess-engine chess-engine))
  (with-chess-engine (process output name debug debug-info)
      chess-engine
    (with-uci-commands (chess-engine)
      :uci)
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
                          (t (setf line-subtype
                                   (with-accessors* (uci-option-name
                                                     uci-option-type
                                                     uci-option-default
                                                     uci-option-min
                                                     uci-option-max
                                                     uci-option-var)
                                       uci-option
                                     (ecase line-subtype
                                       (:name
                                        ;; The word "type" is the end of the name.
                                        (if (string= line "type" :start1 start :end1 end)
                                            (progn
                                              (setf uci-option-name (subseq line name-start name-end))
                                              :type)
                                            (progn
                                              (setf name-end end)
                                              :name)))
                                       (:type
                                        (setf uci-option-type (subseq line start end))
                                        :parameter)
                                       (:default
                                        (setf uci-option-default (subseq line start end))
                                        :parameter)
                                       (:min
                                        (setf uci-option-min (subseq line start end))
                                        :parameter)
                                       (:max
                                        (setf uci-option-max (subseq line start end))
                                        :parameter)
                                       ;; fixme: multiple vars are allowed
                                       (:var
                                        (setf uci-option-var (subseq line start end))
                                        :parameter)
                                       (:parameter (cond ((string= line "default" :start1 start :end1 end) :default)
                                                         ((string= line "min" :start1 start :end1 end) :min)
                                                         ((string= line "max" :start1 start :end1 end) :max)
                                                         ((string= line "var" :start1 start :end1 end) :var)
                                                         (t line-subtype))))))))))))
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

(define-function ready? ((chess-engine chess-engine))
  "
Tries for approximately 5 seconds to receive the line \"readyok\" in
response to the command \"isready\".
"
  (with-chess-engine (name output debug process)
      chess-engine
    (with-uci-commands (chess-engine)
      :ready?)
    (let ((i 0)
          (line-position 0)
          (line "readyok")
          (line-match? nil)
          (start-time (get-internal-real-time))
          (seconds 5))
      (declare (array-index i line-position))
      (do-read-char (char output :no-hang t :end-var line-match?)
        (when (and debug char (zerop line-position))
          (format debug "~A : " name))
        (cond ((null char)
               (sleep 1/1000))
              ((char= char #\Newline)
               (when debug (terpri debug))
               (if (and (= (length line) line-position)
                        (= (length line) i))
                   (setf line-match? t)
                   (setf i 0))
               (setf line-position 0))
              (t
               (when debug (write-char char debug))
               (if (and (<= 0 i (- (length line) 1))
                        (char= (char line i) char))
                   (incf i)
                   (setf i 0))
               (incf line-position)))
        (when (and (not line-match?)
                   (> (- (get-internal-real-time) start-time)
                      (* seconds internal-time-units-per-second)))
          ;; fixme: This will only terminate the one process, but the
          ;; other process will stay up.
          (terminate-process process)
          (error "Process ~A did not respond with \"readyok\"." name))))))

(define-function new-game ((chess-engine chess-engine))
  "Tells the chess engine to start a new game and waits until it's ready."
  (with-uci-commands (chess-engine)
    :uci-new-game)
  (ready? chess-engine))

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

infinite? tells the engine to wait until stop if true.

Note: search-moves has not yet been implemented, but might be
implemented in the future.
"
  (let ((moves-to-go (if (and moves-to-go (zerop moves-to-go))
                         nil
                         moves-to-go)))
    (with-uci-commands (chess-engine)
      (:go ponder? w-time b-time w-inc b-inc moves-to-go depth nodes mate move-time infinite?))))

(defun initialize-chess-engine (chess-engine threads)
  "
Initializes the UCI chess engine from the startup to a new game.
"
  (with-chess-engine (process output name debug)
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
            (with-uci-commands (chess-engine)
              (:set-option "Threads" threads*)))
          (print-chess-engine-output "DEBUG" "Note: Threads cannot be customized." debug)))
    (ready? chess-engine)
    (new-game chess-engine)))

(define-function initialize-chess-engines ((chess-engine-1 chess-engine) (chess-engine-2 chess-engine) threads)
  "Initializes both chess engines. Use this if the AI is playing AI."
  (initialize-chess-engine chess-engine-1 threads)
  (initialize-chess-engine chess-engine-2 threads))

(define-function quit-chess-engine ((chess-engine chess-engine))
  "
Tell the chess engine process to quit and then wait for the process to
complete, reading its remaining output if there is any.
"
  (with-chess-engine (process)
      chess-engine
    (when (process-alive-p process)
      (with-uci-commands (chess-engine)
        :quit)
      (wait-process process)
      (chess-engine-leftover-output chess-engine)))
  nil)

(define-function quit-chess-engines ((chess-engine-1 chess-engine) (chess-engine-2 chess-engine))
  "Quit both chess engines. Use this if the AI is playing AI."
  (quit-chess-engine chess-engine-1)
  (quit-chess-engine chess-engine-2))

(define-function update-position (chess-engine position-string (ponder-move (maybe move)) end)
  (when ponder-move
    (setf (char position-string end) #\Space)
    (replace position-string ponder-move :start1 (1+ end)))
  (let ((end (if ponder-move
                 (+ end (if (promotion? ponder-move)
                            +move-length+
                            (1- +move-length+)))
                 end)))
    (with-uci-commands (chess-engine)
      (:position position-string end)))
  (when ponder-move
    (fill position-string #\Nul :start end :end (+ end +move-length+))))

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
                                    (seconds (integer 1 *))
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

(define-function half-turn ((active-ai chess-engine)
                            (move move)
                            (position-string position-string-and-position)
                            seconds)
  "
Does half a turn, i.e. if an AI is playing AI, then this is one of the
AIs' turns. If the AI is playing a human, then this is the AI's turn.
"
  (with-position-string-and-position (string position) position-string
    (update-position active-ai string nil position)
    (let* ((ponder (make-move))
           (checkmate? (chess-engine-move active-ai seconds move ponder)))
      (setf (char string position) #\Space)
      (replace string move :start1 (1+ position))
      (incf position (if (promotion? move) (1+ +move-length+) +move-length+))
      checkmate?)))

(define-function (make-chess-engine-pair :inline t) ((profile-1 chess-engine-profile)
                                                     (profile-2 chess-engine-profile))
  "
Matches the AI against another AI by creating two chess-engine
objects, one for each chess engine. If the AI is playing another AI of
the same chess engine, then it is a mirror match. In this case, -1 and
-2 need to be added to the name so that the logs show which is which.
They will still run in separate processes, even if it plays itself.
"
  (let ((mirror-match? (string= (chess-engine-profile-name profile-1)
                                (chess-engine-profile-name profile-2))))
    (values (make-chess-engine* profile-1 :side 1 :mirror-match? mirror-match?)
            (make-chess-engine* profile-2 :side 2 :mirror-match? mirror-match?))))

;;; UCI server

(defun id (name author chess-engine)
  (with-uci-commands (chess-engine)
    (:id "name" name)
    (:id "author" author)))
