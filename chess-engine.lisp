(defpackage #:chess-engine
  (:use #:cl)
  (:import-from #:uiop
                #:launch-program
                #:process-info-input
                #:process-info-output
                #:wait-process))

(in-package #:chess-engine)

(defun run-command (command process-input &optional (prompt "> "))
  (write-string prompt)
  (write-string command)
  (terpri)
  (write-line command process-input)
  (force-output process-input))

(defun run-command* (command process-input argument-vector &optional final-argument (prompt "> "))
  (write-string prompt)
  (write-string command)
  (write-string command process-input)
  (dotimes (i (length argument-vector))
    (write-char #\Space)
    (write-char #\Space process-input)
    (write-string (aref argument-vector i))
    (write-string (aref argument-vector i) process-input))
  (when final-argument
    (write-char #\Space)
    (write-char #\Space process-input)
    (write-string final-argument)
    (write-string final-argument process-input))
  (terpri t)
  (terpri process-input)
  (force-output process-input))

(defun quit-chess-engine (process &optional (prompt "> "))
  (let ((process-input (process-info-input process)))
    (run-command "quit" process-input prompt)
    (wait-process process)))

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

(defun print-board (board)
  (declare (board board))
  (dotimes (i 8)
    (write-char #\Space t)
    (dotimes (j 8)
      (write-char (let ((piece (aref board i j)))
                    (if (not (char= #\Nul piece))
                        piece
                        #\Space))
                  t)
      (write-char #\Space t))
    (terpri t)))

;;; note: this is just the board part of the FEN representation
(defun print-fen-board (board)
  (declare (board board))
  (dotimes (i 8)
    (let ((counter 0))
      (declare ((integer 0 8) counter))
      (dotimes (j 8)
        (let ((piece (aref board i j)))
          (if (not (char= #\Nul piece))
              (progn
                (unless (zerop counter)
                  (format t "~D" counter)
                  (setf counter 0))
                (write-char piece t))
              (incf counter))))
      (unless (zerop counter)
        (format t "~D" counter)))
    (unless (= i 7)
      (format t "/")))
  (terpri t))

(defun chess-engine-initialize (engine-name chess-engine-process threads &optional (prompt "> "))
  (let ((input (process-info-input chess-engine-process))
        (output (process-info-output chess-engine-process)))
    ;; Sends it the UCI command and handles the results, as well as
    ;; anything that was output before the UCI command was sent, if
    ;; anything.
    (run-command "uci" input prompt)
    ;; todo: kill process if uciok is never received
    (do ((line (read-line output nil)
               (read-line output nil)))
        ((or (eql :eof line)
             (string= "uciok" line))
         (format t "~A : ~A~%" engine-name line))
      (format t "~A : ~A~%" engine-name line))
    (run-command (format nil "setoption name Threads value ~D" threads) input prompt)
    (run-command "isready" input prompt)
    (format t "~A : ~A~%" engine-name (read-line output nil))
    (run-command "ucinewgame" input prompt)
    (run-command "isready" input prompt)
    (format t "~A : ~A~%" engine-name (read-line output nil))))

(defun chess-engine-update-position (chess-engine-process best-moves &optional ponder-move (prompt "> "))
  (run-command* "position startpos moves" (process-info-input chess-engine-process) best-moves ponder-move prompt))

(defun chess-engine-move (engine-name chess-engine-process best-moves ponder-moves seconds &optional (prompt "> "))
  (let ((chess-engine-input (process-info-input chess-engine-process))
        (chess-engine-output (process-info-output chess-engine-process)))
    (run-command (format nil "go movetime ~D000" seconds) chess-engine-input prompt)
    (do ((line (read-line chess-engine-output nil :eof)
               (read-line chess-engine-output nil :eof)))
        ((or (eql :eof line)
             (and (>= (length line) 8) (string= "bestmove" (subseq line 0 8))))
         (format t "~A : ~A~%" engine-name line)
         (with-input-from-string (command line :start 9)
           ;; Reads the actual best move
           (vector-push (with-output-to-string (out-string)
                          (do ((char (read-char command) (read-char command)))
                              ((or (eql char :eof) (char= char #\Space)))
                            (write-char char out-string)))
                        best-moves)
           ;; Reads ponder, if it exists. This assumes the next word
           ;; either is ponder or does not exist.
           (let ((ponder? (do ((char (read-char command nil :eof) (read-char command nil :eof)))
                              ((or (eql char :eof) (char= char #\Space))
                               (not (eql char :eof))))))
             (format t "~A~%" ponder?)
             (if ponder?
                 ;; Assumes the rest of the line is the ponder command
                 (vector-push (with-output-to-string (out-string)
                                (read-line command))
                              ponder-moves)
                 (vector-push nil ponder-moves)))))
      (unless (and (>= (length line) 4) (string= "info" (subseq line 0 4)))
        (format t "~A : ~A~%" engine-name line)))))

(defun chess-command-ponder-start (chess-engine-process &optional (prompt "> "))
  (run-command "go ponder" (process-info-input chess-engine-process) prompt))

(defun chess-engine-ponder-end (engine-name chess-engine-process success &optional (prompt "> "))
  (let ((chess-engine-input (process-info-input chess-engine-process))
        (chess-engine-output (process-info-output chess-engine-process)))
    (run-command (if success "ponderhit" "stop") chess-engine-input prompt)
    ;; Note: technically, the info is generated while it's pondering
    ;; and merely read here after stop
    (do ((line (read-line chess-engine-output nil :eof)
               (read-line chess-engine-output nil :eof)))
        ((or (eql :eof line)
             (and (>= (length line) 8) (string= "bestmove" (subseq line 0 8))))
         (format t "~A : ~A~%" engine-name line))
      (unless (and (>= (length line) 4) (string= "info" (subseq line 0 4)))
        (format t "~A : ~A~%" engine-name line)))))

(defun chess-engine-leftover-output (engine-name chess-engine-process)
  (let ((output (process-info-output chess-engine-process)))
    (do ((line (read-line output nil :eof)
               (read-line output nil :eof)))
        ((eql line :eof))
      (format t "~A : ~A~%" engine-name line))))

(defun chess-engine-half-turn (process-active name-active prompt-active
                               process-pondering name-pondering prompt-pondering
                               best-moves ponder-moves
                               seconds)
  (let ((ponder-move (vector-pop ponder-moves)))
    (chess-engine-update-position process-active best-moves nil prompt-active)
    (when ponder-move
      (chess-engine-update-position process-pondering best-moves ponder-move prompt-pondering)
      (chess-command-ponder-start process-pondering prompt-pondering))
    (chess-engine-move name-active process-active best-moves ponder-moves seconds prompt-active)
    (let* ((move (vector-pop best-moves))
           (success (string= move ponder-move)))
      (vector-push move best-moves)
      (when ponder-move
        (chess-engine-ponder-end name-pondering process-pondering success prompt-pondering)))))

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

(defun update-board (board best-moves)
  (declare (board board))
  (let ((move (vector-pop best-moves)))
    (if (= (length move) 4)
        (setf (%chess-board-ref board (char move 2) (char move 3))
              (%chess-board-ref board (char move 0) (char move 1))
              (%chess-board-ref board (char move 0) (char move 1))
              #\Null)
        (error "Not a supported move to parse."))
    (vector-push move best-moves)
    board))

;;; todo: record moves in algebraic notation
(defun chess-engine (&key (engine-name "stockfish") (threads 8) (seconds 10) (turns 3))
  (let ((process-1 (launch-program engine-name :input :stream :output :stream))
        (process-2 (launch-program engine-name :input :stream :output :stream))
        (engine-name-1 (concatenate 'string engine-name "-1"))
        (engine-name-2 (concatenate 'string engine-name "-2"))
        (prompt-1 "1 > ")
        (prompt-2 "2 > ")
        (board (make-board))
        ;; fixme: find a more efficient way to store moves
        (best-moves (make-array 400 :fill-pointer 0))
        (ponder-moves (make-array 2 :fill-pointer 1 :initial-element "e2e4"))
        (threads (floor threads 2)))
    (unwind-protect
         (progn
           (chess-engine-initialize engine-name-1 process-1 threads prompt-1)
           (chess-engine-initialize engine-name-2 process-2 threads prompt-2)
           (terpri)
           (print-board board)
           (terpri)
           (dotimes (i turns)
             (chess-engine-half-turn process-1 engine-name-1 prompt-1
                                     process-2 engine-name-2 prompt-2
                                     best-moves ponder-moves
                                     seconds)
             (update-board board best-moves)
             (terpri)
             (print-board board)
             (terpri)
             (chess-engine-half-turn process-2 engine-name-2 prompt-2
                                     process-1 engine-name-1 prompt-1
                                     best-moves ponder-moves
                                     seconds)
             (update-board board best-moves)
             (terpri)
             (print-board board)
             (terpri))
           best-moves)
      ;; Quits the chess engine.
      (quit-chess-engine process-1 prompt-1)
      (chess-engine-leftover-output engine-name-1 process-1)
      (quit-chess-engine process-2 prompt-2)
      (chess-engine-leftover-output engine-name-2 process-2))))
