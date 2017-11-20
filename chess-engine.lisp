(defpackage #:chess-engine
  (:use #:cl)
  (:import-from #:uiop
                #:launch-program
                #:process-info-input
                #:process-info-output
                #:wait-process))

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

(defun print-board (board &optional (stream t))
  (declare (board board))
  (dotimes (i 8)
    (write-char #\Space stream)
    (dotimes (j 8)
      (write-char (let ((piece (aref board i j)))
                    (if (not (char= #\Nul piece))
                        piece
                        #\Space))
                  stream)
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

(defun chess-engine-move (engine-name chess-engine-process best-moves seconds &optional (prompt "> ") debug-stream)
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
      (unless (or (not debug-stream) (and (>= (length line) 4) (string= "info" (subseq line 0 4))))
        (format debug-stream "~A : ~A~%" engine-name line)))))

(defun chess-command-ponder-start (chess-engine-process &optional (prompt "> ") debug-stream)
  (run-command "go ponder" (process-info-input chess-engine-process) prompt debug-stream))

(defun chess-engine-ponder-end (engine-name chess-engine-process success &optional (prompt "> ") debug-stream)
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
      (unless (or (not debug-stream) (and (>= (length line) 4) (string= "info" (subseq line 0 4))))
        (format debug-stream "~A : ~A~%" engine-name line)))))

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
                               debug-stream)
  (let ((new-ponder-move nil))
    (chess-engine-update-position process-active best-moves nil prompt-active debug-stream)
    (when ponder-move
      (chess-engine-update-position process-pondering best-moves ponder-move prompt-pondering debug-stream)
      (chess-command-ponder-start process-pondering prompt-pondering debug-stream))
    (setf new-ponder-move (chess-engine-move name-active process-active best-moves seconds prompt-active debug-stream))
    (let ((move (vector-pop best-moves)))
      (vector-push move best-moves)
      (when ponder-move
        (chess-engine-ponder-end name-pondering process-pondering (string= move ponder-move) prompt-pondering debug-stream)))
    new-ponder-move))

;;; todo: Record moves in algebraic notation
;;;
;;; todo: Handle the end of the game instead of just going a certain
;;; number of turns
(defun chess-engine (&key (engine-name "stockfish") (threads 8) (seconds 10) (turns 3) debug-stream)
  (let ((process-1 (launch-program engine-name :input :stream :output :stream))
        (process-2 (launch-program engine-name :input :stream :output :stream))
        (engine-name-1 (concatenate 'string engine-name "-1"))
        (engine-name-2 (concatenate 'string engine-name "-2"))
        (prompt-1 "1 > ")
        (prompt-2 "2 > ")
        (board (make-board))
        ;; fixme: find a more efficient way to store moves
        (best-moves (make-array 400 :fill-pointer 0))
        (ponder-move "e2e4")
        (threads (floor threads 2)))
    (unwind-protect
         (progn
           (chess-engine-initialize engine-name-1 process-1 threads prompt-1 debug-stream)
           (chess-engine-initialize engine-name-2 process-2 threads prompt-2 debug-stream)
           (terpri)
           (print-board board)
           (terpri)
           (dotimes (i turns)
             (setf ponder-move (chess-engine-half-turn process-1 engine-name-1 prompt-1
                                                       process-2 engine-name-2 prompt-2
                                                       best-moves ponder-move
                                                       seconds
                                                       debug-stream))
             (update-board board best-moves)
             (terpri)
             (print-board board)
             (terpri)
             (setf ponder-move (chess-engine-half-turn process-2 engine-name-2 prompt-2
                                                       process-1 engine-name-1 prompt-1
                                                       best-moves ponder-move
                                                       seconds
                                                       debug-stream))
             (update-board board best-moves)
             (terpri)
             (print-board board)
             (terpri))
           best-moves)
      ;; Quits the chess engine.
      (quit-chess-engine process-1 prompt-1 debug-stream)
      (chess-engine-leftover-output engine-name-1 process-1 debug-stream)
      (quit-chess-engine process-2 prompt-2 debug-stream)
      (chess-engine-leftover-output engine-name-2 process-2 debug-stream))))
