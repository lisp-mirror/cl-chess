(defpackage #:chess-engine
  (:use #:cl)
  (:import-from #:uiop
                #:launch-program
                #:process-info-input
                #:process-info-output
                #:wait-process)
  (:import-from #:zombie-raptor
                #:character-pipe))

(in-package #:chess-engine)

(defun run-command (command process-input &optional (prompt "> "))-
  (format t "~A~A~%" prompt command)
  (write-line command process-input)
  (force-output process-input))

(defun run-command* (command process-input argument-vector &optional (prompt "> "))
  (format t "~A~A" prompt command)
  (write-string command process-input)
  (dotimes (i (length argument-vector))
    (write-char #\Space)
    (write-string (aref argument-vector i))
    (write-string (aref argument-vector i) process-input))
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

(defun initialize-chess-engine (engine-name chess-engine-process threads &optional (prompt "> "))
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

(defun chess-engine-move (engine-name chess-engine-process command-stream best-moves seconds &optional (prompt "> "))
  (let ((chess-engine-input (process-info-input chess-engine-process))
        (chess-engine-output (process-info-output chess-engine-process)))
    (run-command* "position startpos moves" chess-engine-input best-moves prompt)
    (run-command (format nil "go movetime ~D000" seconds) chess-engine-input prompt)
    (do ((line (read-line chess-engine-output nil :eof)
               (read-line chess-engine-output nil :eof)))
        ((or (eql :eof line)
             (and (>= (length line) 8) (string= "bestmove" (subseq line 0 8))))
         (format t "~A : ~A~%" engine-name line)
         (write-line line command-stream)
         ;; Reads bestmove, but does nothing with it
         (do ((char (read-char command-stream) (read-char command-stream)))
             ((char= char #\Space))
           ;; Don't optimize this loop away.
          char)
         ;; Reads the actual best move
         (vector-push (with-output-to-string (out-string)
                        (do ((char (read-char command-stream) (read-char command-stream)))
                            ((char= char #\Space))
                          (write-char char out-string)))
                      best-moves)
         ;; reads the ponder and does nothing with it
         (read-line command-stream))
      (unless (and (>= (length line) 4) (string= "info" (subseq line 0 4)))
        (format t "~A : ~A~%" engine-name line)))))

;; todo: it's stop or ponderhit, depending on if it's a match
(defun chess-engine-end-ponder (engine-name chess-engine-process &optional prompt)
  (let ((chess-engine-input (process-info-input chess-engine-process))
        (chess-engine-output (process-info-output chess-engine-process)))
    (run-command "stop" chess-engine-input prompt)
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
        ((eq line :eof))
      (format t "~A : ~A~%" engine-name line))))

(defun chess-engine (&key (engine-name "stockfish") (threads 8) (seconds 10))
  (with-open-stream (command-stream (make-instance 'character-pipe))
    (let ((process-1 (launch-program engine-name :input :stream :output :stream))
          (process-2 (launch-program engine-name :input :stream :output :stream))
          (engine-name-1 (concatenate 'string engine-name "-1"))
          (engine-name-2 (concatenate 'string engine-name "-2"))
          (prompt-1 "1 > ")
          (prompt-2 "2 > ")
          (board (make-board))
          ;; fixme: find a more efficient way to store moves
          (best-moves (make-array 400 :fill-pointer 0))
          (threads (floor threads 2)))
      (unwind-protect
           (progn
             (initialize-chess-engine engine-name-1 process-1 threads prompt-1)
             (initialize-chess-engine engine-name-2 process-2 threads prompt-2)
             (print-board board)
             (run-command "position startpos moves" (process-info-input process-1) prompt-1)
             (run-command "position startpos moves" (process-info-input process-2) prompt-2)
             (run-command "go ponder" (process-info-input process-1) prompt-1)
             (run-command "go ponder" (process-info-input process-2) prompt-2)
             (sleep 5)
             (chess-engine-end-ponder engine-name-1 process-1 prompt-1)
             (chess-engine-end-ponder engine-name-2 process-2 prompt-2)
             (chess-engine-move engine-name-1 process-1 command-stream best-moves seconds prompt-1)
             (chess-engine-move engine-name-2 process-2 command-stream best-moves seconds prompt-2)
             best-moves)
        ;; Quits the chess engine.
        (quit-chess-engine process-1 prompt-1)
        (chess-engine-leftover-output engine-name-1 process-1)
        (quit-chess-engine process-2 prompt-2)
        (chess-engine-leftover-output engine-name-2 process-2)))))
