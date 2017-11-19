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

(defun run-command (command process-input)
  (format t "> ~A~%" command)
  (write-line command process-input)
  (force-output process-input))

(defun run-command* (command process-input argument-vector)
  (format t "> ~A" command)
  (write-string command process-input)
  (dotimes (i (length argument-vector))
    (write-char #\Space)
    (write-string (aref argument-vector i))
    (write-string (aref argument-vector i) process-input))
  (terpri t)
  (terpri process-input)
  (force-output process-input))

(defun quit-chess-engine (process)
  (let ((process-input (process-info-input process)))
    (format t "> quit")
    (write-line "quit" process-input)
    (force-output process-input)
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

(defun initialize-chess-engine (engine-name chess-engine-input chess-engine-output threads)
  ;; Sends it the UCI command and handles the results, as well as
  ;; anything that was output before the UCI command was sent, if
  ;; anything.
  (run-command "uci" chess-engine-input)
  ;; todo: kill process if uciok is never received
  (do ((line (read-line chess-engine-output nil :eof)
             (read-line chess-engine-output nil :eof)))
      ((or (eql :eof line)
           (string= "uciok" line))
       (format t "~A : ~A~%" engine-name line))
    (format t "~A : ~A~%" engine-name line))
  (run-command (format nil "setoption name Threads value ~D" threads) chess-engine-input)
  (run-command "isready" chess-engine-input)
  (format t "~A : ~A~%" engine-name (read-line chess-engine-output nil :eof))
  (run-command "ucinewgame" chess-engine-input)
  (run-command "isready" chess-engine-input)
  (format t "~A : ~A~%" engine-name (read-line chess-engine-output nil :eof)))

(defun chess-engine-move (engine-name chess-engine-input chess-engine-output command-stream best-moves seconds)
  (run-command* "position startpos moves" chess-engine-input best-moves)
  (run-command (format nil "go movetime ~D000" seconds) chess-engine-input)
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
      (format t "~A : ~A~%" engine-name line))))

(defun chess-engine (&optional (engine-name "stockfish"))
  (with-open-stream (command-stream (make-instance 'character-pipe))
    (let* ((process (launch-program engine-name :input :stream :output :stream))
           (process-input (process-info-input process))
           (process-output (process-info-output process))
           (board (make-board))
           ;; fixme: find a more efficient way to store moves
           (best-moves (make-array 400 :fill-pointer 0))
           (seconds 10)
           (threads 8)
           (threads* (floor threads 2)))
      (print-board board)
      (initialize-chess-engine engine-name process-input process-output threads*)
      ;; fixme: stockfish can't play with itself, run two stockfishes
      ;;
      ;; todo: ponder followed by stop or ponderhit when it's the
      ;; other side's turn
      (chess-engine-move engine-name process-input process-output command-stream best-moves seconds)
      (chess-engine-move engine-name process-input process-output command-stream best-moves seconds)
      ;; Quits the chess engine.
      (quit-chess-engine process)
      (do ((line (read-line process-output nil :eof)
                 (read-line process-output nil :eof)))
          ((eq line :eof))
        (format t "~A~%" line))
      best-moves)))
