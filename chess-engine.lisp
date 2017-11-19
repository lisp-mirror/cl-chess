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

(defun chess-engine (&optional (engine-name "stockfish"))
  (with-open-stream (command-stream (make-instance 'character-pipe))
    (let* ((process (launch-program engine-name :input :stream :output :stream))
           (process-input (process-info-input process))
           (process-output (process-info-output process))
           (board (make-board))
           (best-moves (make-array 400 :fill-pointer 0))
           (seconds 10))
      (print-board board)
      ;; Sends it the UCI command and handles the results, as well as
      ;; anything that was output before the UCI command was sent, if
      ;; anything.
      (run-command "uci" process-input)
      ;; todo: kill process if uciok is never received
      (do ((line (read-line process-output nil :eof)
                 (read-line process-output nil :eof)))
          ((or (eql :eof line)
               (string= "uciok" line))
           (format t "~A : ~A~%" engine-name line))
        (format t "~A : ~A~%" engine-name line))
      (run-command "setoption name Threads value 8" process-input)
      (run-command "isready" process-input)
      (format t "~A : ~A~%" engine-name (read-line process-output nil :eof))
      (run-command "ucinewgame" process-input)
      (run-command "isready" process-input)
      (format t "~A : ~A~%" engine-name (read-line process-output nil :eof))
      (run-command "position startpos moves" process-input)
      (run-command (format nil "go movetime ~D000" seconds) process-input)
      (do ((line (read-line process-output nil :eof)
                 (read-line process-output nil :eof)))
          ((or (eql :eof line)
               (and (>= (length line) 8) (string= "bestmove" (subseq line 0 8))))
           (format t "~A : ~A~%" engine-name line)
           (write-line line command-stream)
           ;; reads bestmove, but does nothing with it
           (do ((char (read-char command-stream) (read-char command-stream)))
               ((char= char #\Space)))
           ;; reads the actual best move
           ;;
           ;; fixme: don't generate garbage here
           (vector-push (with-output-to-string (out-string)
                          (do ((char (read-char command-stream) (read-char command-stream)))
                              ((char= char #\Space))
                            (write-char char out-string)))
                        best-moves)
           ;; reads the ponder and does nothing with it
           (read-line command-stream))
        (unless (and (>= (length line) 4) (string= "info" (subseq line 0 4)))
          (format t "~A : ~A~%" engine-name line)))
      ;; fixme: stockfish can't play with itself, run two stockfishes
      (run-command* "position startpos moves" process-input best-moves)
      (run-command (format nil "go movetime ~D000" seconds) process-input)
      (do ((line (read-line process-output nil :eof)
                 (read-line process-output nil :eof)))
          ((or (eql :eof line)
               (and (>= (length line) 8) (string= "bestmove" (subseq line 0 8))))
           (format t "~A : ~A~%" engine-name line)
           (write-line line command-stream)
           ;; reads bestmove, but does nothing with it
           (do ((char (read-char command-stream) (read-char command-stream)))
               ((char= char #\Space)))
           ;; reads the actual best move
           ;;
           ;; fixme: don't generate garbage here
           (vector-push (with-output-to-string (out-string)
                          (do ((char (read-char command-stream) (read-char command-stream)))
                              ((char= char #\Space))
                            (write-char char out-string)))
                        best-moves)
           ;; reads the ponder and does nothing with it
           (read-line command-stream))
        (unless (and (>= (length line) 4) (string= "info" (subseq line 0 4)))
          (format t "~A : ~A~%" engine-name line)))
      ;; Quits the chess engine.
      (quit-chess-engine process)
      (do ((line (read-line process-output nil :eof)
                 (read-line process-output nil :eof)))
          ((eq line :eof))
        (format t "~A~%" line))
      best-moves)))
