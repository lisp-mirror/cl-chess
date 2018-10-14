;;; todo: bit board

(defpackage #:cl-chess/board
  (:use #:cl #:zombie-raptor)
  (:export #:board
           #:chess-board-ref
           #:make-board
           #:print-board
           #:print-fen-board
           #:update-board))

(in-package #:cl-chess/board)

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