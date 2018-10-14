(defpackage #:cl-chess/graphics
  (:use #:cl #:zombie-raptor)
  (:import-from #:pngload
                #:data
                #:height
                #:load-file
                #:width)
  (:import-from #:static-vectors
                #:make-static-vector)
  (:import-from #:uiop
                #:make-pathname*
                #:merge-pathnames*)
  (:export #:make-chess-graphics
           #:shader-data*
           #:square-model
           #:textures
           #:update-visual-board))

(in-package #:cl-chess/graphics)

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

(defun make-chess-graphics (&key ecs hud-ecs mesh-keys width height)
  (declare (ignore width))
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
(defun update-visual-board (hud-ecs move)
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
