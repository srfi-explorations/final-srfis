;;; array test
;;; 2001 Jussi Piitulainen

;;; An identity matrix.

(define i_4
  (let* ((i (make-array
             (shape 0 4 0 4)
             0))
         (d (share-array i
                         (shape 0 4)
                         (lambda (k)
                           (values k k)))))
    (do   ((k 0 (+ k 1))) ((= k 4))
      (array-set! d k 1))
    i))

(or (array-equal? i_4
                  (build-array
                   (shape 0 4 0 4)
                   (lambda (j k)
                     (if (= j k) 1 0))))
    (error "failed to build i_4"))

(or (array-equal? i_4
                  (array
                   (shape 0 4 0 4)
                   1 0 0 0
                   0 1 0 0 
                   0 0 1 0
                   0 0 0 1))
    (error "failed to array i_4"))

(or (array-equal? (share-array
                   i_4
                   (shape 0 4)
                   (lambda (k)
                     (values k k)))
                  (share-array
                   (array (shape) 1)
                   (shape 0 4)
                   (lambda (k)
                     (values))))
    (error "failed to share diagonal of i_4 or cell of 1"))

(or (array-equal? (share-array
                   i_4
                   (shape 0 4)
                   (lambda (k)
                     (values (- 3 k) k)))
                  (share-array
                   (array (shape) 0)
                   (shape 0 4)
                   (lambda (k)
                     (values))))
    (error "failed to share codiagonal of i_4 or cell of 0"))

(or (array-equal? (share-array
                   i_4
                   (shape 0 2 0 2)
                   (lambda (j k)
                     (values (* 3 j) (* 3 k))))
                  (share-array
                   i_4
                   (shape 0 2 0 2)
                   (lambda (j k)
                     (values (+ j 1) (+ k 1)))))
    (error "failed to share corners or center of i_4"))

(or (array-equal? i_4 (transpose i_4))
    (error "failed to transpose i_4"))

;;; Try a three dimensional transpose. This will also exercise matrix
;;; multiplication.

(define threed123
  (array (shape 0 1 0 2 0 3)
         'a 'b 'c
         'd 'e 'f))

(define threed312
  (array (shape 0 3 0 1 0 2)
         'a 'd
         'b 'e
         'c 'f))

(define rot231
  (list 0 1 0
        0 0 1
        1 0 0))

(or (array-equal? threed123
                  (apply transpose threed312 rot231))
    (error "failed to transpose three dimensions"))


;;; A three dimensional chess board with two phases: piece and colour
;;; of piece. Think of pieces in a cube with height, width and depth,
;;; and piece colours in a parallel cube. We put pink jays around and
;;; grey crows inside the board proper. Later we put in a blue rook.

(define board
  (build-array
   (shape -1 9 -1 9 -1 9 0 2)
   (lambda (t u v w)
     (case w
       ((0) (if (and (< -1 u 8)
                     (< -1 v 8)
                     (< -1 t 8))
                'crow
                'jay))
       ((1) (if (and (< -1 u 8)
                     (< -1 v 8)
                     (< -1 t 8))
                'grey
                'pink))))))

;;; A cylinder with height 4, width 4, depth 6, both phases, centered
;;; inside the board. Top left front corner is at 0 0 0 of cylinder but
;;; 2 2 1 of board.

(define board-cylinder
  (share-array
   board
   (shape 0 4 0 4 0 6 0 2)
   (lambda (t u v w)
     (values (+ t 2) (+ u 2) (+ v 1) w))))

;;; The center cube with side 2 of the cylinder, hence of the board,
;;; with both phases. Top left corner is 0 0 0 of center but 1 1 2
;;; of cylinder and 3 3 3 of board.

(define board-center
  (share-array
   board-cylinder
   (shape 0 2 0 2 0 2 0 2)
   (lambda (t u v w)
     (values (+ t 1) (+ u 1) (+ v 2) w))))

;;; Front face of center cube, in two dimensions plus phase. Top left
;;; corner is 0 0 of face but 0 0 0 of center and 1 1 2 of cylinder
;;; 3 3 3 of board.

(define board-face
  (share-array
   board-center
   (shape 0 2 0 2 0 2)
   (lambda (t u w)
     (values t u 0 w))))

;;; Left side of face in three dimensions plus phase. Top is 0 0 0 of
;;; pillar but 0 0 of face and 0 0 0 of center and 1 1 2 of cylinder
;;; and 3 3 3 of board. Bottom is 1 0 0 of pillar but 1 0 of face and
;;; 1 0 0 of center and 2 1 2 of cylinder and 4 3 3 of board.

(define board-pillar
  (share-array
   board-face
   (shape 0 2 0 1 0 1 0 2)
   (lambda (t u v w)
     (values t 0 w))))

;;; Pillar upside down. Now top 0 0 0 is 1 0 of face and 1 0 0 of center
;;; and 2 1 2 of cylinder and 4 3 3 of board.

(define board-reverse-pillar
  (share-array
   board-pillar
   (shape 0 2 0 1 0 1 0 2)
   (lambda (t u v w)
     (values (- 1 t) u v w))))

;;; Bottom of pillar.

(define board-cubicle
  (share-array
   board-pillar
   (shape 0 2)
   (lambda (w)
     (values 1 0 0 w))))

;;; Top of upside down pair.

(define board-reverse-cubicle
  (share-array
   board-reverse-pillar
   (shape 0 2)
   (lambda (w)
     (values 0 0 0 w))))

;;; Piece phase of cubicle.

(define board-piece
  (share-array
   board-cubicle
   (shape)
   (lambda ()
     (values 0))))

;;; Colour phase of the other cubicle that is actually the same cubicle.

(define board-colour
  (share-array
   board-reverse-cubicle
   (shape)
   (lambda ()
     (values 1))))

;;; Put a blue rook at the bottom of the pillar and at the top of the
;;; upside pillar.

(array-set! board-piece 'rook)
(array-set! board-colour 'blue)

;;; Build the same chess position directly.

(define board-two
  (build-array
   (shape -1 9 -1 9 -1 9 0 2)
   (lambda (t u v w)
     (if (and (= t 4) (= u 3) (= v 3))
         (case w
           ((0) 'rook)
           ((1) 'blue))
         (case w
           ((0) (if (and (< -1 u 8)
                         (< -1 v 8)
                         (< -1 t 8))
                    'crow
                    'jay))
           ((1) (if (and (< -1 u 8)
                         (< -1 v 8)
                         (< -1 t 8))
                    'grey
                    'pink)))))))

(or (array-equal? board board-two)
    (error "failed in three dimensional chess"))

;;; Permute the dimensions of the chess board in two different ways.
;;; The transpose also exercises matrix multiplication.

(define board-three
  (share-array
   board-two
   (shape 0 2 -1 9 -1 9 -1 9)
   (lambda (w t u v)
     (values t u v w))))

(or (array-equal? board-three
                  (transpose board-two
                             0 0 0 1
                             1 0 0 0
                             0 1 0 0
                             0 0 1 0))
    (error "failed to permute chess board dimensions"))

(or (array-equal? (share-array
                   board-two
                   (shape -1 9 0 2 -1 9 -1 9)
                   (lambda (t w u v)
                     (values t u v w)))
                  (transpose board-two
                             1 0 0 0
                             0 0 0 1
                             0 1 0 0
                             0 0 1 0))
    (error "failed to permute chess board dimensions another way"))

;;; Just see that empty share does not crash. No index is valid. Just by
;;; the way. There is nothing to be done with it.

(define board-nothing
  (share-array
   board
   (shape 0 3 1 1 0 3)
   (lambda (t u v)
     (values 0 0 0))))
