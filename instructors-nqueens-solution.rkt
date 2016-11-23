;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname nqueens-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

;; nqueens-solution.rkt

;; A simple solution to the generic nqueens problem.
;;  - uses lambda to reduce and simplify the amount of code


;; Data definitions:

;; Position is Natural[0, (sqr N))
;; interp. positions on the board
;;         N is the number of queens
;;         so (sqr N) is the number of positions on the board
(define P1 0)        ;upper left corner of board
(define P2 (- 16 1)) ;lower left corner of 4x4 board

;; Board is (listof Position)  up to N elements long
;; interp. the positions of the queens that have been placed on the board
(define BD1 empty)           ;no queens placed
(define BD2 (list 0))        ;one queen in upper left corner
(define BD3 (list 14 8 7 1)) ;a solution to 4x4 puzzle 




;; Functions:

;; NOTE about following function. It could have been designed with N as a top-level
;; constant and all the locally defined functions as top-level functions. But doing 
;; it the way we have done below, makes it easy to make the top-level nqueens function
;; consume N which is kind of nice. The trampoline starts the actual search out by 
;; calling fn-for-bd with an empty board.


;; Natural[>0] -> Board or false
;; produce a solution for n queens of size N; or false if no solution is possible
;; solution produced is the first one encountered in depth-first left to right 
;; traversal of assigning queens to board squares
(check-expect (nqueens 1) (list 0))
(check-expect (nqueens 2) false)
(check-expect (nqueens 3) false)
(check-expect (nqueens 4) (list 14 8 7 1))
(check-expect (nqueens 5) (list 23 16 14 7 0))
(check-expect (nqueens 6) (list 34 26 18 17 9 1))
(check-expect (nqueens 7) (list 47 38 29 27 18 9 0))
(check-expect (nqueens 8) (list 59 49 46 34 29 23 12 0))


(define (nqueens N)                                      ;For any given call to nqueens
  ;                                                      ;N and ALL-POS are constants.
  (local [(define ALL-POS (build-list (sqr N) identity)) ;So we name them as constants.
          
          
          ;; Termination argument:
          ;; Trivial cases:
          ;;   bd is solved or there are no valid next boards left to explore
          ;; 
          ;; Reduction step:
          ;;   (fn-for-lobd (next-boards bd)) in other words go explore the
          ;;   valid next boards of this board
          ;; 
          ;; Since board is finite, and each board is explored at most once, 
          ;; search will definitely terminate. (But the search space does grow
          ;; really fast!)
          
          
          ;; Board -> Board or false
          ;; do backtracking generative search of board space looking for solution
          (define (fn-for-bd bd)
            (if (solved? bd)
                bd
                (fn-for-lobd (next-boards bd))))
          
          (define (fn-for-lobd lobd)
            (cond [(empty? lobd) false]
                  [else
                   (local [(define try (fn-for-bd (first lobd)))]
                     (if (not (false? try))
                         try
                         (fn-for-lobd (rest lobd))))]))
          
          
          ;; Board -> Boolean
          ;; Produce true if board has N queens.
          ;; ASSUMPTION: Board is valid.
          (define (solved? bd) (= (length bd) N))
          
          
          ;; Board -> (listof Board)
          ;; produce next valid boards by adding a queen at every position that does not
          ;; conflict with one of the existing queens
          
          ;; p1 is a position after current max and that does not attack any queen in bd
          ;; p2 is a position in ALL-POS after current max queen
          ;; p3 is a position in bd
          ;; p4 is a position in ALL-POS
          (define (next-boards bd)
            (local [(define max-so-far (foldr max -1 bd))]
              (map (lambda (p1) (cons p1 bd)) 
                   (filter (lambda (p2)                        ;only use positions that
                             (andmap (lambda (p3)              ;do not attack existing 
                                       (not (attack? p2 p3)))  ;queens
                                     bd))
                           (filter (λ (p4) (> p4 max-so-far))  ;only use positions after
                                   ALL-POS)))))                ;current last queen
                    
          
          ;; Position Position -> Boolean
          ;; produce true if queens at position a and b attack each other
          (define (attack? pa pb)
            (local [(define x1 (pos-x pa))
                    (define y1 (pos-y pa))
                    (define x2 (pos-x pb))
                    (define y2 (pos-y pb))]
              (or (= x1 x2)                           ;same row
                  (= y1 y2)                           ;same column
                  (= (/ (- x2 x1) (- y2 y1))  1)      ;same slope  1 diagonal
                  (= (/ (- x2 x1) (- y2 y1)) -1))))   ;same slope -1 diagonal
          
          
          ;; Pos -> Natural[0, N)
          ;; produce the row or column number for the given position
          (define (pos-x p) (remainder p N))
          (define (pos-y p) (quotient  p N))]
    
    (fn-for-bd empty)))

