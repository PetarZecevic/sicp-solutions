#lang racket

;N queens problem.

(define (board-position row column)
  (list row column))

(define (get-row position)
  (car position))

(define (get-column position)
  (cadr position))

(define empty-board null)

(define (enumerate-interval start end)
  (if (> start end)
      null
      (cons start (enumerate-interval (+ start 1) end))))

(define (filter predicate sequence)
  (cond ((null? sequence) sequence)
        ((predicate (car sequence)) (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append
              null
              (map proc seq)))

;Proverava da li dve pozicije leze na istoj dijagonali na tabli.
(define (diagonal? position-1 position-2)
  (= (abs (- (get-row position-1)
             (get-row position-2)))
     (abs (- (get-column position-1)
             (get-column position-2)))))
  
(define (safe? k positions)
  (let ((k-queen (car positions))
        (rest-queens (cdr positions)))
    (accumulate (lambda (x y) (and x y))
                #t
                (map (lambda (position)
                       (cond ((= (get-row position) (get-row k-queen)) #f)
                             ((diagonal? k-queen position) #f)
                             (else #t))) rest-queens))))

(define (adjoin-position row column rest)
  (append (list (board-position row column)) rest))

;One way to solve the puzzle is to work across the board, placing a
;queen in each column. Once we have placed k - 1 queens, we must place the kth queen in a position
;where it does not check any of the queens already on the board. We can formulate this approach
;recursively: Assume that we have already generated the sequence of all possible ways to place k - 1
;queens in the first k - 1 columns of the board. For each of these ways, generate an extended set of
;positions by placing a queen in each row of the kth column. Now filter these, keeping only the
;positions for which the queen in the kth column is safe with respect to the other queens. This produces
;the sequence of all ways to place k queens in the first k columns.
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))