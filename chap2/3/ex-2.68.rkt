#lang racket
;Listovi stabla su parovi simbol-tezina.
;Tezina predstavlja frekvenciju simbola.
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

;Skup simbola predstavljamo preko liste simbola.
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;Dekodiranje informacije uspomoc stabla.
;Pocinje se od prvog bita, 0 oznacava operaciju prelaska na levu granu stabla,
;a 1 operaciju prelaska na desnu granu stabla.
;Kada se stigne do liste, ubacuje se u listu dekodovanih informacija,
;i dekodiranje pocinje od narednog bita sve dok se ne dodje do kraja liste bitova.
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

;Enkodiranje poruke.
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (found-sym? sym symbols)
    (cond ((null? symbols) false)
          ((eq? sym (car symbols)) true)
          (else (found-sym? sym (cdr symbols)))))
  (define (encode-1 tree)
    (cond ((leaf? tree) '())
          ((found-sym? symbol (symbols (left-branch tree))) (cons '0 (encode-1 (left-branch tree))))
          ((found-sym? symbol (symbols (right-branch tree))) (cons '1 (encode-1 (right-branch tree))))))
  (if (not (found-sym? symbol (symbols tree)))
      (error symbol " not found in code-tree")
      (encode-1 tree)))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(define decoded-message (decode sample-message sample-tree))
(define encoded-message (encode decoded-message sample-tree))
(print "Kod poruke: ")
(print sample-message)
(newline)
(print "Dekodiranje: ")
(print decoded-message)
(newline)
(print "Enkodiranje: ")
(print encoded-message)