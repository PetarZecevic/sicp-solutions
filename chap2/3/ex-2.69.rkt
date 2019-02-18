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

;Konstruisanje uredjene liste u rastucem poretku, ulazni podatak je lista parova simbol-vrednost
;npr: ((A 2) (B 3) (D 1) (E 1))
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
              (adjoin-set x (cdr set))))))
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) ;symbol
                               (cadr pair)) ;frequency
                    (make-leaf-set (cdr pairs))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
(define (successive-merge set)
  (define (one-item-set? set) (null? (cdr set)))
  (if (one-item-set? set)
      set
      (let ((tree (make-code-tree (car set)
                                  (cadr set))))
        (successive-merge (adjoin-set tree (cddr set))))))

(generate-huffman-tree (list (list 'A 3) (list 'B 2) (list 'C 1) (list 'D 1)))