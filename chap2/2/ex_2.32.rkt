#lang racket

;Procedura koja za dati skup predstavljen preko liste, vraca listu svih podskupova skupa.
(define (one-subset set)
  (if (null? set)
      set
      (cons (list (car set))
            (one-subset (cdr set)))))

(define (make-two curr-sub rem-set)
    (if (null? rem-set)
        rem-set
        (cons (list curr-sub (car rem-set))
              (make-two curr-sub (cdr rem-set)))))

(define (two-subsets set)
  (make-two (car set) (cdr set)))

(define (two-subsets-1 set)
  (if (null? set)
      set
      (append (make-two (car set) (cdr set))
              (two-subsets-1 (cdr set)))))

;Mapiranje stabla, podrazumeva primenu procedura na svakom listu stabla i vracanje izmenjenog stabla.
(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree))) tree))

(define (make-subset cur rem)
  (let ((l-cur (if (pair? cur) cur (list cur))))
    (cons l-cur
          (map (lambda (item)
                 (let ((l-item (list item)))
                   (append l-cur l-item))) rem))))

(define (two-element-subset set)
  (if (null? set)
      null
      (append (make-subset (car set) (cdr set))
              (two-element-subset (cdr set)))))


;Korektno resenje:
;Heuristika:
;Uzmi sve podskupove bez prvog elementa.
;Povezi te podskupove sa istim podskupovima popunjenim prvim elementom.
(define (subsets s)
  (if (null? s)
      (list s)
      (let ((rest (subsets (cdr s))))
        (append rest
                (map (lambda (item)
                       (cons (car s) item)) rest)))))