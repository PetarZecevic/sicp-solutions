#lang racket

;Skalira sve listove stabla zadatom vrednoscu i vraca identicno stablo sa skaliranim listovima.
(define (scale-tree tree factor)
  (cond ((null? tree) tree) ;Prazno stablo
        ((not (pair? tree)) (* tree factor)) ;Naisao na list.
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor))))) ;Spaja skaliranu levu i skaliranu desnu stranu.

;Mapiranje stabla.
(define (map-tree proc tree)
  (cond ((null? tree) tree)
        ((not (pair? tree)) (proc tree))
        (else (cons (map-tree proc (car tree))
                    (map-tree proc (cdr tree))))))

;Skaliranje realizovano uspomoc mapiranja.
(define (scale-tree-1 tree factor)
  (map-tree (lambda (x) (* x factor)) tree))

(define t-1 (list 1 2 3 4 (list 1 2 3 4 (list 1 2 3 4)) (list 1 2 3 4)))
(scale-tree t-1 2)
(scale-tree-1 t-1 2)

;Skaliranje stabla realizovano uspomoc mapiranja lista.
;Ideja je da se mapiraju podliste stabla, jer stablo predstavlja listu lista.

;Mapiranje liste.
(define (map proc items)
  (if (null? items)
      items
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (scale-tree-2 tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor))) tree))

(scale-tree-2 t-1 2)