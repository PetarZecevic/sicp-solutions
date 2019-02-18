#lang racket

;Obrtanje stabla.
;Podrazumeva obrtanje redosleda listi koje cine stablo kao i rekurzivno obrtanje samih lista.
(define (deep-reverse tree)
  (cond ((null? tree) tree) ;Prazno stablo je bazni slucaj.
        ((pair? (car tree)) (append (deep-reverse (cdr tree)) ;Slucaj kada je prvi element liste lista. Pravimo par lista cons, da bi lista ocuvala strukturu.
                                    (cons (deep-reverse (car tree))
                                          null)))
        (else (append (deep-reverse (cdr tree)) ;Slucaj kada prvi element nije lista, tada pravi listu sa jednim elemntom i spajamo je sa ostatkom stabla.
                      (list (car tree))))))

(define t1 (list (list (list 1 2) (list 3 4)) (list (list 5 6) (list 7 8))))
(deep-reverse (deep-reverse t1))
(deep-reverse t1)

;Bolja i razumnija heuristika.
;Kreni kroz stablo, ako naidjes na list nadovezi ga na pocetak.
;Ako naidjes na listu, obrni je pa je nadovezi na pocetak.
(define (d-reverse tree)
  (reverse (map (lambda (item)
                  (if (pair? item)
                      (d-reverse item)
                      item)) tree)))