#lang racket

;Liste i gradjenje lista uspomoc parova.
(define lista-1 (cons 1 (cons 2 (cons 3 (cons 4 null))))) ;null - specijalan znak koji oznacava kraj liste.
(define lista-2 (list 1 2 3 4))

;Prvi clan liste.
(car lista-2)

;Svi sem prvog clana.
(cdr lista-2)

;Odabir drugog clana liste.
(car (cdr lista-1))

;Skraceno zapis car-cdr uzastopnih poziva.
;Broj 'a' znakova se odnosi na broj car poziva.
;Broj 'd' znakova se odnosi na broj cdr poziva.
(cadr lista-1)

;Odabir treceg clana liste.
;Odabir n-tog clana liste bi bio ca[d..d](number of d's <=> n-1)r, gde je n prirodan broj.
(caddr lista-2)
(cadddr lista-2)

;Dodavanje elemenata listi.
(cons 10 lista-1)
(cons 5 lista-2)