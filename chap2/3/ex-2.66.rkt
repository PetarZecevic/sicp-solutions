#lang racket

;Baza podataka, realizovan kao skup parova kljuc-vrednost.
;Interna reprezentacija je u vidu binarnog stabla.
(define (entry-record set-of-records) (car set-of-records))
(define (key record) (car record))
(define (data record) (cadr record))

(define (left-branch set-of-records) (cadr set-of-records))
(define (right-branch set-of-records) (caddr set-of-records))
(define (make-record key data left right)
  (list (list key data) left right))


;Pretraga po kljucu u bazi podataka.
(define (lookup given-key set-of-records)
  (if (null? set-of-records)
      false
      (let ((entry-key (key (entry-record set-of-records))))
        (cond ((= given-key entry-key) (entry-record set-of-records))
              ((> given-key entry-key) (lookup given-key (right-branch set-of-records)))
              ((< given-key entry-key) (lookup given-key (left-branch set-of-records)))))))

(define r1 (make-record 3 'Petar
                        (make-record 2 'Milan
                                     (make-record 1 'Jovan '() '())
                                     '())
                        (make-record 4 'Mihajlo
                                     '()
                                     (make-record 5 'Zeljko '() '()))))

(lookup 1 r1)
(lookup 2 r1)
(lookup 3 r1)
(lookup 4 r1)
(lookup 5 r1)
(lookup 6 r1)