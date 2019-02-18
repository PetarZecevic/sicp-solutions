#lang racket

;Akumulator, akumulira po zadatoj operaciji vrednosti liste na pocetnu vrednost.
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))
;Verzija koja radi nad listom lista,sve podliste su iste duzine i kombinuju se korespodentni elementi i
;popunjavaju novu listu, koja se vraca kao povratna vrednost.
(define (accumulate-n op initial seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op initial (map (lambda (item) (car item)) seqs))
            (accumulate-n op initial (map (lambda (item) (cdr item)) seqs)))))

(define test-equal (lambda (x y)
                     (cond ((and (pair? x) (pair? y)) (and (test-equal (car x) (car y))
                                                           (test-equal (cdr x) (cdr y))))
                           ((xor (pair? x) (pair? y)) #f)
                           ((xor (null? x) (null? y)) #f)
                           ((and (null? x) (null? y)) #t)
                           (else (= x y)))))

;Testiranje procedure test-equal.
(display "1: ")(display (test-equal (list 1 2 3)
                                    (list 1 2 3)))(display " - #t")(newline)

(display "2: ")(display (test-equal (list 1 2 3)
                                    (list 1 2 2)))(display " - #f")(newline)

(display "3: ")(display (test-equal (list 1 2 3 4)
                                    (list 1 2 3)))(display " - #f")(newline)

(display "4: ")(display (test-equal (list 1)
                                    1))(display " - #f")(newline)

(display "5: ")(display (test-equal (list 1 (list 2 3))
                                    (list 1 (list 2 3))))(display " - #t")(newline)

(display "6: ")(display (test-equal (list 1 (list 2 3 4))
                                    (list 1 (list 2 3))))(display " - #f")(newline)

(display "7: ")(display (test-equal null
                                    null))(display " - #t")(newline)

(display "8: ")(display (test-equal 1
                                    null))(display " - #f")(newline)

(display "9: ")(display (test-equal (list (list (list 1)))
                                    (list (list (list 1)))))(display " - #t")(newline)

(display "10: ")(display (test-equal (list (list (list 1)))
                                    (list (list 1))))(display " - #f")(newline)

;Testiranje procedure accumulate-n.
(test-equal (accumulate-n +
                          0
                          (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
            (list 22 26 30))

(test-equal (accumulate-n cons
                        null
                        (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
            (list (list 1 4 7) (list 2 5 8) (list 3 6 9)))

(test-equal (accumulate-n *
                          1
                          (list (list 1) (list 2)))
            (list 2))