#lang racket

;;; Aufgabe 1.1

(define spiel #(0 0 0 0 0 9 0 7 0
                  0 0 0 0 8 2 0 5 0
                  3 2 7 0 0 0 0 4 0
                  0 1 6 0 4 0 0 0 0
                  0 5 0 0 0 0 3 0 0
                  0 0 0 0 9 0 7 0 0
                  0 0 0 6 0 0 0 0 5
                  8 0 2 0 0 0 0 0 0
                  0 0 4 2 0 0 0 0 8))

(define xy->index (λ (x y) (+ (* 9 y) x)))

(define zeile->indizes (λ (x) (map (curry + (* 9 x))
                                   '(0 1 2 3 4 5 6 7 8)))) ;erste Zeile

(define spalte->indizes (λ (x) (map (curry + x)
                                    '(0 9 18 27 36 45 54 63 72)))) ;erste Spalte

(define quadrant->indizes (λ (x) (map (curry + (+ (* 3 (modulo x 3))
                                                  (* (floor (/ x 3)) 9 3)))
                                      '(0 1 2 9 10 11 18 19 20)))) ;erster Quadrant


(define spiel->einträge (λ (spiel indizes)
                          (map (curry vector-ref spiel) indizes)))

(define 0bis8 '(0 1 2 3 4 5 6 7 8))

(define highest-number (λ (xs)
                         (let ([max (λ (x y) (if (> x y) x y))])
                           (foldl max (car xs) (cdr xs)))))

(define remove0 (λ (xs) (filter (negate zero?) xs)))

(define all-true (λ (xs)
                   (equal? (filter (negate false?) xs) xs)))

(define liste->konsistent? (λ (xs)
                             (and
                              (< (highest-number xs) 10)
                              (equal? (remove-duplicates(remove0 xs)) (remove0 xs))))) ; konsistent = nur kleiner 10 und außer 0 keine doppelt
  
(define liste->gelöst? (λ (xs)
                         (and
                          (liste->konsistent? xs)
                          (equal? (remove0 xs) xs)))) ;gelöst = konsistent und keine nullen


(define spiel->konsistent? (λ (spiel)
                             (let(
                                  [zeilen (map (curry spiel->einträge spiel) (map zeile->indizes 0bis8))]
                                  [spalten (map (curry spiel->einträge spiel) (map spalte->indizes 0bis8))]
                                  [quadranten (map (curry spiel->einträge spiel) (map quadrant->indizes 0bis8))])
                               
                               (and
                                (all-true (map liste->konsistent? zeilen))
                                (all-true (map liste->konsistent? spalten))
                                (all-true (map liste->konsistent? quadranten)))))) ;alle konsistent

(define spiel->gelöst? (λ (spiel)
                         (let([zeilen (map (curry spiel->einträge spiel) (map zeile->indizes 0bis8))]
                              [spalten (map (curry spiel->einträge spiel) (map spalte->indizes 0bis8))]
                              [quadranten (map (curry spiel->einträge spiel) (map quadrant->indizes 0bis8))])
                           (and
                            (all-true (map liste->gelöst? zeilen))
                            (all-true (map liste->gelöst? spalten))
                            (all-true (map liste->gelöst? quadranten)))))) ;alle gelöst

;Aufgabe 1.2

(define index->quadrant (λ (x) (+ (* 3 (floor (/ (index->spalte x) 3)))
                                  (floor (/ (modulo (index->zeile x) 9) 3)))))
(define index->spalte (λ (x) (floor(/ x 9))))
(define index->zeile (λ (x) (modulo x 9)))

(define zero-indices (λ (xs)
                       (for/list ([i xs]
                                  [n (in-naturals)]
                                  #:when (zero? i))
                         n)))

(define zahl-möglich (λ (spiel zahl index)
                       (let ([zeile (spiel->einträge spiel (zeile->indizes(index->zeile index)))]
                             [spalte (spiel->einträge spiel (spalte->indizes (index->spalte index)))]
                             [quadrant (spiel->einträge spiel (quadrant->indizes (index->quadrant index)))])
                         (and
                          (liste->konsistent? (cons zahl zeile))
                          (liste->konsistent? (cons zahl spalte))
                          (liste->konsistent? (cons zahl quadrant)))))) ;zahl möglich = passt in alle noch rein

(define my-vector-set! (λ (vec val index)
                         (vector-set! vec index val))) ;convenience method


(define markiere-ausschluss (λ (spiel zahl)
                              (let* ([returnee (vector-copy spiel)]
                                     [0-indices (zero-indices spiel)]
                                     [x-indices (filter (curry zahl-möglich spiel zahl) 0-indices)])
                                (begin
                                  (map (curry my-vector-set! returnee 'X) x-indices)
                                  returnee))))

