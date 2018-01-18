#lang racket

;;; Aufgabe 1

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


