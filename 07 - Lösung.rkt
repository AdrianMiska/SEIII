#lang racket
(require racket/trace)

;;;Aufgabe 1

(define (zaehlen1 x xs)
  (if (null? xs)
      0
      (if (eq?(car xs) x)
          (+ 1 (zaehlen1 x (cdr xs)))
          (zaehlen1  x (cdr xs)))))

(define (zaehlen2 x xs)
  (letrec ([zaehlenInner (λ (x xs acc)
                           (if (null? xs)
                               acc
                               (begin
                                 (if (eq? (car xs) x)
                                     (zaehlenInner x (cdr xs) (+ 1 acc))
                                     (zaehlenInner x (cdr xs) acc)))))])
    (zaehlenInner x xs 0)))


(define (zaehlen3 x xs)
  (letrec ([count (λ (y counter)
                    (if (eq? y x)
                        (+ counter 1)
                        counter))])
    (foldl count 0 xs)))


(zaehlen1 3 '(2 4 3 2 3 4 1))
(zaehlen2 3 '(2 4 3 2 3 4 1))
(zaehlen3 3 '(2 4 3 2 3 4 1))


;;;Aufgabe 2

