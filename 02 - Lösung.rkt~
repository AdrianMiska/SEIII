#lang racket

"Tafel"
(/
 (/ (sin 3) (cos 3))
 (tan 3))

"Aufgabe 1.3"

(+ 1 4)
(+ 1 4 3 6)
(- 3 4 5)
(/
 (+ (* 1 4) (* 5 6))
 (+ 3 (* 4 5) 6))

(define x 3)
(/ (sin x) (cos x))
(- 1 (expt (sin x) 2))


"Aufgabe 2.1"

(define (getInterest rate amount years)
  (let ([x (+ amount (* amount rate))])
  (if (= years 1)
      x
      (getInterest rate x (- years 1)))))


(getInterest 0.02 3000 1)


"Tafel"

(define (sumBelow n acc)
  (if (= n 1)
      (+ n acc)
      (sumBelow (- n 1) (+ acc n))))


(sumBelow 10 0)