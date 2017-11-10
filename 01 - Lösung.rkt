#lang racket

;;; Aufgabe 1.1
(define (degrees->radians deg)
  (* deg (/ pi 180)))

(define (radians->degrees rad)
  (* rad (/ 180 pi)))


;;; Aufgabe 1.2
(define (my-acos x)
  (if (= x 0)
      (/ pi 2)
      (if (< 0 x)
          (atan (sqrt (- (/ 1 (expt x 2)) 1)))
          (- pi (my-acos (abs x))))))


;;; Aufgabe 1.3
(define (nm->km nm)
  (* nm 1.852))


;;; Aufgabe 2.1
(define (distanzAB bA lA bB lB)
  (nm->km
   (*
    (radians->degrees
     (my-acos
      (+
       (*
        (sin
         (degrees->radians bA))
        (sin
         (degrees->radians bB)))
       (*
        (cos
         (degrees->radians bA))
        (cos
         (degrees->radians bB))
        (cos
         (degrees->radians
          (abs (- lA lB))))))))
    60)))

;Die Entfernung zwischen Oslo und Hongkong berechnet sich zu 8782 km
;Die Entfernung zwischen San Francisco und Honolulu berechnet sich zu 3845 km
;Die Entfernung zwischen der Osterinsel und Lima berechnet sich zu 3757 km

;;; Aufgabe 2.3.1
(define (gradToHimmelsrichtung a)
  (define b (modulo a 360))
  (cond [(< b 11.25) "N"]
        [(< b 33.75) "NNO"]
        [(< b 56.25) "NO"]
        [(< b 78.75) "ONO"]
        [(< b 101.25) "O"]
        [(< b 123.75) "OSO"]
        [(< b 146.25) "SO"]
        [(< b 168.75) "SSO"]
        [(< b 191.25) "S"]
        [(< b 213.75) "SSW"]
        [(< b 236.25) "SW"]
        [(< b 258.75) "WSW"]
        [(< b 281.25) "W"]
        [(< b 303.75) "WNW"]
        [(< b 326.25) "NW"]
        [(< b 348.75) "NNW"]
        [else "N"]))

;;; Aufgabe 2.3.2
(define (HimmelsrichtungToGrad a)
  (cond [(eq? a "N") 0]
        [(eq? a "NNO") 22.5]
        [(eq? a "NO") 45]
        [(eq? a "ONO") 67.5]
        [(eq? a "O") 90]
        [(eq? a "OSO") 112.5]
        [(eq? a "SO") 135]
        [(eq? a "SSO") 157.5]
        [(eq? a "S") 180]
        [(eq? a "SSW") 202.5]
        [(eq? a "SW") 225]
        [(eq? a "WSW") 247.5]
        [(eq? a "W") 270]
        [(eq? a "WNW") 292.5]
        [(eq? a "NW") 315]
        [(eq? a "NNW") 337.5]))


