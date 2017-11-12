#lang racket
;;;Aufgabe 1
;1.) 'Flocki , da die Variable wuff den Wert 'Flocki hat.
;2.) 'Flocki , da die Variable Hund denselben Wert, wie die Variable wuff hat.
;3.) 'wuff , da die Variable Wolf den Wert 'wuff hat.
;4.) 'Hund , da die Funktion quote aus Hund das Symbol 'Hund macht.
;5.) 'Flocki , da die Variable Wolf den Wert 'wuff hat. 'wuff ausgewertet ergibt die Variable wuff, welche den Wert 'Floci hat.
;6.) undefined , da das Symbol 'Flocki nicht ausgewertet werden kann(Die Variable Flocki wurde nicht definiert).
;7.) 'wuff , da 'Wolf ausgewertet die Variable Wolf gibt, welche den Wert 'wuff hat.
;8.) 'lily , da let beiden Variablen gleichzeitig einen Wert zuweist, wird für die Zuweisung von PersonC die Eingabe PersonA benutzt, welche den Wert 'lily hat.
;9.) (wuff Hund) , da die Funktion cdr dreimal angewendet wird und dementsprechend die ersten drei Elemente der Liste wegfallen.
;10.) '(Flocki)
;11.) 'Flocki  , da cdr das zweite Element der Liste ausgibt, welche mit mit cons erstellt wird. Das zweite Element hat gerade den Wert 'Flocki.
;12.) 1/2
;13.) 'Wolf , da die Funktion welcherNameGiltWo 'Wolf ausgibt. Dazu wird dann das Symbol erstellt und mit eval ausgewertet, was einfach wieder 'Wolf ergibt.
;14.) 'Flocki , da die Funktion welcherNameGiltWo 'Hund ausgibt, was mit eval ausgewertet wird und daher die Variable Hund ausgibt, welche den Wert 'Flocki hat.

(define wuff 'Flocki)
(define Hund wuff)
(define Wolf 'wuff)
(define (welcherNameGiltWo PersonA PersonB)
   (let
        ((PersonA 'Zaphod)
         (PersonC PersonA))
      PersonC))
(define xs1 '(0 2 3 wuff Hund))
(define xs2 (list wuff Hund))
(define xs3 (cons Hund wuff))

;;;Aufgabe 2.1
(define (Fakultät n)
  (if (= n 0)
      1
      (* n (Fakultät (- n 1)))))

;;;Aufgabe 2.2
(define (Power r n)
  (if (= n 0)
      1
      (cond [(odd? n) (* r (Power r (- n 1)))]
            [(even? n)(sqr (Power r (/ n 2)))])))

;;;Aufgabe 2.3
;Funktion, die die Eulerzahl auf n Stellen genau berechnet
(define (Eulerzahl n)
  (letrec ([EulerzahlInner (lambda (k n)
                           (if (< (/ k (Fakultät (- k 1))) (/ 1 (Power 10 n)))
                               (/ k (Fakultät (- k 1)))
                               (+ (/ k (Fakultät (- k 1))) (EulerzahlInner (+ k 1) n))))])
  (/ (EulerzahlInner 1 n) 2)))
;;;Aufgabe 2.4
;Die Funktion PiNäherung berechnet die Zahl Pi bis auf n Stellen genau
(define (PiNäherung n)
  (letrec ([PiNäherungInner (lambda (k n)
                              (cond [(odd? k) (if (< (/ 1 (- (* 2 k) 1)) (/ 1 (Power 10 n)))
                                                  (/ 1 (- (* 2 k) 1))
                                                  (+ (PiNäherungInner (+ k 1) n) (/ 1 (- (* 2 k) 1))))]
                                    [(even? k) (if (< (/ 1 (- (* 2 k) 1)) (/ 1 (Power 10 n)))
                                                   (- 0 (/ 1 (- (* 2 k) 1)))
                                                   (- (PiNäherungInner (+ k 1) n) (/ 1 (- (* 2 k) 1))))]))])
    (* 4 (PiNäherungInner 1 n))))
;;;Aufgabe 3
;Ergebnisse:
;1.) number
;2.) boolean
;3.) pair
;4.) list
;5.) procedure
;6.) char
;7.) procedure
;8.) procedure
;9.) string
(define (type-of x)
  (cond [(boolean? x) "boolean"]
        [(pair? x) "pair"]
        [(list? x) "list"]
        [(symbol? x) "symbol"]
        [(number? x) "number"]
        [(char? x) "char"]
        [(string? x) "string"]
        [(vector? x) "vector"]
        [(procedure? x) "procedure"]))
(define (id z) z)
