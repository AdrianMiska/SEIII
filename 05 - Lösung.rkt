#lang racket
(require se3-bib/butterfly-module)
;Die unterschiedlichen Merkmalstypen als Listen dargestellt
(define Musterung '(Sterne Punkte Streifen))
(define Flügelfarbe '(blat grün gelb rot))
(define Fühlerform '(gekrümmt geschweift gerade))
(define Flügelform '(eliptisch rhombisch hexagonal))
;Hilfsfunktion von Seite 145 des Skripts
(define (member? item xs)
  (if (null? xs)
      #f
      (or (equal? item (car xs))
           (member? item (cdr xs)))))
;Funktion, die zu einem gegebenen Merkmal A eine Liste aller rezessiven Merkmale ausgibt
(define (rezessiveMerkmale A)
  (cond [(member? A Musterung) (member A Musterung)]
        [(member? A Flügelfarbe) (member A Flügelfarbe)]
        [(member? A Fühlerform) (member A Fühlerform)]
        [(member? A Flügelform) (member A Flügelform)]
        [else '()]))
;Funktion, die #t genau dann ausgibt, wenn das Merkmal A dominant gegenüber dem Merkmal B ist
(define (dominant? A B)
  (member? B (rezessiveMerkmale A)))
;Funktion, die #t genau dann ausgibt, wenn das Merkmal A rezessiv gegenüber dem Merkmal B ist
(define (rezessiv? A B)
  (member? A (rezessiveMerkmale B)))
