#lang racket
(require se3-bib/butterfly-module)
;Die unterschiedlichen Merkmalstypen als Listen dargestellt
(define Flügelfarbe '(blue green yellow red))
(define Musterung '(star dots stripes))
(define Fühlerform '(curved curly straight))
(define Flügelform '(ellipse rhomb hexagon))
;Hilfsfunktion von Seite 145 des Skripts
(define (member? item xs)
  (if (null? xs)
      #f
      (or (equal? item (car xs))
           (member? item (cdr xs)))))
;Funktion, die zu einem gegebenen Merkmal A eine Liste aller rezessiven Merkmale ausgibt
(define (rezessiveMerkmale A)
  (cond [(member? A Flügelfarbe) (member A Flügelfarbe)]
        [(member? A Musterung) (member A Musterung)]
        [(member? A Fühlerform) (member A Fühlerform)]
        [(member? A Flügelform) (member A Flügelform)]
        [else '()]))
;Funktion, die #t genau dann ausgibt, wenn das Merkmal A dominant gegenüber dem Merkmal B ist
(define (dominant? A B)
  (member? B (rezessiveMerkmale A)))
;Funktion, die #t genau dann ausgibt, wenn das Merkmal A rezessiv gegenüber dem Merkmal B ist
(define (rezessiv? A B)
  (member? A (rezessiveMerkmale B)))
;Datenstruktur butterfly
(define-struct butterfly (sichtbar unsichtbar))
;Funktion, die einen Schmetterling anhand der eingegebenen sichtbaren Merkmale mit zufällig rezessiven Merkmalen erstellt
(define (createButterfly Farbe Muster Fühler Flügel)
  (let ([rezessivFarbe (rezessiveMerkmale Farbe)]
        [rezessivMuster (rezessiveMerkmale Muster)]
        [rezessivFühler (rezessiveMerkmale Fühler)]
        [rezessivFlügel (rezessiveMerkmale Flügel)])
  (make-butterfly
   (list Farbe Muster Fühler Flügel)
   (list
    (list-ref rezessivFarbe (random (length rezessivFarbe)))
    (list-ref rezessivMuster (random (length rezessivMuster)))
    (list-ref rezessivFühler (random (length rezessivFühler)))
    (list-ref rezessivFlügel (random (length rezessivFlügel)))))))
;Funktion, die die dominanten/sichtbaren Merkmale eines Schmetterlings ausgibt
(define (gibDominant A)
  (if (butterfly? A)
      (butterfly-sichtbar A)
      "gibDominant braucht einen Schmetterling als Eingabe"))
;Funktion, die die rezessiven/unsichtbaren Merkmale eines Schmetterlings ausgibt
(define (gibRezessiv A)
  (if (butterfly? A)
      (butterfly-unsichtbar A)
      "gibDominant braucht einen Schmetterling als Eingabe"))
