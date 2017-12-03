#lang racket
(require se3-bib/butterfly-module)
;Die unterschiedlichen Merkmalstypen als Listen dargestellt
(define Flügelfarbe '(blue green yellow red))
(define Musterung '(star dots stripes))
(define Fühlerform '(curved curly straight))
(define Flügelform '(ellipse rhomb hexagon))
;Hilfsfunktion von Seite 145 des Skripts, die überprüft, ob ein Element in einer Liste enthalten ist
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
;Funktion, die bei Eingabe zweier Merkmale das dominante der beiden Merkmale ausgibt
(define (dominantesMerkmal A B)
  (if (dominant? A B)
      A
      B))
;Funktion, die bei Eingabe zweier Merkmale das rezessive der beiden Merkmale ausgibt
(define (rezessivesMerkmal A B)
  (if (dominant? A B)
      B
      A))
;Datenstruktur butterfly. Hierbei sollen sichtbar und unsichtbar zwei Listen sein, die jeweils ein Merkmal
;jedes Merkmaltyps enthalten
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
;Funktion, die einen Schmetterling nach seinen dominanten Merkmalen zeichnet
(define (zeichne A)
  (let ([sichtbar (gibDominant A)])
    (apply show-butterfly sichtbar)))
;Hilfsfunktion: Die Eingabe soll eine Liste sein, die aus 8 Merkmalen besteht, wobei zu jedem Merkmaltyp genau zwei Merkmale
;enthalten sein sollen, die in der Liste bereits nebeneinander stehen. Die Hilfsfunktion sortiert die liste nun so um,
;dass jeweils das dominante Merkmal links neben dem zugehörigen rezessiven Merkmal steht.
(define (Hilfsfunktion1 xs)
  (letrec ([HilfsfunktionInner (lambda (k ys acc)
                                 (if (< 0 k)
                                     (HilfsfunktionInner
                                      (- k 1)
                                      ys
                                      (cons
                                       (dominantesMerkmal (list-ref ys (* 2 k)) (list-ref ys (+ 1 (* 2 k))))
                                       (cons
                                        (rezessivesMerkmal (list-ref ys (* 2 k)) (list-ref ys (+ 1 (* 2 k))))
                                        acc)))
                                     (cons
                                       (dominantesMerkmal (list-ref ys (* 2 k)) (list-ref ys (+ 1 (* 2 k))))
                                       (cons
                                        (rezessivesMerkmal (list-ref ys (* 2 k)) (list-ref ys (+ 1 (* 2 k))))
                                        acc))))])
    (HilfsfunktionInner 3 xs '())))
;Eine weitere Hilfsfunktion: Die Eingabe soll wieder eine Liste sein, die dieselben Anforderungen erfüllt, wie die Eingabeliste
;der ersten Hilfsfunktion. Die Liste wird nun mithilfe von Hilfsfunktion1 umsortiert und danach wird der dazugehörige Schmetterling
;erstellt
(define (Hilfsfunktion2 xs)
  (let ([ys (Hilfsfunktion1 xs)])
    (make-butterfly
     (list
      (list-ref ys 0)
      (list-ref ys 2)
      (list-ref ys 4)
      (list-ref ys 6))
     (list
      (list-ref ys 1)
      (list-ref ys 3)
      (list-ref ys 5)
      (list-ref ys 7)))))
;Funktion, die bei Eingabe zweier Schmetterlinge ein Kind generiert. Hierbei wird zuerst eine Liste erstellt, die die zufällig
;ausgewählten Merkmale der Elternteile enthält. Mit Hilfsfunktio2 wird dann der dazugehörende Schmetterling generiert.
(define (generiereKind V M)
  (letrec ([Inner (lambda (k A B C D acc)
                    (if (< 0 k)
                        (Inner
                         (- k 1)
                         A B C D 
                         (cons
                          (list-ref (list (list-ref A k) (list-ref C k)) (random 2))
                          (cons
                           (list-ref (list (list-ref B k) (list-ref D k)) (random 2))
                           acc)))
                         (cons
                          (list-ref (list (list-ref A k) (list-ref C k)) (random 2))
                          (cons
                           (list-ref (list (list-ref B k) (list-ref D k)) (random 2))
                           acc))))])
    (Hilfsfunktion2 (Inner 3 (gibDominant V) (gibRezessiv V) (gibDominant M) (gibRezessiv M) '()))))
;Funktion, die bei Eingabe zweier Schmetterlinge und einer natürlichen Zahl, die mindestens 1 ist, entsprechend
;viele Kinder generiert und diese direkt zeichnet
(define (generiereKinder V M n)
  (letrec ([Inner (lambda (V M k acc)
                    (if (< 1 k)
                        (Inner
                         V M (- k 1) (cons (generiereKind V M) acc))
                        (cons (generiereKind V M) acc)))])
    (map zeichne (Inner V M n '()))))

(generiereKinder (createButterfly 'red 'stripes 'curved 'hexagon) (createButterfly 'yellow 'star 'curly 'rhomb) 100)