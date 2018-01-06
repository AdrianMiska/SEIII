#lang racket

#|
Aufgabe 1

1. Eine funktion höherer Ordnung, ist eine Funktion, die andere Funktionen
als Parameter übernimmt oder Funktionen als Rückgabewerte liefert.

2.

a) ist keine.
b) ist eine F. höherer Odnung, da map als ersten Parameter eine Fuktion entgegennimmt,
um sie auf die Elemente einer Liste (zweiter Parameter) anzuwenden.
c) ist keine.
d) ist eine F.  höherer Ordnung, da sie eine Funktion zurückgibt (entweder "<", ">" oder "=").
e) ist eine F. höherer Ordnung, da sie eine Funktion zurückgibt.

3.

Wenn wir (schweinchen-in-der-mitte list 4) aufrufen, bekommen wir eine Funktion zurück, die zwei Parameter entgegennimmt.
Diese Funktion rufen wir mit den Parametern (1 3) auf. Im funktionalen Abschluss der zurückgegebenen Methode ist arg1 mit dem Wert belegt,
den wir beim Aufruf von schweinchen-in-der-mitte, in unserem Fall 4, gesetzt haben.

4.


TODO

|#

;;;Aufgabe 2

(define xs (list 1 2 3 4 5 6 7 8 9 10 11 22 33 81))

;1.
(map (λ (x) (* x x)) xs)

;2.
(filter (λ (x)
          (cond [(zero? (remainder x 11)) x]
                [(zero? (remainder x 9)) x]
                [else #f]))
        xs)

;3.
(foldl + 0
       (filter (λ (x)
                 (if (and (not (zero? (remainder x 2))) (> x 6)) x #f))
               xs))

;4.
(define predicateAndInverse (λ (p? xs)
                              (cons
                               (filter p? xs)
                               (filter (negate p?) xs))))

(predicateAndInverse odd? xs)



