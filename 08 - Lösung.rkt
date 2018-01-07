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

(foldl (curry + 2) 3 '(3 4 5))
-> 21, da foldl den Startwert 3 mit der Funktion "(curry + 2)" mit jedem Element "inkrementiert" also natürlichsprachlich  3  +2+3  +2+4  +2+5 = 21


(map gerade-oder-ungerade '(4 587 74 69 969 97 459 4))
-> '(ungerade gerade ungerade gerade gerade gerade gerade ungerade), weil map die gegebene Funktion "gerade-oder-ungerade" auf jedes Element der Liste anwendet
   und die Resultate wieder als Liste zurückgibt.

(filter number? '((ab) () 1 (()) 4 -7 "a"))
-> '(1 4 -7), weil filter das Prädikat "number?" auf alle Elemente der Liste anwendet und eine Liste mit allen Elementen zurückgibt, die das Prädikat erfüllen.
   (ab), (), (()) und "a" sind keine Zahlen

((compose (curry foldl + 0) (curry filter (curryr < 0))) '(5682 48 24915 -45 -6 48))
-> -51, weil compose die Funktionen "(curry foldl + 0)" und "(curry filter (curryr < 0)))" aneinanderreiht und dann die Liste als Parameter übergibt.

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


;;; Aufgabe 3
(require se3-bib/setkarten-module)
(require se3-bib/kombinatorik-module)

(define numbers '(1 2 3))
(define patterns '(waves oval rectangle))
(define modes '(outline solid hatched))
(define colors '(red green blue))

;1. Repräsentation einer Spielkarte als Liste von Eigenschaften.
;2. Die Eigenschaften werden mit for*/list kombiniert.

(define deck
  (for*/list ([number numbers] [pattern patterns] [mode modes] [color colors])
    (list number pattern mode color)))

;Anzeigen einer Liste von Karten
(define show-list-of-cards (λ (cards)
                          (map (curry apply show-set-card) cards)))


(define valid-combination (λ (prop1 prop2 prop3)
                           (or (and (eq? prop1 prop2) (eq? prop2 prop3) (eq? prop1 prop3))
                               (and (not (eq? prop1 prop2)) (not (eq? prop2 prop3)) (not (eq? prop1 prop3)) ))))
;3. SET prüfen
(define is-a-set? (λ (cardList)
                    (let ([card1 (car cardList)] [card2 (cadr cardList)] [card3 (caddr cardList)])
                   (and (= 3 (length cardList))
                        (valid-combination (car card1) (car card2) (car card3))
                        (valid-combination (cadr card1) (cadr card2) (cadr card3))
                        (valid-combination (caddr card1) (caddr card2) (caddr card3))
                        (valid-combination (cadddr card1) (cadddr card2) (cadddr card3))))))

(is-a-set? '((2 red oval hatched)
             (2 red rectangle hatched)
             (2 red wave hatched)))
(is-a-set? '((2 red rectangle outline)
             (2 green rectangle outline)
             (2 green rectangle solid)))


;4.
;Karten ziehen
(define draw-from-deck (λ (n) (take (shuffle deck) n)))

(define temp (draw-from-deck 12))

(show-list-of-cards temp)

;Alle SETs suchen
(define all-3-n-sublists (λ (xs)
                           (filter (λ (x) (if (= (length x) 3) #t #f)) (subs xs))))

(map show-list-of-cards (filter is-a-set? (all-3-n-sublists temp)))
