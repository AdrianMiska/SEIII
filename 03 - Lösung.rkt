#lang racket


;;;Aufgabe 1
#|
Wir haben uns als Datenstruktur für die Hash-Tabelle entschieden,
da diese im Code gut lesbar ist und eine Abfrage in O(log N) ermöglicht.
|#

(define phoneticAlphabet (hash #\a "Alpha" #\b "Bravo"
                               #\c "Charlie" #\d "Delta"
                               #\e "Echo" #\f "Foxtrott"
                               #\g "Golf" #\h "Hotel"
                               #\i "India" #\j "Juliet"
                               #\k "Kilo" #\l "Lima"
                               #\m "Mike" #\n "November"
                               #\o "Oscar" #\p "Papa"
                               #\q "Quebec" #\r "Romeo"
                               #\s "Sierra" #\t "Tango"
                               #\u "Uniform" #\v "Viktor"
                               #\w "Whiskey" #\x "X-Ray"
                               #\y "Yankee" #\z "Zulu"
                               #\0 "Nadazero" #\1 "Unoone"
                               #\2 "Doutwo" #\3 "Terrathree"
                               #\4 "Carrefour" #\5 "Pentafive"
                               #\6 "Soxsix" #\7 "Setteseven"
                               #\8 "Oktoeight" #\9 "Novonine"
                               #\, "Decimal" #\. "Stop"
                               #\space " "))

(define (string->phonetic s)
  (letrec ([string->phoneticInner (λ (returnee charlist)
                                    (if (null? charlist) returnee
                                        (string->phoneticInner (cons (hash-ref phoneticAlphabet (char-downcase (car charlist)) void)
                                                                     returnee)
                                                               (cdr charlist))))])
    (string->phoneticInner '() (reverse (string->list s)))))


(string->phonetic "Hallo, dies ist ein Test.")
(list? (string->phonetic "Hallo, dies ist ein Test."))


;;;Aufgabe 2

(require se3-bib/flaggen-module)

(define flagAlphabet (hash #\a A #\b B
                           #\c C #\d D
                           #\e E #\f F
                           #\g G #\h H
                           #\i I #\j J
                           #\k K #\l L
                           #\m M #\n N
                           #\o O #\p P
                           #\q Q #\r R
                           #\s S #\t T
                           #\u U #\v V
                           #\w W #\x X
                           #\y Y #\z Z
                           #\0 Z0 #\1 Z1
                           #\2 Z2 #\3 Z3
                           #\4 Z4 #\5 Z5
                           #\6 Z6 #\7 Z7
                           #\8 Z8 #\9 Z9))

(define (string->flags s)
  (letrec ([string->flagsInner (λ (returnee charlist)
                                    (if (null? charlist) returnee
                                        (string->flagsInner (cons (hash-ref flagAlphabet (char-downcase (car charlist)) void)
                                                                     returnee)
                                                               (cdr charlist))))])
    (string->flagsInner '() (reverse (string->list s)))))

(string->flags "Hallo, dies ist ein Test.")
(list? (string->flags "Hallo, dies ist ein Test."))



