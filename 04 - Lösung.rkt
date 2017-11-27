#lang racket
;;;Aufgabe 1
;1.) 6 , da max und min jeweils Funktionen sind und die Argumente daher am Anfang ausgewertet werden
;2.)
;3.) 'King , da cadr die Hintereinanderausführung von cdr car ist.
;4.) '() , da die zweimalige Anwendung von cdr genau '((On the feast of Steven)) liefert. Dies ist eine Liste mit nur einem Element, daher liefert die dritte Anwendung von cdr genau '()
;5.) '(When the snow lay round about) , da cons eine Funktion ist und daher 'When evaluiert wird und somit When der Liste hinzugefügt wird
;6.) '((Deep and) . crisp) , da das zweite Argument keine Liste ist und cons daher ein Paar ausgibt, bestehend aus der Liste '(Deep and) und crisp.
;7.) #t , da die beiden Listen dieselben Elemente beinhalten und equal? deshalb true ausgibt.
;8.) #f , da eq? überprüft, ob die Objekte identisch sind. Da beide Objekte separat voneinander erstellt werden, sind sie nicht identisch.

;;;Aufgabe 2
;Zur Übersicht führen wir die Funktion aus Aufgabenblatt 3, die wir hier benutzen wollen, noch einmal auf
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

;;Aufgabe 2.1

;;Aufgabe 2.2

;Hilfsmethode zum Übersetzen des Formats aus Blatt 3
(define (stringlist->string list)
  (if (eq? list '()) ""
      (string-append (car list) " " (stringlist->string (cdr list)))))

(define (notmeldungHeader name rufzeichen)
  (string-append "MAYDAY MAYDAY MAYDAY \n"
                 (stringlist->string (string->phonetic "DE")) "\n"
                 name " " name " " name " " (stringlist->string (string->phonetic rufzeichen)) "\n"
                 "MAYDAY " name " " (stringlist->string (string->phonetic name)) "\n"
                 (stringlist->string (string->phonetic rufzeichen)) "\n"))

(define (notmeldung name rufzeichen position infos)
  (string-upcase (string-append (notmeldungHeader name rufzeichen)
                                position "\n"
                                infos "\n"
                                "- - \n"
                                name " " (stringlist->string (string->phonetic rufzeichen)) "\n"
                                "over")))

;;Aufgabe 2.3
(display (notmeldung "Unicorn" "UCRN" "NOTFALLPOSITION UNGEFÄHR 5 SM NORDWESTLICH LEUCHTTURM ROTER SAND" "NOTFALLZEIT 1000 UTC \nSCHWERE SCHLAGSEITE WIR SINKEN \nKEINE VERLETZTEN \nSECHS MANN GEHEN IN DIE RETTUNGSINSEL \nSCHNELLE HILFE ERFORDERLICH \nICH SENDE DEN TRÄGER"))
(display "\n\n")
(display (notmeldung "Nautilus" "DEYJ" "NOTFALLPOSITION UNGEFÄHR 10 SM östlich Point Nemo 48° 52’ 31,75” S, 123° 23’ 33,07“ W" "Eine Riesenkrake hat das Schiff umschlungen \nGroßes Leck im Rumpf \n20 Personen an Bord \nTreiben antriebslos an Wasseroberfläche \nSende Peilzeichen"))
(display "\n\n")
(display (notmeldung "Maltese Falcon" "HUQ9" "NOTFALLPOSITION  54° 34’ 5,87” N, 8° 27’ 33,41” E" "Sind auf eine Sandbank aufgelaufen \n10 Mann an Bord \nDas Schiff ist 88m lang, schwarzer Rumpf \nUnfallzeit 0730 UTC \nSende Peilzeichen"))
(display "\n\n")


;;;Aufgabe 3
;;Aufgabe 3.1

(define (hoch3 x) (* x x x))

;Innere Auswertung
;(hoch3 (+ 3 (hoch3 3)))
;(hoch3 (+ 3 (* 3 3 3)))
;(hoch3 (+ 3 27))
;(hoch3 30)
;27000

;Äußere Auswertung
;(hoch3 (+ 3 (hoch3 3)))
;(* (+ 3 (hoch3 3)) (+ 3 (hoch3 3)) (+ 3 (hoch3 3)))
;(* (+ 3 (* 3 3 3)) (+ 3 (* 3 3 3)) (+ 3 (* 3 3 3)))
;(* (+ 3 27) (+ 3 27) (+ 3 27))
;(* 30 30 30)
;27000

;;Aufgabe 3.2
;Für normale Ausdrücke verwendet Racket die innere Auswertung, für S-Expressions die äußere Auswertung.

;;Aufgabe 3.3

(define (new-if condition? then-clause else-clause)
  (cond (condition? then-clause)
        (else else-clause)))

(define (faculty product counter max-count)
  (new-if (> counter max-count)
          product
          (faculty (* counter product)
                   (+ counter 1)
                   max-count)))

;Beim Ausführen von
;(faculty 1 1 5)
;kommt es zu einer Endlosschleife und letztendlich zu einem volllaufen des Speichers, weil der zweite "Zweig" von 
;faculty immer, selbst bei der vermeintlichen Abbruchbedingung, durchlaufen wird.
;Mit einer S-Expression würde dies vermieden werden, da new-if schon bei der Auswertung von (> counter max-count)
;aufhören und den zweiten Zweig gar nicht erst auswerten würde.
