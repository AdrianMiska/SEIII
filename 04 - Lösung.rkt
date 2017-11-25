#lang racket
;;;Aufgabe 1
;1.) 6 , da max und min jeweils Funktionen sind und die Argumente daher am Anfang ausgewertet werden
;2.) '(+ ,(- 13 11) 17) , ist das richtig so?
;3.) 'King , da cadr die Hintereinanderausführung von cdr car ist.
;4.) '() , da die zweimalige Anwendung von cdr genau '((On the feast of Steven)) liefert. Dies ist eine Liste mit nur einem Element, daher liefert die dritte Anwendung von cdr genau '()
;5.) '(When the snow lay round about) , da cons eine Funktion ist und daher 'When evaluiert wird und somit When der Liste hinzugefügt wird
;6.) '((Deep and) . crisp) , da das zweite Argument keine Liste ist und cons daher ein Paar ausgibt, bestehend aus der Liste '(Deep and) und crisp.
;7.) #t , da die beiden Listen dieselben Elemente beinhalten und equal? deshalb true ausgibt.
;8.) #f , da eq? überprüft, ob die Objekte identisch sind. Da beide Objekte separat voneinander erstellt werden, sind sie nicht identisch.
(max (min 5 (- 8 7)) 6)
'(+ ,(- 13 11) 17)
(cadr '(Good King Wenceslas))
(cdddr '(looked out (On the feast of Steven)))
(cons 'When '(the snow lay round about))
(cons '(Deep and) 'crisp)
(equal? (list 'and 'even) '(and even))
(eq? (list 'Rudolph 'the 'red-nosed 'reindeer)
     (cons 'Rudolph '(the 'red-nosed 'reindeer)))



;;;Aufgabe 2
(require racket/include)
(include "phonetic.rkt")

;;1

;;2

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

;;3
(display (notmeldung "Unicorn" "UCRN" "NOTFALLPOSITION UNGEFÄHR 5 SM NORDWESTLICH LEUCHTTURM ROTER SAND" "NOTFALLZEIT 1000 UTC \nSCHWERE SCHLAGSEITE WIR SINKEN \nKEINE VERLETZTEN \nSECHS MANN GEHEN IN DIE RETTUNGSINSEL \nSCHNELLE HILFE ERFORDERLICH \nICH SENDE DEN TRÄGER"))
(display "\n\n")
(display (notmeldung "Nautilus" "DEYJ" "NOTFALLPOSITION UNGEFÄHR 10 SM östlich Point Nemo 48° 52’ 31,75” S, 123° 23’ 33,07“ W" "Eine Riesenkrake hat das Schiff umschlungen \nGroßes Leck im Rumpf \n20 Personen an Bord \nTreiben antriebslos an Wasseroberfläche \nSende Peilzeichen"))
(display "\n\n")
(display (notmeldung "Maltese Falcon" "HUQ9" "NOTFALLPOSITION  54° 34’ 5,87” N, 8° 27’ 33,41” E" "SInd auf eine Sandbank aufgelaufen \n10 Mann an Bord \nDas Schiff ist 88m lang, schwarzer Rumpf \nUnfallzeit 0730 UTC \n Sende Peilzeichen"))