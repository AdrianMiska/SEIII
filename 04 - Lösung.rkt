#lang racket
;;;Aufgabe 1
;1.) 6 , da man und min jeweils Funktionen sind und die Argumente daher am Anfang ausgewertet werden
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

