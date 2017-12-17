#lang racket
(require racket/trace)

;;;Aufgabe 1

(define (zaehlen1 x xs)
  (if (null? xs)
      0
      (if (eq?(car xs) x)
          (+ 1 (zaehlen1 x (cdr xs)))
          (zaehlen1  x (cdr xs)))))

(define (zaehlen2 x xs)
  (letrec ([zaehlenInner (λ (x xs acc)
                           (if (null? xs)
                               acc
                               (begin
                                 (if (eq? (car xs) x)
                                     (zaehlenInner x (cdr xs) (+ 1 acc))
                                     (zaehlenInner x (cdr xs) acc)))))])
    (zaehlenInner x xs 0)))


(define (zaehlen3 x xs)
  (letrec ([count (λ (y counter)
                    (if (eq? y x)
                        (+ counter 1)
                        counter))])
    (foldl count 0 xs)))


(zaehlen1 3 '(2 4 3 2 3 4 1))
(zaehlen2 3 '(2 4 3 2 3 4 1))
(zaehlen3 3 '(2 4 3 2 3 4 1))


;;;Aufgabe 2
(require 2htdp/image)
(require 2htdp/universe)

;Die Datenstruktur cell beschreibt eine Zelle durch ihren x-Wert und Y-Wert
(define-struct cell (x-value y-value))

;Die Datenstruktur gamestate ist durch livingCells und deadCells gegeben. Dabei sind livingCells und deadCells jeweils Listen von
;Elementen des Typs cell. Es wird im weiteren davon ausgegangen, dass jede der 900 Zellen des 30x30-Spielfeldes in genau einer
;dieser beiden Listen enthalten ist
(define-struct gamestate (livingCells deadCells))

;Zu einer gegebenen Zelle(Element vom Typ cell) wird ein Bild gezeichnet, welches die Position dieser Zelle anzeigt.
;Dabei wird die Zelle als lebendige Zelle gezeichnet(schwarz ausgefülltes Kästchen).
(define (showLivingCell zelle)
  (underlay/offset (rectangle 300 300 'outline 'black)
                   (+ -145 (* 10 (- (cell-x-value zelle) 1)))
                   (+ -145 (* 10 (- (cell-y-value zelle) 1)))
                   (rectangle 10 10 'solid 'black)))

;Zu einer gegebenen Zelle(Element vom Typ cell) wird ein Bild gezeichnet, welches die Position dieser Zelle anzeigt.
;Dabei wird die Zelle als tote Zelle gezeichnet(schwarz umrandetes Kästchen).
(define (showDeadCell zelle)
  (underlay/offset (rectangle 300 300 'outline 'black)
                   (+ -145 (* 10 (- (cell-x-value zelle) 1)))
                   (+ -145 (* 10 (- (cell-y-value zelle) 1)))
                   (rectangle 10 10 'outline 'black)))

;Zeichnet den eigegebenen Spielzustand. Die Eingabe soll ein Element vom Typ gamestate sein.
(define (zeigeSpielzustand spielzustand)
  (cond [(null? (gamestate-livingCells spielzustand)) (apply overlay (map showDeadCell (gamestate-deadCells spielzustand)))]
        [(null? (gamestate-deadCells spielzustand)) (apply overlay (map showLivingCell (gamestate-livingCells spielzustand)))]
        [else (overlay/offset (apply overlay (map showLivingCell (gamestate-livingCells spielzustand)))
                              0
                              0
                              (apply overlay (map showDeadCell (gamestate-deadCells spielzustand))))]))

;Hilfsfunktion, die überprüft, ob zwei Zellen dieselben Werte haben
(define (equalCells? cell1 cell2)
  (and (= (cell-x-value cell1) (cell-x-value cell2)) (= (cell-y-value cell1) (cell-y-value cell2))))

;Hilfsfunktion, die überprüft, ob eine Zelle in einer Liste enthalten ist
(define (member? zelle xs)
  (if (null? xs)
      #f
      (or (equalCells? zelle (car xs))
           (member? zelle (cdr xs)))))

;Hilfsfunktion, die zu einer gegebenen Zahl 1=<n=<900 die eindeutig zugeordnete Zelle ausgibt. Die 900 Zellen des
;30x30-Spielfeldes werden dazu zeilenweise durchnummeriert. Zum soll die Zelle (20 1) die Nummer 20 haben und die Zelle (20 2)
;soll die Nummer 50 haben
(define (number->cell n)
  (letrec ([Inner (lambda (k n)
                    (if (< 30 n)
                        (Inner (+ k 1) (- n 30))
                        k))])
    (make-cell (- n (* 30 (Inner 0 n ))) (+ 1 (Inner 0 n)))))

;Erstellt ein Element vom Typ gamestate. Die Eingabe ist eine Liste von Zellen(Keine Zelle soll doppelt auftreten). diese Zellen
;sollen im erstellten gamestate lebendig sein. Alle anderen Zellen des 30x30-Spielfeldes werden in dem erstellten gamestate
;tote Zellen sein
(define (erstelleSpielzustand xs)
  (letrec ([createDeadCells (lambda (ys n)
                              (if (= n 900)
                                  (if (member? (number->cell n) xs)
                                      ys
                                      (cons (number->cell n) ys))
                                  (if (member? (number->cell n) xs)
                                      (createDeadCells ys (+ n 1))
                                      (createDeadCells (cons (number->cell n) ys) (+ n 1)))))])
    (make-gamestate xs (createDeadCells '() 1))))

;Zu zwei gegebenen Zellen(Elemente vom Typ cell) wird ermittelt, ob beide Nachbarn von einander sind. Dabei wird 1 ausgegeben,
;wenn sie Nachbarn sind und 0 sonst. Eine Zelle soll nicht ihr eigener nachbar sein.
(define (nachbar? zelle1 zelle2)
  (if (equalCells? zelle1 zelle2)
      0
      (if (and (<= (abs (- (cell-x-value zelle1)
                           (cell-x-value zelle2)))
                   1)
               (<= (abs (- (cell-y-value zelle1)
                           (cell-y-value zelle2)))
                   1))
          1
          0)))

;Hier ist eine weitere Funktion, die zu zwei gegebenen Zellen ermittelt, ob beide Nachbarn voneinander sind. Diese Funktion hier
;bezieht sich jedoch auf Die Zusatzaufgabe mit dem Torus-förmigen Spielfeld. Damit die Simulation funktioniert, tragen beide
;nachbar?-Funktionen denselben namen und es wird einfach die Funktion auskommentiert, die gerade nicht gebraucht wird.
#|(define (nachbar? zelle1 zelle2)
  (if (equalCells? zelle1 zelle2)
      0
      (if (and (or (<= (abs (- (cell-x-value zelle1)
                               (cell-x-value zelle2)))
                       1)
                   (= (abs (- (cell-x-value zelle1)
                              (cell-x-value zelle2)))
                      29))
               (or (<= (abs (- (cell-y-value zelle1)
                               (cell-y-value zelle2)))
                       1)
                   (= (abs (- (cell-y-value zelle1)
                              (cell-y-value zelle2)))
                      29)))
          1
          0)))|#

;Zu einem gegebenen Spielzustand(Element vom Typ gamestate) und einer gegebenen Zelle(Element vom Typ cell) wird die Anzahl der
;lebenden Zellen in der Nachbarschaft ermittelt
(define (neighbourhood spielzustand zelle)
  (apply + (map (curry nachbar? zelle)
                (gamestate-livingCells spielzustand))))

;Zu einem gegebenen Spielzustand(Element vom Typ gamestate) und einer gegebenen Zelle(Element vom Typ cell) wird ermittelt, ob
;die Zelle lebendig oder tot ist. Ist sie lebendig, wird #t ausgegeben, ansonsten wird #f ausgegeben
(define (living? spielzustand zelle)
  (member? zelle (gamestate-livingCells spielzustand)))

;Zu einem gegebenen Spielzustand und einer gegebenen Zelle wird ermittelt, ob die Nachfolgezelle lebendig ist. Die Nachfolgezelle
;ist genau dann lebendig, wenn sie entweder drei lebendige Nachbarn hat oder wenn sie zwei lebendige Nachbarn hat und vorher
;schon lebendig war. Ansonsten ist die Nachfolgezelle tot.
(define (nachfolgeZelle spielzustand zelle)
  (or (= 3 (neighbourhood spielzustand zelle))
      (and (= 2 (neighbourhood spielzustand zelle))
           (living? spielzustand zelle))))

;Zu einem gegebenen Spielzustand wird der Nachfolgespielzustand ermittelt. Dabei wird für jede Zelle die Funktion nachfolgeZelle
;aufgerufen. Hier benutzen wir wieder die Nummern von 1 bis 900 für die Zellen(siehe Funktion number->cell).
;Die Funktion Inner liefert eine Liste, die alle lebendigen Nachfolgezellen enthält.
(define (nachfolgeSpielzustand spielzustand)
  (letrec ([Inner (lambda (n acc)
                    (if (= n 900)
                        (if (nachfolgeZelle spielzustand (number->cell n))
                            (cons (number->cell n) acc)
                            acc)
                        (if (nachfolgeZelle spielzustand (number->cell n))
                            (Inner (+ n 1) (cons (number->cell n) acc))
                            (Inner (+ n 1) acc))))])
    (erstelleSpielzustand (Inner 1 '()))))

;Animation
(define (animation startzustand)
  (big-bang startzustand
            (on-tick nachfolgeSpielzustand 0.25)
            (to-draw zeigeSpielzustand)))

;;Beispiele für Startzustände
;Statisches Objekt
(define statisch (erstelleSpielzustand (list (make-cell 17 14)
                                             (make-cell 16 13)
                                             (make-cell 15 14)
                                             (make-cell 16 15)
                                             (make-cell 15 16)
                                             (make-cell 14 15))))

;Zyklisches Objekt
(define zyklisch (erstelleSpielzustand (list (make-cell 16 13)
                                             (make-cell 17 12)
                                             (make-cell 18 13)
                                             (make-cell 17 14)
                                             (make-cell 17 15)
                                             (make-cell 18 16)
                                             (make-cell 17 17)
                                             (make-cell 16 16)
                                             (make-cell 15 16)
                                             (make-cell 14 17)
                                             (make-cell 13 16)
                                             (make-cell 14 15)
                                             (make-cell 14 14)
                                             (make-cell 13 13)
                                             (make-cell 14 12)
                                             (make-cell 15 13))))

;Gleiter
(define gleiter (erstelleSpielzustand (list (make-cell 2 1)
                                            (make-cell 3 2)
                                            (make-cell 3 3)
                                            (make-cell 2 3)
                                            (make-cell 1 3))))

;Innerhalb endlicher vieler Schritte ist das Spielfeld leer
(define leer (erstelleSpielzustand (list (make-cell 14 14)
                                         (make-cell 14 13)
                                         (make-cell 14 12)
                                         (make-cell 15 12)
                                         (make-cell 16 12)
                                         (make-cell 16 13)
                                         (make-cell 16 14)
                                         (make-cell 16 16)
                                         (make-cell 16 17)
                                         (make-cell 16 18)
                                         (make-cell 15 18)
                                         (make-cell 14 18)
                                         (make-cell 14 17)
                                         (make-cell 14 16))))
