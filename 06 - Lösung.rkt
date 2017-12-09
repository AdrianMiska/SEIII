#lang racket
(require 2htdp/image)
(require 2htdp/universe)
;Tannenbaum
(define tannenbaum
  (letrec ([Inner (lambda (L H W acc img)
                    (if (< L 30)
                        (overlay/offset
                         (isosceles-triangle L W 'solid 'green)
                         0
                         (+ 10 H)
                         img)
                        (Inner
                         (- L 5)
                         (+ H 10)
                         (- W 10)
                         (- acc 1)
                         (overlay/offset
                          (isosceles-triangle L W 'solid 'green)
                          0
                          (+ 10 H acc)
                          img))))])
    (above/align
     'center
     (Inner 70 0 110 10(isosceles-triangle 80 120 'solid 'green))
     (rectangle 40 25 'solid 'brown))))
;Stern
(define stern
  (letrec ([Inner (lambda (g)
                    (if (< g 2)
                        (star-polygon g 5 2 'solid (make-color 250 (+ 145 (* g 3)) 0))
                        (overlay/offset (Inner (- g 1))
                                        0
                                        0
                                        (star-polygon g 5 2 'solid (make-color 250 (+ 145 (* g 3)) 0)))))])
    (Inner 35)))
;Tannenbaum mit einem Stern auf der Spitze
(define weihnachtsbaum
  (overlay/offset stern
                  0
                  125
                  tannenbaum))
;Das Geweih eines Rentiers, linksOderRechts soll die Wert 'r oder 'l annehmen und festhalten, ob wir das linke oder rechte
;Geweihstück haben wollen. Die Variable L muss einen Wert der Form 2^n annehmen, wobei n eine natürliche Zahl
;ist(n=1 ist nicht erlaubt). n ist dann die Anzahl der Verzweigungen des Geweihs und L ist die Länge der untersten Geweihstücks.
;Die Funktion geweih wird bei der Funktion rentierKopf mit L=32 aufgerufen.
(define (geweih L linksOderRechts)
  (letrec ([Inner (lambda (L leftorright)
                    (if (eq? leftorright 'l)
                        (if (< L 4)
                            (line L L (make-pen 'brown (/ L 2) 'solid 'round 'round))
                            (overlay/offset (beside (Inner (/ L 2) 'l) (Inner (/ L 2) 'r))
                                            (/ L 2)
                                            L
                                            (line L L (make-pen 'brown (/ L 2) 'solid 'round 'round))))
                        (if (< L 4)
                            (line (* -1 L) L (make-pen 'brown (/ L 2) 'solid 'round 'round))
                            (overlay/offset (beside (Inner (/ L 2) 'l) (Inner (/ L 2) 'r))
                                            (* -1 (/ L 2))
                                            L
                                            (line (* -1 L) L (make-pen 'brown (/ L 2) 'solid 'round 'round))))))])
    (Inner L linksOderRechts)))
;Das Auge eines Rentiers
(define rentierAuge
  (overlay/offset (circle 8 'solid 'black)
                  0
                  0
                  (circle 16 'solid 'white)))
;Der Kopf eines Rentiers. Diese Funktion ruft die Funktionen rentierAuge und geweih auf.
(define rentierKopf
  (overlay/offset (above/align 'center
                               (overlay/offset rentierAuge
                                               48
                                               0
                                               rentierAuge)
                               (circle 12 'solid 'red))
                  0
                  -12
                  (overlay/offset (overlay/offset (geweih 32 'l)
                                                  160
                                                  0
                                                  (geweih 32 'r))
                                  0
                                  80
                                  (circle 64 'solid 'brown))))
;Das komplette Rentier
(define rentier
  (overlay/offset (overlay/offset rentierKopf
                                  150
                                  75
                                  (ellipse 250 100 'solid 'brown))
                  70
                  120
                  (overlay/offset (ellipse 35 110 'solid 'brown)
                                  200
                                  0
                                  (ellipse 35 110 'solid 'brown))))
;Der Schlitten des Weihnachtsmanns
(define schlitten
  (overlay/offset (overlay/offset (line 70 100 (make-pen 'gold 20 'solid 'round 'round))
                                  250
                                  0
                                  (overlay/offset (isosceles-triangle 115 60 'solid 'red)
                                                  75
                                                  7
                                                  (above/align 'right
                                                               (rectangle 150 100 'solid 'red)
                                                               (rectangle 150 14 'solid 'gold))))
                  24
                  61
                  (overlay/offset (overlay/offset (rectangle 260 15 'solid 'gold)
                                                  35
                                                  17
                                                  (overlay/offset (rectangle 10 20 'solid 'brown)
                                                               250
                                                               0
                                                               (rectangle 10 20 'solid 'brown)))
                                  13
                                  20
                                  (line 300 0 (make-pen 'brown 10 'solid 'round 'round)))))
;Ein, von n Elchen gezogener, Schlitten
(define (gezogenerSchlitten n)
  (letrec ([Inner (lambda (k)
                    (if (< k 2)
                        (overlay/offset (line 100 0 (make-pen 'black 5 'solid 'round 'round))
                                        -45
                                        0
                                        (scale (/ 1 3) rentier))
                        (overlay/align/offset 'right
                                              'center
                                              (Inner (- k 1))
                                              142
                                              0
                                              (overlay/offset (line 100 0 (make-pen 'black 5 'solid 'round 'round))
                                                        -45
                                                        0
                                                        (scale (/ 1 3) rentier)))))])
    (overlay/align/offset 'right
                          'center
                          (Inner n)
                          180
                          0
                          (scale (/ 1 2) schlitten))))
;Ein Wald, bestehend aus n Weihnachtsbäumen
(define (wald n)
  (letrec ([Inner (lambda (k)
                    (if (< k 2)
                        weihnachtsbaum
                        (overlay/align/offset 'right
                                              'center
                                              (Inner (- k 1))
                                              180
                                              0
                                              weihnachtsbaum)))])
    (Inner n)))
;Das komplette Weihnachtsbild
(define weihnachtsBild
  (overlay/offset (gezogenerSchlitten 3)
                  0
                  200
                  (wald 5)))
;Eine Scene für die Weihnachtsanimation
(define (createWeihnachtsScene s)
  (place-image/align (overlay/align/offset 'left
                                           'center
                                           (wald 5)
                                           (- 864 (* 3 s))
                                           -200
                                           (gezogenerSchlitten 3))
                     0
                     225
                     'left
                     'center
                     (empty-scene 864 450)))
;Starten der Weihnachtsanimation
(animate createWeihnachtsScene)
