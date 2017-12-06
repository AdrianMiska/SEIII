#lang racket
(require 2htdp/image)
;Ich hab mit dem Modul schonmal ein bisschen rumgespielt. Viel ist zur Lösung der Aufgabe noch nicht 
;rausgekommen, aber einen Tannenbaum habe ich schonmal
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
