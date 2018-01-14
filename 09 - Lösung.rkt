#lang swindle
(require swindle/setf
         swindle/misc )

;;;Aufgabe 1.1
;Definition der Klasse "Literaturbeitrag"
(defclass* Literaturbeitrag ()
  (Schlüssel :initarg :key
             :reader key)
  (Autor :initarg :autor
         :reader autor)
  (Erscheinungsjahr :initarg :year
                    :reader year)
  (Titel :initarg :titel
         :reader titel)
  :printer #t)
;Definition der Klasse "Buch"
(defclass* Buch (Literaturbeitrag)
  (Verlag :initarg :verlag
          :reader verlag)
  (Verlagsort :initarg :place
              :reader place)
  (Reihe :initarg :series
         :reader series)
  (Seriennummer :initarg :seriennummer
                :reader seriennummer)
  :printer #t)
;Definition der Klasse "Sammelband"
(defclass* Sammelband (Buch)
  (Herausgeber :initarg :publisher
               :reader publisher)
  (Seitenangaben :initarg :pages
                 :reader pages))
;Definition der Klasse "Zeitschriftenartikel
(defclass* Zeitschriftenartikel (Literaturbeitrag)
  (Zeitschrift :initarg :magazine
               :reader magazine)
  (Bandnummer :initarg :bandnummer
              :reader bandnummer)
  (Heftnummer :initarg :heftnummer
              :reader heftnummer)
  (Erscheinungsmonat :initarg :month
                     :reader month))
;Beispielobjekte
(define Beitrag1 (make Buch
                       :autor "Nessie"
                       :year 1790
                       :titel "Mein Leben im Loch Ness: Verfolgt als Ungeheuer"
                       :series "Die besondere Biographie"
                       :seriennummer 1
                       :verlag "Iverness"))
(define Beitrag2 (make Zeitschriftenartikel
                       :autor "H. G. Wells"
                       :year 3200
                       :titel "Zeitmaschinen leicht gemacht"
                       :magazine "Heimwerkerpraxis für Anfänger"
                       :bandnummer 500
                       :heftnummer 3))
;;;Aufgabe 1.2
(defgeneric* cite ((LB Literaturbeitrag)))
(defmethod cite ((LB Literaturbeitrag))
  (string-append (autor LB) " (" (number->string (year LB)) "), " (titel LB)))
(defmethod cite ((LB Buch))
  (string-append (autor LB) " (" (number->string (year LB)) "), " (titel LB) ", Band "
                 (number->string (seriennummer LB)) " der Reihe: " (series LB) ". " (verlag LB) "."))
(defmethod cite ((LB Zeitschriftenartikel))
  (string-append (autor LB) " (" (number->string (year LB)) "), "
                 (titel LB) ". " (magazine LB) ", Band " (number->string (bandnummer LB)) ", Heft "
                 (number->string (heftnummer LB)) "."))

(display (cite Beitrag1))
(display "\n\n")
(display (cite Beitrag2))
(display "\n\n")

#|
Aufgabe 1.3

- Ergänzungsmethoden werden an eine Methode vorn oder hinten "angehängt" und bei aufruf der ursprünglichen
  Methode mit ausgeführt.
- Vorteil: keine Überladung/Überdeckung notwendig und wir können uns darauf verlassen, dass alle Statements
  in der Ursprünglichen Methode garantiert immer mit ausgeführt werden.


|#

;;;Aufgabe 2


(defclass* Speichermedium ()
  (Mobilität :reader portability)
  (Max.Speed :reader speed)
  (Kapazität :reader capacity)
  (Lebensdauer :reader lifeexpect)
  :autoinitargs #t
  :printer #t
  :autopred #t
  :autoacceccors #t
  )

(defclass* magnetisch (Speichermedium))

(defclass* optisch (Speichermedium)
  (Mobilität :initvalue #t))

(defclass* halbleiter (Speichermedium))

(defclass* HDD (magnetisch)
  (Mobilität :initvalue #f))

(defclass* Diskette (magnetisch)
  (Mobilität :initvalue #t))

(defclass* SSD (halbleiter)
  (Mobilität :initvalue #f))

(defclass* USB-Stick (halbleiter)
  (Mobilität :initvalue #t))

(defclass* Magneto-Optical-Disk (magnetisch optisch)
  (Mobilität :initvalue #t))

(defclass* SSHD (magnetisch halbleiter)
  (Mobilität :initvalue #f))

(defclass* Bankkarte (magnetisch halbleiter)
  (Mobilität :initvalue #t))



(defgeneric* Speichertyp ((medium Speichermedium)))
(defgeneric* Max.Speed ((medium Speichermedium)))
(defgeneric* Kapazität ((medium Speichermedium)))
(defgeneric* Lebensdauer ((medium Speichermedium)))
(defgeneric* Mobilität ((medium Speichermedium)))


(defmethod Speichertyp ((medium Speichermedium))
  (class-name (class-of medium)))

(defmethod Speichertyp ((medium Magneto-Optical-Disk))
  "gemischt (optisch-magnetisch)")

(defmethod Speichertyp ((medium SSHD))
  "gemischt (halbleiter-magnetisch)")

(defmethod Speichertyp ((medium Bankkarte))
  "gemischt (halbleiter-magnetisch)")

(defmethod Max.Speed ((medium Speichermedium))
  (speed medium)
  )
(defmethod Kapazität ((medium Speichermedium))
  (capacity medium)
  )
(defmethod Lebensdauer ((medium Speichermedium))
  (lifeexpect medium)
  )
(defmethod Mobilität ((medium Speichermedium))
  (portability medium)
  )


(define print-medium
  (lambda (medium)
    (display (Speichertyp medium))
    (display "\n")
    (display (Max.Speed medium))
    (display "\n")
    (display (Kapazität medium))
    (display "\n")
    (display (Lebensdauer medium))
    (display "\n")
    (display (Mobilität medium))))


(define med1 (make Magneto-Optical-Disk
                   :Max.Speed 100
                   :Kapazität 100
                   :Lebensdauer 50))

(define med2 (make Bankkarte
                   :Max.Speed 10
                   :Kapazität 5
                   :Lebensdauer 5))

(define med3 (make SSHD
                   :Max.Speed 100000
                   :Kapazität 1000000000
                   :Lebensdauer 15))

(define med4 (make USB-Stick
                   :Max.Speed 100
                   :Kapazität 32000000
                   :Lebensdauer 10))

(print-medium med1)
(display "\n\n")
(print-medium med2)
(display "\n\n")
(print-medium med3)
(display "\n\n")
(print-medium med4)

#|
Anm:
Da wir auf keine Probleme mit der Klassenpräzedenz gestoßen sind, haben wir die Aufgabe vermutlich
anders gelöst als erwartet. Auch die generischen Methoden wären eigentlich nicht notwendig gewesen,
wurden der Vollständigkeit halber jedoch hinzugefügt.

|#



