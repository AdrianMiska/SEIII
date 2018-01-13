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
  (Seriennummer :Initarg :seriennummer
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
                       :autor 'Nessie
                       :year 1790
                       :titel "Mein Leben im Loch Ness: Verfolgt als Ungeheuer"
                       :series "Die besondere Biographie"
                       :seriennummer 1
                       :verlag 'Iverness))
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
