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


TODO

|#

;;;Aufgabe 2

