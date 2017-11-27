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