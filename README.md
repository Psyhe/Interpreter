Mój język można określić jako Latte+-. 
Składnia języka podobna do C, z kilkoma zapożyczeniami składniowymi z innych popularnych języków.
Znajdują się w nim elementy takie jak w standardowym Latte z kilkoma zmianami:

Zmiany względem tabelki:
1. Kosmetyczna zmiana - w Latte są już typy boolean, string, int i void, nazwa typu "boolean" została zmieniona na "bool".
2. Arytmetyka - operacje na dodawania, odejmowania itp. na intach; arytmetyka porównawcza dla wszystkich typów jak w Haskellu; arytmetyka or i and boolowy zaimplementowany dla booli; konkatenacja stringów.
3. Przypisanie wartości do zmiennej ma składnię var <typ> <nazwa> = <wartość>
4. Korzystamy z dostępnych już w Latte printów dla danych typów. (Mamy funkcje wbudowane printInt, printString, printBool).
5. While, if i else jak w oryginalnym Latte, z tym że wszystkie bloki w otoczeniu muszą być w nawiasach klamrowych dla większej przejrzystości.
6. Język pozwala na implementację funkcji i stosowanie rekurencji.
7. Przekazywanie argumentów przez zmienną (referencję) o składni: var <typ> <nazwa>, przez wartość o składni <typ> <nazwa>.
9. Mój interpreter zapewnia statyczne wiązanie, globalne zmienne (por. przykłady).
10. Obsługa błędów:
Błędy składniowe

Sprawdzanie typów:
Niezdefiniowany identyfikator
Niepoprawny typ
Niepoprawne argumenty funkcji
Błąd returna (out of scope)
Brak returna 
Duplikacja identyfikatorów
Pozostałe Błędy

Błędy wykonania:
Dzielenie przez 0.
Pozostałe Błędy

11. Przy definicji funkcji jest podawany jej typ, który definiuje typ wartości zwracanej.
12. Język jest typowany statycznie - typy są sprawdzane przez Typechecker przed fazą ewaluacji.
13. Jest możliwość tworzenia zagnieżdżonych funkcji.

Dodatkowe informacje:
1. return musi zawsze znajdować się wewnątrz funkcji (nawet jeśli funkcja to void)
2. return musi zawierać się w ciele funkcji (nie np. wewnątrz ifa)

Tabelka zawiera informacje o funkcjonalnościach zrealizowanych.
Wspomagałam się w trakcie pisania kodu githubCopilotem oraz chatem GPT.

Uruchomienie programu:
Należy wykonać polecenie make w głównym folderze.
Polecenie to stworzy pliki pomocnicze wykonywane na podstawie gramatyki (korzystając z bnfc na students) oraz wygeneruje plik wykonywany interpretera.

W ten sposób utworzą się pliki pomocnicze oraz plik wykonywany interpreter w folderze examples (Tam jest od razu sprawdzaka dla prawidłowo działających przykładów. Sprawdzarkę można uruchomić poleceniem 'python3 good_script.py'.
Ponadto można zobaczyć błędy wyświetlane przez niepoprawne przykłady używając sprawdzarki bad_script.py
uruchamianej poleceniem 'python3 bad_script.py').

Interpreter uruchamiamy (w folderze examples) poleceniami:
./interpreter <ścieżka_do_pliku>
./interpreter <wejście wczytywanie na stdin>

Na koniec pliki wykonywalne można usunąć poleceniem make clean lub distclean (jeśli chcemy też usunąć pliki wygenerowane z gramatyki).

W folderze examples znajduje się również opis języka, tabelka cech oraz plik z gramatykę (wyso.cf).

  Na 15 punktów
  01 (trzy typy) + +
  02 (literały, arytmetyka, porównania) + +
  03 (zmienne, przypisanie) + +
  04 (print) + +
  05 (while, if) + +
  06 (funkcje lub procedury, rekurencja) ++
  07 (przez zmienną / przez wartość / in/out) ++
  08 (zmienne read-only i pętla for) 
  Na 20 punktów
  09 (przesłanianie i statyczne wiązanie) + +
  10 (obsługa błędów wykonania) + +
  11 (funkcje zwracające wartość) + +
  Na 30 punktów
  12 (4) (statyczne typowanie) + +
  13 (2) (funkcje zagnieżdżone ze statycznym wiązaniem) + +
  14 (1/2) (rekordy/listy/tablice/tablice wielowymiarowe)
  15 (2) (krotki z przypisaniem)
  16 (1) (break, continue) 
  17 (4) (funkcje wyższego rzędu, anonimowe, domknięcia) 
  18 (3) (generatory)

  Razem: 30
  Suma: 26 (15 + 5 + 6)

Co się udało względem uwag:
1. Poprawiono Makefile
2. Dodano konkatenację na stringu (operator ++)
3. Uzupełniono przesłanianie - por przykłady przeslanianie.txt, other.txt, przeslanianieFun.txt
4. Usunięto możliwość parametrów i zmiennych typu void oraz porównaniach na voidach.
5. Więcej przykładów, w tym przykład dla każdego exception poza OtherException (który jest zaślepką pod ewentualny
przyszły rozwój).
6. Poprawnie obsługiwane błędy returna, przy czym return MUSI być konkretnie w ciele funkcji (a nie np. wewnątrz bloku czy ifa).
7. Zmiana dwumonadycznego typecheckera w jednomonatyczny z wykorzystaniem SE