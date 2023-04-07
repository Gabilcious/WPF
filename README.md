## Arytmetyka przybliżonych wartości

Tam gdzie dokonujemy pomiarów wielkości fizycznych, wyniki są obarczone pewnym błędem, np. 5m ± 10%. Każdą taką przybliżoną wartość traktujemy jak zbiór możliwych wartości. Zaimplementuj pakiet operacji arytmetycznych na takich przybliżonych wartościach zawierający:

- konstruktory:
  - wartosc_dokladnosc x p = x ± p% (dla p > 0),
  - wartosc_od_do x y = (x+y)/2 ± (y-x)/2 (dla x < y),
  - wartosc_dokladna x = x ± 0
- selektory:
  - in_wartosc x y ⇔ wartość x może być równa y,
  - min_wartosc x = kres dolny możliwych wartości x (lub -∞ jeśli możliwe wartości x nie są ograniczone od dołu),
  - max_wartosc x = kres górny możliwych wartości x (lub ∞ jeśli możliwe wartości x nie są ograniczone od góry),
  - sr_wartosc x = średnia (arytmetyczna) wartości min_wartosc x i max_wartosc x (lub nan jeśli min_wartosc x i max_wartosc x nie są skończone),
- modyfikatory:
  - plus a b = { x + y : in_wartosc a x ∧ in_wartosc b y },
  - minus a b = { x - y : in_wartosc a x ∧ in_wartosc b y },
  - razy a b = { x · y : in_wartosc a x ∧ in_wartosc b y },
  - podzielic a b = {x/y:  in_wartosc a x ∧ in_wartosc b y }.

Zakładamy przy tym implicite, że wszystkie argumenty typu float są liczbami rzeczywistymi (tzn. są różne od infinity, neg_infinity i nan.
Natomiast w przypadku, gdy wynik nie jest liczbą rzeczywistą, powinien być odpowiednią z wartości: infinity, neg_infinity lub nan.

Przyjmij, że modyfikatory domykają wynikowe zbiory wartości, tzn. jeżeli, na przykład, wynikiem jest przedział otwarty, to przyjmij, że zostaje on zamieniony na przedział domknięty.

## Drzewa lewicowe

Drzewa lewicowe to ciekawa implementacja złączalnych kolejek priorytetowych.

Kolejki priorytetowe to struktury przechowujące dane obdarzone priorytetami, umożliwiające łatwy dostęp do elementu o najwyższym priorytecie. (Tradycyjnie, im mniejsza liczba reprezentująca priorytet, tym wyższy priorytet. ;-) Struktury te dostarczają następujących operacji:

- utwórz pustą strukturę,
- wstaw nowy element,
- usuń element o najwyższym priorytecie.

Oczywiście po usunięciu elementu o najwyższym priorytecie, drugi w kolejności staje się tym najwyższym itd. Kolejki złączalne umożliwiają dodatkowo łączenie dwóch kolejek w jedną.

Kolejki priorytetowe implementuje się zwykle za pomocą tzw. kopców, czyli struktur drzewiastych, które spełniają tzw. warunek kopca, mówiący, że priorytet elementu zawartego w korzeniu każdego poddrzewa jest mniejszy lub równy niż każdego innego elementu w tym poddrzewie.

Drzewa lewicowe to kopce binarne (czyli każdy węzeł może mieć 0, 1 lub dwóch potomków) spełniające, oprócz warunku kopca, tzw. warunek lewicowości. Warunek lewicowości mówi, że dla każdego węzła skrajnie prawa ścieżka zaczynająca się w danym węźle jest najkrótszą ścieżką od tego węzła do liścia.

Dzięki temu w każdym drzewie lewicowym, tzw. prawa wysokość, czyli długość skrajnej prawej ścieżki od korzenia do liścia, jest co najwyżej logarytmicznej wielkości, w porównaniu z liczbą elementów drzewa. Dodatkowo, aby umożliwić efektywne wykonywanie operacji na drzewie, w każdym węźle przechowywana jest prawa wysokość poddrzewa zaczepionego w tym węźle.

Najważniejszą operacją na drzewach lewicowych jest ich łączenie. Pozostałe operacje wykonuje się bardzo prosto:

- wstawianie elementu do istniejącego drzewa polega na utworzeniu jednoelementowego drzewa i połączeniu go z danym drzewem,
- usuwanie najmniejszego to usunięcie korzenia drzewa i połączenie poddrzew.

Łączenie drzew lewicowych też nie jest trudne. Aby połączyć dwa niepuste drzewa lewicowe, ustawiamy jako pierwsze (d1) to, które ma mniejszy element w korzeniu, a jako drugie (d2) to, które ma większy. W korzeniu wynikowego drzewa na pewno będzie więc korzeń d1. Teraz rekurencyjnie łączymy prawe poddrzewo d1 oraz całe drzewo d2, w wyniku dostając drzewo d3. Jako wynik całej operacji łączenia d1 i d2 zwracamy drzewo d4, w którego korzeniu jest korzeń d1, natomiast poddrzewami są lewe poddrzewo d1 oraz drzewo d3, przy czym prawym poddrzewem d4 zostaje to z nich, które ma mniejszą prawą wysokość. Dzięki temu d4 pozostaje drzewem lewicowym. Oczywiście przy konstrukcji drzewa d4 należy pamiętać o odpowiednim ustawieniu prawej wysokości.

Rysunkową wersję łączenia drzew lewicowych można zobaczyć np. tu:
https://courses.cs.washington.edu/courses/cse326/00wi/handouts/lecture7/sld001.htm

W naszym zadaniu dla uproszczenia zakładamy, że dane składają się z samych priorytetów.

### Zadanie

Używając drzew lewicowych zaimplementuj złączalną kolejkę priorytetową z następującymi operacjami:

```
type 'a queue (** Typ złączalnej kolejki priorytetowej *) 
val empty : 'a queue (** Pusta kolejka priorytetowa *) 
val add : 'a -> 'a queue -> 'a queue (** [add e q] zwraca kolejkę powstałą z dołączenia elementu [e] do kolejki [q] *) 
exception Empty (** Wyjątek podnoszony przez [delete_min] gdy kolejka jest pusta *) 
val delete_min : 'a queue -> 'a * 'a queue (** Dla niepustej kolejki [q], [delete_min q] zwraca parę [(e,q')] 
gdzie [e] jest elementem minimalnym kolejki [q] a [q'] to [q] bez elementu [e]. 
Jeśli [q] jest puste podnosi wyjątek [Empty]. *) 
val join : 'a queue -> 'a queue -> 'a queue (** [join q1 q2] zwraca złączenie kolejek [q1] i [q2] *) 
val is_empty : 'a queue -> bool (** Zwraca [true] jeśli dana kolejka jest pusta. W przeciwnym razie [false] *) 
```

## Modyfikacja drzew

Zadanie polega na zmodyfikowaniu biblioteki zbiorów pojedynczych elementów zaimplementowanych jako pewien wariant drzew AVL (drzewa BST z wyważaniem). Dzięki wyważaniu wysokość drzewa jest zawsze rzędu logarytmu z liczby wierzchołków i dlatego wszystkie operacje wykonywane są w czasie logarytmicznym (nawet operacja split, ale to jest trochę mniej oczywiste: wynika z tego, że koszt join jest w istocie proporcjonalny do różnicy wysokości drzew, które łączymy. A ponieważ na split składa się ciąg operacji join na coraz wyższych drzewach, ich koszty sumują się do wysokości drzewa razy pewna stała).

Wynikiem modyfikacji ma być biblioteka zbiorów liczb całkowitych oparta o przedziały. Czyli elementami występującymi w drzewie muszą być przedziały, a nie pojedyncze liczby. Przedziały muszą być rozłączne i w dodatku, aby uniknąć niejednoznaczności reprezentacji, przedziały w drzewie nie mogą być "sąsiednie", czyli np. dwa przedziały [1..3] i [4..6] powinny być zastąpione przez jeden przedział [1..6]. W naszej bibliotece dopuszczamy przedziały jednoelementowe, np. [3..3].

Wszystkie operacje (poza fold, iter, elements oraz is_empty) mają wykonywać się w czasie O(log n), gdzie n jest liczbą wierzchołków w drzewie.

Do zadania dołączona jest oryginalna specyfikacja i implementacja zbiorów (obie dostępne na licencji GNU Lesser General Public License) oraz specyfikacja zbiorów przedziałów, której implementację należy przesłać poprzez system moodle jako plik o nazwie iSet.ml. 

Przy implementacji zwróć uwagę, jak zachowuje się Twój kod dla liczb równych bądź bliskich max_int (albo min_int). W szczególności konkretne wymaganie dotyczące tego aspektu dla funkcji below podane jest w specyfikacji (iSet.mli).

Jak zwykle implementacja powinna być udokumentowana; w szczególności należy wpisać w komentarzu niezmienniki dla używanych struktur danych oraz pre- i post-warunki wszystkich metod występujących w implementacji (zwłaszcza tych, których nazwy nie występują w specyfikacji). Warunki te mogą dotyczyć np. poprawności drzew, zakładanej różnicy wysokości drzew, itp.

## Origami

Zaimplementuj bibliotekę dla fanów origami do badania ile warstw ma w danym punkcie sprytnie poskładana kartka papieru. Biblioteka powinna implementować interfejs origami.mli, podany poniżej.

```
type point = float * float
(** Punkt na płaszczyźnie *)

type kartka = point -> int
(** Poskładana kartka: ile razy kartkę przebije szpilka wbita w danym punkcie *)

val prostokat : point -> point -> kartka
(** [prostokat p1 p2] zwraca kartkę, reprezentującą domknięty prostokąt
o bokach równoległych do osi układu współrzędnych i lewym dolnym rogu [p1]
a prawym górnym [p2]. Punkt [p1] musi więc być nieostro na lewo i w dół
od punktu [p2]. Gdy w kartkę tę wbije się szpilkę wewnątrz
(lub na krawędziach) prostokąta, kartka zostanie przebita 1 raz,
w pozostałych przypadkach 0 razy *)

val kolko : point -> float -> kartka
(** [kolko p r] zwraca kartkę, reprezentującą kółko domknięte o środku w punkcie [p] i promieniu [r] *)

val zloz : point -> point -> kartka -> kartka
(** [zloz p1 p2 k] składa kartkę [k] wzdłuż prostej przechodzącej przez
punkty [p1] i [p2] (muszą to być różne punkty). Papier jest składany
w ten sposób, że z prawej strony prostej (patrząc w kierunku od [p1] do [p2])
jest przekładany na lewą. Wynikiem funkcji jest złożona kartka. Jej
przebicie po prawej stronie prostej powinno więc zwrócić 0.
Przebicie dokładnie na prostej powinno zwrócić tyle samo,
co przebicie kartki przed złożeniem. Po stronie lewej -
tyle co przed złożeniem plus przebicie rozłożonej kartki w punkcie,
który nałożył się na punkt przebicia. *)

val skladaj : (point * point) list -> kartka -> kartka
(** [skladaj [(p1_1,p2_1);...;(p1_n,p2_n)] k = zloz p1_n p2_n (zloz ... (zloz p1_1 p2_1 k)...)]
czyli wynikiem jest złożenie kartki [k] kolejno wzdłuż wszystkich prostych
z listy *) 
```

## Sortowanie topologiczne

Sortowanie topologiczne polega na rozszerzeniu grafu skierowanego bez cykli (DAG-u) do porządku liniowego.

Mówiąc prościej, mając dany DAG należy przypisać wierzchołkom takie różne liczby naturalne (nadające kolejność tym wierzchołkom), żeby dla każdej krawędzi grafu jej źródło miało niższy numer niż jej cel.

Mówiąc jeszcze prościej, mając daną częściową informację o zależności np. czynności od siebie (np. buty wkładamy po skarpetkach, krawat po koszuli itp. ale kolejność wkładania skarpetek i koszuli może być dowolna) mamy wygenerować ścisłą kolejność wykonywania czynności (np. koszula, skarpetki, buty, krawat).

Konkretnie należy zaprogramować implementację topol.ml załączonej specyfikacji topol.mli.

## Przelewanka

Masz dane n szklanek, ponumerowanych od 1 do n, o pojemnościach odpowiednio x1, x2, ..., xn.  Początkowo wszystkie szklanki są puste. Możesz wykonywać następujące czynności:

nalać do wybranej szklanki do pełna wody z kranu, 
wylać całą wodę z wybranej szklanki do zlewu, 
przelać wodę z jednej szklanki do drugiej — jeżeli się zmieści, to przelewasz całą wodę, a jeżeli nie, to tyle żeby druga szklanka była pełna.
Twoim celem jest uzyskanie takiej sytuacji, że w każdej szklance jest określona ilość wody, odpowiednio y1, y2, ..., yn. 

Napisz procedurę przelewanka : (int * int) array → int, która mając daną tablicę par liczb [|(x1, y1); (x2, y2); ...; (xn, yn)|] wyznaczy minimalną liczbę czynności potrzebnych do uzyskania opisanej przez nie sytuacji. Jeżeli jej uzyskanie nie jest możliwe, to poprawnym wynikiem jest -1. 

Możesz założyć, że 0 ≤ n, oraz 0 ≤ yi ≤ xi dla i = 1, 2, ..., n.
