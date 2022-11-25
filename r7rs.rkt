#lang r7rs

(import (scheme base)
        (scheme write))

; R7RS
; ----
;
; In deze cursus gebruiken we de R7RS-variant van Scheme, en de DrRacket
; ontwikkelomgeving. In deze file leggen we enkele praktische aspecten uit
; om R7RS te gebruiken.

; Je bent vrij om een andere code editor te gebruiken, maar daarvoor geven
; de assistenten geen officiële handleiding/instructies.



; A. R7RS in DrRacket
; ___________________
; 
; DrRacket is een editor waarin men verschillende programmeertalen kan gebruiken,
; bijvoorbeeld R5RS en 'Racket' (een andere variant van Scheme).
; DrRacket heeft geen ingebouwde ondersteuning voor R7RS. Om R7RS te gebruiken
; moet je eerst een "package" (een soort van plugin/library) installeren.
; Je kan deze package installeren via de menu's van DrRacket.
;
; 1. R7RS installeren in DrRacket
; 
; In de menubalk van DrRacket vind je de optie:
;   "File -> Package Manager..."
; Klik hierop, en er zal een nieuw scherm (een nieuw window) verschijnen.
; Bovenaan in het geopende scherm zie je een tekstveld
; "Package source: <tekstveld>".
; Type "r7rs" in het tekstveld, en druk op "enter" (op je toetsenbord) of druk op
; de "install" knop. De r7rs package zal nu installeren. De package is klaar met
; installeren wanneer je onderaan niet meer kan klikken op de "Abort install"
; knop, en wel op de "Close output" knop. Je mag nu het scherm van de package
; manager gewoon sluiten.
;
; 2. R7RS gebruiken in DrRacket
;
; Om R7RS code te kunnen schrijven moet je DrRacket "helpen" om te weten in welke
; programmeertaal je document is geschreven. Selecteer links onderaan in DrRacket
; "Choose Language...", en selecteer vervolgens "Determine language from source".
; Voortaan kan je dan, net als in dit bestand, bovenaan je code de lijn
; "#lang r7rs" plaatsen. Hierdoor weet DrRacket dat je code in R7RS
; geschreven is.
;
; B. Modules in R7RS
; __________________
;
; 1. Gebruik van modules
;
; R7RS laat, in tegenstelling tot andere Scheme-varianten, toe om code onder te
; verdelen in modules, die "libraries" heten. Libraries zijn een handige manier
; om functies te groeperen, en vormen op die manier een extra niveau van
; abstractie.
;
; Alle Scheme-functies, dus ook define, bevinden zich in R7RS in een library.
; Standaard zijn er nauwelijks functies beschikbaar, tenzij je deze dus expliciet
; importeert.  Daarom zal je bovenaan je eigen code, vlak onder de lijn die de
; taal specifieert, meestal de "import" functie oproepen met als parameters de
; libraries die je in je code wil gebruiken.  Een typische import staat bovenaan
; dit bestand:
;
;     (import (scheme base)
;             (scheme write))
;
; De eerste lijn geeft aan dat we de basisfuncties van Scheme willen importeren,
; de tweede lijn betreft een aantal eenvoudige I/O functies, zoals bv. display.
; Een volledig overzicht van de standaard R7RS libraries is te vinden in de
; documentatie: https://small.r7rs.org/attachment/r7rs.pdf
; Onder de import schrijf je vervolgens gewoon je eigen Scheme-code.
;
; 2. Definitie van modules
;
; Naast het importeren van bestaande libraries, kunnen we uiteraard ook zelf
; libraries gaan definiëren.  Dat kan door middel van de "define-library" functie, die
; opnieuw vlak onder de lijn die de taal specifieert moet komen.  Een minimale
; library, die slechts één functie "f" definieert, ziet er als volgt uit:
;
;      #lang r7rs
;      (define-library
;        (my-collection my-library)
;        (export f)
;        (import (scheme base)
;                (scheme write))
;        (begin
;          (define (f x) x)))
;
; De definitie van een library bestaat uit vier delen:
;
;   * De naam van de library met daarvoor de collection waarin de library is
;     ondergebracht.  In dit geval geven we dus aan dat de naam van onze library
;     "my-library" is, en dat we deze library in de collection "my-collection"
;     willen onderbrengen.
;   * Een export, die bepaalt welke functies door de library aan haar gebruikers
;     worden aangeboden.  In dit geval bieden we enkel de functie "f" aan.
;   * Een import, die bepaalt welke functies onze library zelf kan gebruiken.
;     In dit geval importeren we "scheme base" (m.a.w. de collection is "scheme",
;     en de library is "base") en "scheme write".
;   * Een body, die bestaat uit een "begin" met daarin een willekeurig aantal
;     definities.  Hier moeten we dus elke functie die we exporteren gaan
;     definiëren, door gebruik te maken van de functies die we importeren.
;     In dit geval definiëren we dus de geëxporteerde functie "f", door gebruik
;     te maken van de (uit base) geïmporteerde functie "define".  Je mag in de
;     body ook functies definiëren die niet geëxporteerd worden, bv. hulpfuncties.
;
; 3. Eigen libraries gebruiken in DrRacket
;
; Er zijn 2 manieren om zelfgemaakte libraries bruikbaar te maken in je code.
; Oftewel werk je via de Package Manager van DrRacket (dit is aanbevolen) of
; je kan de locatie van je library toevoegen aan de Collection Path(s) van DrRacket.
;
; 3.1. Installeren via de Package Manager (aanbevolen)
;
; Het installeren van zelfgemaakte libraries verloopt gelijkaardig aan het
; installeren van de R7RS package (zie A 1.).
;
; In de menubalk van DrRacket vind je de optie:
;   "File -> Package Manager..."
; Klik hierop, en er zal een nieuw scherm (een nieuw window) verschijnen.
;
; Bovenaan in het geopende scherm zie je een tekstveld
; "Package source: <tekstveld>".
;
; Naast het tekstveld zie je een knop "Browse".
; Klik hierop, en er zal een nieuw scherm op poppen die je vraagt of je file (bestand)
; of directory (map) wilt toevoegen. Indien de library uit meerdere bestanden bestaat,
; kies je best voor de optie "directory". (directory is bijvoorbeeld nodig voor de
; bibliotheek met de cursuscode, a-d, te installeren)
;
; Na de juiste optie te hebben gekozen, verandert het venster zodat je nu
; door bestanden kunt bladeren.
; Navigeer naar de locatie waar de directory (map) zich bevindt.
; Klik de map aan (1 klik), zodat deze geselecteerd staat (gearceerd).
; Klik vervolgens op de knop "Choose" onderaan het venster.
;
; Het venster verdwijnt nu en in het tekstveld van de package manager zou nu het pad
; naar je library moeten staan: <path/to/my-library>.
; Druk op "enter" (op je toetsenbord) of druk op de "install" knop.
; De library zal nu geïnstalleerd worden.
;
; De package manager is klaar met installeren wanneer je onderaan niet meer
; kan klikken op de "Abort install" knop, en wel op de "Close output" knop.
; Je mag nu het scherm van de package manager gewoon sluiten.
;
; Je kan altijd nagaan of de library geïnstalleerd is,
; in het tabblad "Currenty Installed" bovenaan in het venster van de package manager.
;
; 3.2. Collection Path(s)
;
; Libraries zijn bruikbaar in je code als deze opgeslaan worden in het
; "collection path" van DrRacket. Het standaard collection path verwijst
; enerzijds naar de installatie-directory van DrRacket voor libraries die door
; alle gebruikers gedeeld worden (bv. de standaard Scheme libraries), en
; anderzijds naar een directory die specifiek is voor de huidige gebruiker.
; Wanneer je een library importeert die niet gevonden wordt, zal DrRacket een
; foutmelding tonen die het huidige collections path bevat.
;
; Libraries komen overeen met Scheme-bestanden, collections komen overeen met
; directories (mappen) die Scheme-bestanden bevatten.  Als je my-library wil kunnen
; gebruiken, moet je dus aan één van de directories in je collections path
; de directory "my-collection" toevoegen, met daarin het bestand
; "my-library.rkt". Doe dit, bijvoorbeeld, in het standaard collections path
; van DrRacket. Dit is standaard als volgt: "<Racket installatiefolder>/collects"
;
; Merk op dat je ook zelf directories kan toevoegen aan het collections path, door
; links onderaan het scherm "Choose Language..." te selecteren, vervolgens op
; "Show Details" te klikken, en dan onder "Collection Path" op "Add" te klikken.
; Daar selecteer je dan de directory die je collection bevat (let op: dus niet
; de collection folder zelf, maar de folder waar je collections folder in zit!)
;
; 4. Name clashes
;
; Wanneer je code twee libraries importeert die allebei dezelfde functie
; exporteren, toont DrRacket een foutmelding: in dat geval kan immers niet
; bepaald worden welke implementatie er moet gebruikt worden wanneer je die
; functie oproept.  Dit probleem noemt men een "name clash", omdat de namen van
; de functies als het ware met mekaar botsen.  In dat geval kan je echter één
; van beide functies hernoemen, waardoor de name clash verdwijnt.
;
; Stel dat de functie "+" de oorzaak van de name clash is.  In dat geval kan je
; bijvoorbeeld het volgende doen:
;
;     (import (rename (scheme base) (+ base:+))
;             (my-collection my-library))
;
; De eerste lijn geeft aan dat we alle functies uit (scheme base) willen
; importeren, maar dat we (enkel) de functie "+" willen hernoemen in "base:+".
; Merk op dat we het voorvoegsel "base:" kiezen omdat een voorvoegsel met ":"
; makkelijk aangeeft waar een hernoemde functie vandaan komt. In principe
; kan je een functie hernoemen naar eendert wat, en dit hoeft niet te beginnen
; met "base:", en het hoeft ook niet de naam van de originele functie te bevatten.
;
; Om alle functies die door een library worden geëxporteerd in één keer te
; hernoemen, kunnen we het volgende doen:
;
;     (import (prefix (scheme base) base:)
;             (my-collection my-library))
;
; Nu geeft de eerste lijn aan dat we alle functies uit (scheme base) willen
; importeren, maar dat we elk van die functies willen hernoemen naar de
; oorspronkelijke naam, met vlak daarvoor de prefix "base:".  "define" wordt dus
; "base:define", "+" wordt "base:+", enzovoort, zonder dat we alle functies
; uit (scheme base) zelf moeten opsommen.
