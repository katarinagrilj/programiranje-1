(* ========== Vaja 2: Uvod v funkcijsko programiranje  ========== *)

(*----------------------------------------------------------------------------*]
Vektorje predstavimo kot seznam števil s plavajočo vejico.
[*----------------------------------------------------------------------------*)

type vector = float list

(*----------------------------------------------------------------------------*]
Definirajte enotske vektorje `i`, `j` in `k` v treh dimenzijah.
[*----------------------------------------------------------------------------*)

let i = [1.; 0.; 0.]
let j = [0.; 1.; 0.]
let k = [0.; 0.; 1.]

(*----------------------------------------------------------------------------*]
Napišite funkcijo `razteg : float -> vector -> vector`, ki vektor, 
predstavljen s seznamom števil s plavajočo vejico, pomnoži z danim skalarjem.
[*----------------------------------------------------------------------------*)

let razteg k = List.map (( *. ) k )

(*----------------------------------------------------------------------------*]
Napišite funkcijo `sestej : vector -> vector -> vector`, ki vrne vsoto dveh 
vektorjev.
[*----------------------------------------------------------------------------*)

let sestej = List.map2 ( +. ) 

(*----------------------------------------------------------------------------*]
Napišite funkcijo `skalarni_produkt : vector -> vector -> float`, ki izračuna 
skalarni produkt dveh vektorjev
[*----------------------------------------------------------------------------*)

let mnozenje = List.map2 ( *. ) 

let skalarni_produkt x y = 
    List.fold_left ( +. ) 0. @@ mnozenje x y


(*----------------------------------------------------------------------------*]
Napišite funkcijo `norma : vector -> float`, ki vrne evklidsko normo vektorja.
[*----------------------------------------------------------------------------*)

let norma x = sqrt @@ skalarni_produkt x x

(*----------------------------------------------------------------------------*]
Napišite funkcijo `projeciraj : vector -> vector -> vector`, ki izračuna 
projekcijo prvega vektorja na drugega.
[*----------------------------------------------------------------------------*)

let projeciraj x y =  skalarni_produkt x y  /.  norma y 

(*----------------------------------------------------------------------------*]
Napišite funkcijo `ovij : string -> string -> string`, ki sprejme ime HTML 
oznake in vsebino ter vrne niz, ki predstavlja ustrezno HTML oznako.

Primer:
`ovij "h1" "Hello, world!"`

[*----------------------------------------------------------------------------*)

let ovij ime niz = "<" ^ ime ^ ">" ^ niz ^ "</" ^ ime ^ ">"

(*----------------------------------------------------------------------------*]
Napišite funkcijo `zamakni : int -> string -> string`, ki sprejme število 
presledkov in niz ter vrne niz, v katerem je vsaka vrstica zamaknjena za ustrezno število presledkov.

Primer:
`zamakni 4 "Hello, world!"`

[*----------------------------------------------------------------------------*)

let zamakni stevilo niz = 
    let presledki = String.make stevilo ' ' in
    niz
    |> String.split_on_char '\n'
    |> List.map (String.cat presledki)
    |> String.concat "\n"
    

(*----------------------------------------------------------------------------*]
Napišite funkcijo `ul : string list -> string`, ki sprejme seznam nizov in vrne 
niz, ki predstavlja ustrezno zamaknjen neurejeni seznam v HTML-ju:

Primer:
`ul ["ananas"; "banana"; "čokolada"]`

[*----------------------------------------------------------------------------*)

let ul seznam = 
    seznam
    |> List.map ( ovij "li" ) 
    |> List.map ( zamakni 1 )
    |> String.concat "\n"
    |> ( fun (vzorec) -> "\n" ^ vzorec ^ "\n")
    |> ovij "ul"

(*----------------------------------------------------------------------------*]
Napišite funkcijo `razdeli_vrstico : string -> string * string`, ki sprejme niz, 
ki vsebuje vejico, loči na del pred in del za njo.

Primer:
`razdeli_vrstico "mleko, 2"`

[*----------------------------------------------------------------------------*)

let razdeli_vrstico vrstica = 
    let indeks = String.index vrstica ',' in
    let prvi = String.sub vrstica 0 indeks in
    let drugi = String.sub vrstica ( indeks + 1 ) ( String.length vrstica - ( indeks + 1 ) ) in
    ( String.trim prvi, String.trim drugi )
    

(*----------------------------------------------------------------------------*]
Napišite funkcijo `pretvori_v_seznam_parov : string -> (string * string) list`, 
ki sprejme večvrstični niz, kjer je vsaka vrstica niz oblike 
"izdelek, vrednost", in vrne seznam ustreznih parov.

Primer:
`pretvori_v_seznam_parov "mleko, 2\nkruh, 1\njabolko, 5"`

[*----------------------------------------------------------------------------*)

let pretvori_v_seznam_parov niz = 
    niz
    |> String.trim
    |> String.split_on_char '\n'
    |> List.map razdeli_vrstico 

(*----------------------------------------------------------------------------*]
Napišite funkcijo `pretvori_druge_komponente : ('a -> 'b) -> (string * 'a) list -> (string * 'b) list`,
ki dano funkcijo uporabi na vseh drugih komponentah elementov seznama.

Primer:
```ml
let seznam = [("ata", "mama"); ("teta", "stric")] in 
pretvori_druge_komponente String.length seznam
```

[*----------------------------------------------------------------------------*)

let pretvori_druge_komponente funkcija = 
    let sprememba tup = funkcija @@ snd tup in
    let novi tup = ( fst tup, sprememba tup ) in
    List.map novi 

(*----------------------------------------------------------------------------*]
Napišite funkcijo `izracunaj_skupni_znesek : string -> string -> float`, ki 
sprejme večvrstična niza nakupovalnega seznama in cenika in izračuna skupni 
znesek nakupa.

Primer:
```ml
let nakupovalni_seznam = "mleko, 2\njabolka, 5"
and cenik = "jabolka, 0.5\nkruh, 2\nmleko, 1.5" in
izracunaj_skupni_znesek cenik nakupovalni_seznam
```

[*----------------------------------------------------------------------------*)

let izracunaj_skupni_znesek cenik seznam = 
    let lepi nekaj = pretvori_druge_komponente ( float_of_string ) @@ pretvori_v_seznam_parov nekaj in
    lepi seznam
    |> List.map ( fun (x, y) -> (x, y *. snd (List.find ( fun s -> fst s = x ) ( lepi cenik) ) ) )
    |> List.fold_left ( fun z (x, y) -> y *. z) 1.  


    
