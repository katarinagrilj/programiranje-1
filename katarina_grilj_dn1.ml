(*----------------------------------------------------------------------------*
 # 1. domača naloga
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ## Ogrevanje
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ### Števke
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `stevke : int -> int -> int list`, ki sprejme pozitivni celi
 števili $b$ in $n$ ter vrne seznam števk števila $n$ v bazi $b$. Pri tem tudi
 za baze, ki so večje od $10$, uporabimo števke od $0$ do $b - 1$.
[*----------------------------------------------------------------------------*)

let stevke b n = 
  if n <= 0 then failwith "Število mora biti pozitivno" else match b with
  | baza when baza <= 0 -> failwith "Baza mora biti pozitivno celo število"
  | 1 -> List.init n ( fun x -> 0 ) 
  | _ ->
    let rec stevke_tlrec b n acc1 = 
      match n / b with
      |0 ->  n :: acc1 
      |kolicnik -> stevke_tlrec b kolicnik (( n mod b ) :: acc1) in
    stevke_tlrec  b n []  





let primer_1_1 = stevke 10 12345
(* val primer_1_1 : int list = [1; 2; 3; 4; 5] *)

let primer_1_2 = stevke 2 42
(* val primer_1_2 : int list = [1; 0; 1; 0; 1; 0] *)

let primer_1_3 = stevke 16 (3 * 16 * 16 * 16 + 14 * 16 * 16 + 15 * 16 + 9)
(* val primer_1_3 : int list = [3; 14; 15; 9] *)

(*----------------------------------------------------------------------------*
 ### Začetek seznama
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `take : int -> 'a list -> 'a list`, ki sprejme naravno
 število in vrne ustrezno število elementov z začetka danega seznama. Če je
 podani seznam krajši od zahtevane dolžine, naj funkcija vrne kar celoten
 seznam.
[*----------------------------------------------------------------------------*)

let take n =
  if n <= 0 then failwith "Število mora biti večje od 0"
  else
  let rec take_tlrec dolzina acc =
    function
    | [] -> List.rev acc
    | seznam when dolzina = 0 -> List.rev acc
    | prvi :: ostalo -> take_tlrec ( dolzina - 1 ) ( prvi :: acc ) ostalo in
take_tlrec n []



let primer_1_4 = take 3 [1; 2; 3; 4; 5]
(* val primer_1_4 : int list = [1; 2; 3] *)

let primer_1_5 = take 10 [1; 2; 3; 4; 5]
(* val primer_1_5 : int list = [1; 2; 3; 4; 5] *)

(*----------------------------------------------------------------------------*
 ### Odstranjevanje ujemajočih
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `drop_while : ('a -> bool) -> 'a list -> 'a list`, ki z
 začetka seznama odstrani vse elemente, ki zadoščajo danemu predikatu. Ko najde
 element, ki predikatu ne zadošča, vrne preostanek seznama.
[*----------------------------------------------------------------------------*)

let rec drop_while f seznam  = 
match seznam with
|[] -> []
| prvi :: rep -> if f prvi = true then drop_while f rep else seznam
  

let primer_1_6 = drop_while (fun x -> x < 5) [3; 1; 4; 1; 5; 9; 2; 6; 5; 3; 5]
(* val primer_1_6 : int list = [5; 9; 2; 6; 5; 3; 5] *)

let primer_1_7 = drop_while (fun x -> x < 5) [9; 8; 7; 6; 5; 4; 3; 2; 1; 0]
(* val primer_1_7 : int list = [9; 8; 7; 6; 5; 4; 3; 2; 1; 0] *)

(*----------------------------------------------------------------------------*
 ### Funkcija `filter_mapi`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `filter_mapi : (int -> 'a -> 'b option) -> 'a list -> 'b
 list`, ki deluje tako kot `List.filter_map`, le da funkcija poleg elemenov dobi
 še njihove indekse.
[*----------------------------------------------------------------------------*)

let filter_mapi f zacetni_seznam  =
let rec naredi_seznam acc1 acc2 = 
  function
  |[] -> List.rev acc1
  |prvi :: rep -> naredi_seznam ( ( f acc2 prvi ) :: acc1 ) ( succ acc2 ) rep in
let rec filtriraj_seznam acc3 =
  function
  |[] -> List.rev acc3
  |None :: rep -> filtriraj_seznam acc3 rep
  |Some x :: rep -> filtriraj_seznam ( x :: acc3 ) rep in
filtriraj_seznam [] ( naredi_seznam [] 0 zacetni_seznam )





let primer_1_8 =
  filter_mapi
    (fun i x -> if i mod 2 = 0 then Some (x * x) else None)
    [1; 2; 3; 4; 5; 6; 7; 8; 9]
(* val primer_1_8 : int list = [1; 9; 25; 49; 81] *)

(*----------------------------------------------------------------------------*
 ## Izomorfizmi množic
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Na predavanjih smo videli, da funkciji `curry : ('a * 'b -> 'c) -> ('a -> ('b
 -> 'c))` in `uncurry : ('a -> ('b -> 'c)) -> ('a * 'b -> 'c)` predstavljata
 izomorfizem množic $C^{A \times B} \cong (C^B)^A$, če kartezični produkt
 predstavimo s produktnim, eksponent pa s funkcijskim tipom.

 Podobno velja tudi za ostale znane izomorfizme, če disjunktno unijo
   $$A + B = \{ \mathrm{in}_1(a) \mid a \in A \} \cup \{ \mathrm{in}_2(b) \mid b
 \in B \}$$
 predstavimo s tipom `('a, 'b) sum`, definiranim z:
[*----------------------------------------------------------------------------*)

type ('a, 'b) sum = In1 of 'a | In2 of 'b

(*----------------------------------------------------------------------------*
 Napišite pare funkcij `phi1` & `psi1`, …, `phi7` & `psi7`, ki predstavljajo
 spodnje izomorfizme množic. Tega, da so si funkcije inverzne, ni treba
 dokazovati.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ### $A \times B \cong B \times A$
[*----------------------------------------------------------------------------*)


let phi1 : 'a * 'b  -> 'b * 'a = fun (a, b) -> (b, a)

let psi1: 'b * 'a -> 'a * 'b = fun (b, a) -> (a , b)

(*----------------------------------------------------------------------------*
 ### $A + B \cong B + A$
[*----------------------------------------------------------------------------*)

let phi2 : ('a, 'b) sum -> ('b, 'a) sum = function
| In1 a -> In2 a
| In2 b -> In1 b

let psi2:  ('b, 'a) sum -> ('a, 'b) sum = function
| In2 a -> In1 a
| In1 b -> In2 b

(*----------------------------------------------------------------------------*
 ### $A \times (B \times C) \cong (A \times B) \times C$
[*----------------------------------------------------------------------------*)

let phi3: 'a  * ('b * 'c) -> ('a  * 'b) * 'c = 
fun (a, (b, c)) -> ((a, b), c)

let psi3: ('a  * 'b) * 'c -> 'a  * ('b * 'c) = 
fun ((a, b), c) -> (a, (b, c))

(*----------------------------------------------------------------------------*
 ### $A + (B + C) \cong (A + B) + C$
[*----------------------------------------------------------------------------*)

let phi4: ('a, ('b, 'c) sum) sum -> (('a, 'b) sum, 'c) sum = function
|In1 a -> In1 ( In1 a )
|In2 ( In1 b ) -> In1 ( In2 b )
|In2 ( In2 c) -> In2 c

let psi4: (('a, 'b) sum, 'c) sum -> ('a, ('b, 'c) sum) sum  = function
|In1 ( In1 a) -> In1 a
|In1 ( In2 b ) -> In2 ( In1 b )
|In2 c -> In2 ( In2 c )

(*----------------------------------------------------------------------------*
 ### $A \times (B + C) \cong (A \times B) + (A \times C)$
[*----------------------------------------------------------------------------*)

let phi5: 'a * ('b, 'c) sum -> ('a * 'b,  'a * 'c) sum = function
|( a, In1 b ) -> In1 ( a, b )
|( a, In2 c ) -> In2 ( a, c )

let psi5: ('a * 'b,  'a * 'c) sum -> 'a * ('b, 'c) sum = function
|In1 ( a, b) -> ( a, In1 b )
|In2 ( a, c ) -> (a, In2 c)

(*----------------------------------------------------------------------------*
 ### $A^{B + C} \cong A^B \times A^C$
[*----------------------------------------------------------------------------*)

let phi6: (('b, 'c) sum -> 'a) -> (('b -> 'a) * ('c -> 'a)) = 
fun f -> ( (fun  b ->  f ( In1 b ) ), ( fun c -> f ( In2 c )))

let psi6 : (('b -> 'a) * ('c -> 'a)) -> (('b, 'c) sum -> 'a) = 
fun (g, h)  ->  function
|In1 b -> g b
|In2 c -> h c

(*----------------------------------------------------------------------------*
 ### $(A \times B)^C \cong A^C \times B^C$
[*----------------------------------------------------------------------------*)

let phi7 : ('c -> 'a * 'b) -> (('c -> 'a) * ('c -> 'b)) =
   fun f -> ((fun c -> fst (f c)), (fun c -> snd ( f c ) ))

let psi7 : (('c -> 'a) * ('c -> 'b)) -> ('c -> 'a * 'b) = 
  fun ( g, h ) -> ( fun c -> (g c, h c))


(*----------------------------------------------------------------------------*
 ## Polinomi
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Polinome $a_0 + a_1 x + \cdots + a_n x^n$ predstavimo s seznami celoštevilskih
 koeficientov od prostega do vodilnega člena. Na primer, polinom $1 - 2 x + 3
 x^2$ predstavimo s seznamom `[1; -2; 3]`.
[*----------------------------------------------------------------------------*)

type polinom = int list

(*----------------------------------------------------------------------------*
 ### Odstranjevanje odvečnih ničel
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `pocisti : polinom -> polinom`, ki s konca seznama
 koeficientov odstrani odvečne ničle.
[*----------------------------------------------------------------------------*)

let pocisti (seznam : polinom) = 
  let rec pocisti_pomozna = function
  |[] -> []
  |0 :: ostalo -> pocisti_pomozna ostalo
  |poljuben_seznam -> poljuben_seznam in
( List.rev ( pocisti_pomozna  ( List.rev seznam ) ) : polinom )



let primer_3_1 = pocisti [1; -2; 3; 0; 0]
(* val primer_3_1 : int list = [1; -2; 3] *)

(*----------------------------------------------------------------------------*
 ### Seštevanje
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `( +++ ) : polinom -> polinom -> polinom`, ki sešteje dva
 polinoma.
[*----------------------------------------------------------------------------*)

let ( +++ ) ( polinom1 : polinom ) ( polinom2 : polinom ) = 
  let rec sestej pol1 pol2 acc = match pol1, pol2 with
  | [], [] -> List.rev acc
  | prvi :: rep , [] -> sestej rep [] ( prvi :: acc )
  | [], prvi :: rep -> sestej rep [] ( prvi :: acc )
  | prvi1 :: rep1 , prvi2 :: rep2 ->  sestej rep1 rep2 ( ( prvi1 + prvi2 ) :: acc ) in
( pocisti ( sestej polinom1 polinom2 [] ) : polinom)

let primer_3_2 = [1; -2; 3] +++ [1; 2]
(* val primer_3_2 : int list = [2; 0; 3] *)

let primer_3_3 = [1; -2; 3] +++ [1; 2; -3]
(* val primer_3_3 : int list = [2] *)

(*----------------------------------------------------------------------------*
 ### Množenje
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `( *** ) : polinom -> polinom -> polinom`, ki zmnoži dva
 polinoma.
[*----------------------------------------------------------------------------*)

let ( *** ) ( polinom1 : polinom ) ( polinom2 : polinom) = 
let dolzina_polinoma_1 = List.length polinom1 in

(*Definiramo funkcijo, ki sestavi seznam, kjer n-krat ponovi element x*)
let rec ponovi x n acc1 =
  if n <= 0 then acc1
  else ponovi x ( n - 1 ) (x :: acc1) in

(*Definiramo funkcijo, ki zdruzi 2 seznama (upostevamo, da bo seznam1 vedno sestavljen iz samih 0, zato ne rabimo obračati seznamov)*)
let rec zdruzi_seznama sez1 sez2 =
  match sez1 with
  |[] -> sez2
  |prvi :: rep -> zdruzi_seznama rep ( prvi :: sez2 ) in

(*Zmnožimo polinoma in ustvarimo sezname, kjer je vsak izmed členov pol1 pomnožen z vsemi členi pol2 *)
let rec zmnozi pol1 pol2 acc = match pol1 with
| [] -> acc
| prvi :: rep -> 
    zmnozi rep pol2 ( ( zdruzi_seznama ( ponovi 0 ( dolzina_polinoma_1 - ( List.length pol1 ) ) [] )  (List.map ( ( * ) prvi ) pol2) ) :: acc ) in

(*Dobljene sezname seštejemo*)
( pocisti ( List.fold_left ( +++ ) [] ( zmnozi polinom1 polinom2 [] ) ) : polinom)





let primer_3_4 = [1; 1] *** [1; 1] *** [1; 1]
(* val primer_3_4 : int list = [1; 3; 3; 1] *)

let primer_3_5 = [1; 1] *** [1; -1] 
(* val primer_3_5 : int list = [1; 0; -1] *)

(*----------------------------------------------------------------------------*
 ### Izračun vrednosti v točki
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `vrednost : polinom -> int -> int`, ki izračuna vrednost
 polinoma v danem argumentu.
[*----------------------------------------------------------------------------*)

(*Definiramo funkcijo potence za nenegativna cela števila*)
let rec pomnozi osnova eksponent acc = match eksponent with
|0 -> acc
|_ -> pomnozi osnova ( eksponent - 1 ) ( osnova * acc ) 

(*Ker v razdelku o odvedljivih funkcijah obstaja funkcija z imenom vrednost, spremenimo ime na vrednost_polinoma*)
let rec vrednost_polinoma (polinom : polinom) argument =
let rec vrednost_tlrec arg acc = function
|[] -> acc
|prvi :: ostalo -> 
  vrednost_tlrec arg ( ( prvi * ( pomnozi arg ( List.length ostalo ) 1 ) ) + acc) ostalo in
vrednost_tlrec argument 0 (List.rev polinom) 


let primer_3_6 = vrednost_polinoma [1; -2; 3] 2
(* val primer_3_6 : int = 9 *)

(*----------------------------------------------------------------------------*
 ### Odvajanje
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `odvod : polinom -> polinom`, ki izračuna odvod polinoma.
[*----------------------------------------------------------------------------*)
(*Ker v razdelku o odvedljivih funkcijah obstaja funkcija z imenom odvod, spremenimo ime na odvod_polinoma*)
let odvod_polinoma (polinom : polinom) =
let rec odvod_tlrec acc = function
  | [] -> []
  | prvi :: [] -> acc
  | prvi :: rep -> odvod_tlrec (( prvi * ( List.length rep) ) :: acc) rep in
pocisti (odvod_tlrec [] (List.rev polinom))

let primer_3_7 = odvod_polinoma [1; -2; 3]
(* val primer_3_7 : int list = [-2; 6] *)

(*----------------------------------------------------------------------------*
 ### Lep izpis
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `izpis : polinom -> string`, ki polinom lepo izpiše. Na
 primer, `izpis [1; -2; 3]` vrne `"3 x^2 - 2 x + 1"` oziroma še bolje kot `"3 x²
 - 2 x + 1"`. Pozorni bodite, da izpis začnete z vodilnim členom.
[*----------------------------------------------------------------------------*)

 let izpis (polinom : polinom ) =
  let seznam_eksponentov = ["\u{2070}"; "\u{00b9}"; "\u{00b2}"; "\u{00b3}"; "\u{2074}"; "\u{2075}"; "\u{2076}"; "\u{2077}"; "\u{2078}"; "\u{2079}" ] in
  
  (*Definiramo funkcijo, ki naravna števila preslika v seznam števk*)
  let rec v_stevke acc1 = 
  function
  |0 -> acc1
  |n -> v_stevke ( ( n mod 10 ) :: acc1 ) (n / 10) in

  (*Definiramo funkcijo, ki seznam števk preslika v niz, ki predstavlja eksponent *)
  let rec v_eksponente acc2 = function
  |[] -> String.concat "" (List.rev acc2) 
  | prvi :: rep -> v_eksponente ( (List.nth seznam_eksponentov prvi) :: acc2) rep in

  (*Definiramo funkcijo, ki odstrani odvečne + in - v izpisu polinoma in poskrbi, da ni presledka za prvim - v primeru, da je vodilni člen negativen*)
  let lepsanje = 
    let odstrani_plus_minus = function
    | '+' -> ' '
    | '-' -> ' '
    | x -> x in
    let osnova niz = (String.trim ( String.map odstrani_plus_minus niz)) in
    let dodaj_polepsaj niz = if String.exists (function
    |'-' -> true
    | _ -> false
   ) niz = true then "-" ^ (osnova niz) else osnova niz in
    
  function
  | [] -> []
  | prvi :: rep -> (dodaj_polepsaj prvi) :: rep in


  let rec naredi_izpis polinom1 acc =
    match polinom1 with
    |[] -> lepsanje (List.rev acc)
    
    (*Odstranimo odvečne 0*)
    |0 :: rep -> naredi_izpis rep acc
    (*Zapišemo prosti člen*)
    |prvi :: [] -> (
      if prvi > 0 then naredi_izpis [] (( " + " ^ ( string_of_int prvi )) :: acc ) else
      naredi_izpis [] (( " - " ^ ( string_of_int ( abs prvi ))) :: acc ) 
    )
    (*Zapišemo člen s stopnjo 1*)
    |prvi :: drugi :: [] -> (
      match prvi with
      | 1 -> naredi_izpis (drugi :: []) ( " + x" :: acc )
      | -1 -> naredi_izpis (drugi :: []) ( " - x" :: acc )
      | _ -> if prvi > 0 then naredi_izpis (drugi :: []) ((" + " ^ ( string_of_int prvi ) ^ " x") :: acc) else
        naredi_izpis (drugi :: []) ((" - " ^ ( string_of_int (abs prvi) ) ^ " x") :: acc)
        )
    (*Zapišemo ostale člene*)
    |prvi :: rep ->
      (
      match prvi with
      | 1 -> naredi_izpis rep ( (" + x" ^ (v_eksponente [] (v_stevke [] ( (List.length polinom1) - 1 ) ) )) :: acc) 
      | -1 -> naredi_izpis rep ( (" - x" ^ (v_eksponente [] (v_stevke [] ( (List.length polinom1) - 1 ) ) )) :: acc)
      | _ -> if prvi > 0 then 
      naredi_izpis rep ( (" + " ^ (string_of_int prvi) ^ " x" ^ (v_eksponente [] (v_stevke [] ( (List.length polinom1) - 1 ) ) )) :: acc)
      else naredi_izpis rep ( (" - " ^ (string_of_int (abs prvi)) ^ " x" ^ (v_eksponente [] (v_stevke [] ( (List.length polinom1) - 1 ) ) )) :: acc)
      ) in
      (*Združimo vse elemente seznama v niz*)
String.concat "" ( naredi_izpis (List.rev polinom) [] )
  


 let primer_3_8 = izpis [1; 2; 1]
(* val primer_3_8 : string = "x² + 2 x + 1" *)

let primer_3_9 = izpis [1; 0; -1; 0; 1; 0; -1; 0; 1; 0; -1; 0; 1] 
(* val primer_3_9 : string = "x¹² - x¹⁰ + x⁸ - x⁶ + x⁴ - x² + 1" *)

 let primer_3_10 = izpis [0; -3; 3; -1] 
(* val primer_3_10 : string = "-x³ + 3 x² - 3 x" *)

(*----------------------------------------------------------------------------*
 ## Samodejno odvajanje
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Ob razmahu strojnega učenja, ki optimalno rešitev išče s pomočjo gradientnega
 spusta, si želimo čim bolj enostavno računati odvode. Odvod funkcije $f$ v
 točki $x_0$ lahko seveda ocenimo tako, da v

 $$\frac{f (x_0 + h) - f(x_0)}{h}$$

 vstavimo dovolj majhno število $h$.
[*----------------------------------------------------------------------------*)

let priblizek_odvoda f x0 h =
  (f (x0 +. h) -. f x0) /. h
(* val priblizek_odvoda : (float -> float) -> float -> float -> float = <fun> *)

let primer_4_1 =
  let f x = sin x +. cos x +. exp x in
  List.map (priblizek_odvoda f 1.) [0.1; 0.01; 0.001; 0.0001; 0.00001]
(* val primer_4_1 : float list =
  [2.48914386298364931; 2.42384618742050861; 2.41778190719976749;
   2.41717997997881184; 2.41711983210990411] *)

(*----------------------------------------------------------------------------*
 Pri samodejnem odvajanju izkoristimo to, da poznamo odvode elementarnih
 funkcij, odvode sestavljenih funkcij pa lahko izračunamo iz posameznih odvodov.
 Tako bomo vsako funkcijo predstavili s parom: prvotno funkcijo in njenim
 odvodom.
[*----------------------------------------------------------------------------*)

type odvedljiva = (float -> float) * (float -> float)

let sinus : odvedljiva = (sin, cos)
let kosinus : odvedljiva = (cos, (fun x -> -. sin x))
let eksp : odvedljiva = (exp, exp)
let ( ++. ) : odvedljiva -> odvedljiva -> odvedljiva =
  (* pozorni bodite, da anonimni funkciji v paru date med oklepaje *)
  fun (f, f') (g, g') -> ((fun x -> f x +. g x), (fun x -> f' x +. g' x))
(* val sinus : odvedljiva = (<fun>, <fun>) *)
(* val kosinus : odvedljiva = (<fun>, <fun>) *)
(* val eksp : odvedljiva = (<fun>, <fun>) *)
(* val ( ++. ) : odvedljiva -> odvedljiva -> odvedljiva = <fun> *)

let primer_4_2 =
  let (_, f') = sinus ++. kosinus ++. eksp in
  f' 1.
(* val primer_4_2 : float = 2.41711314951928813 *)

(*----------------------------------------------------------------------------*
 ### Vrednost odvoda
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkciji `vrednost : odvedljiva -> float -> float` in `odvod :
 odvedljiva -> float -> float`, ki izračunata vrednost funkcije in njenega
 odvoda v danem argumentu.
[*----------------------------------------------------------------------------*)

let vrednost : odvedljiva -> float -> float = fun funkcija -> fst(funkcija) 
let odvod : odvedljiva -> float -> float = fun funkcija -> snd(funkcija) 

(*----------------------------------------------------------------------------*
 ### Osnovne funkcije
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkciji `konstanta : float -> odvedljiva` in `identiteta :
 odvedljiva`, ki predstavljata konstantno in identično funkcijo.
[*----------------------------------------------------------------------------*)

let konstanta : float -> odvedljiva = fun a -> ( (fun x -> a), (fun x -> 0.) )
let identiteta : odvedljiva = ( (fun x -> x), (fun x -> 1.) )

(*----------------------------------------------------------------------------*
 ### Produkt in kvocient
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkciji `( **. ) : odvedljiva -> odvedljiva -> odvedljiva` in `( //.
 ) : odvedljiva -> odvedljiva -> odvedljiva`, ki predstavljata produkt in
 kvocient dveh odvedljivih funkcij.
[*----------------------------------------------------------------------------*)

let ( **. ) : odvedljiva -> odvedljiva -> odvedljiva = 
fun (f, f') (g, g') -> ((fun x ->  f x  *.  g x ), fun x -> ( f' x *. g x ) +. ( f x *. g' x ) )

let (  //. ) : odvedljiva -> odvedljiva -> odvedljiva = 
fun ( f, f' ) ( g, g' ) -> 
  ( ( fun x -> ( f x ) /. ( g x ) ), fun x -> ( ( f' x *. g x ) -. ( f x *. g' x ) ) /. ( g x *. g x) )


let kvadrat = identiteta **. identiteta
(* val kvadrat : odvedljiva = (<fun>, <fun>) *)

(*----------------------------------------------------------------------------*
 ### Kompozitum
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `( @@. ) : odvedljiva -> odvedljiva -> odvedljiva`, ki
 predstavlja kompozitum dveh odvedljivih funkcij.
[*----------------------------------------------------------------------------*)

let ( @@. ) : odvedljiva -> odvedljiva -> odvedljiva =
 fun (f, f') (g, g') -> ( ( fun x -> f ( g x ) ), fun x -> (f' ( g x )) *. g' x  )

(* POZOR: Primer je zaenkrat zakomentiran, saj ob prazni rešitvi nima tipa *)
 let vedno_ena = (kvadrat @@. sinus) ++. (kvadrat @@. kosinus) 
(* val vedno_ena : odvedljiva = (<fun>, <fun>) *)

(* POZOR: Primer je zaenkrat zakomentiran, saj brez vedno_ena ne deluje *)
let primer_4_3 = vrednost vedno_ena 12345. 
(* val primer_4_3 : float = 0.999999999999999889 *)

(* POZOR: Primer je zaenkrat zakomentiran, saj brez vedno_ena ne deluje *)
 let primer_4_4 = odvod vedno_ena 12345. 
(* val primer_4_4 : float = 0. *)

(*----------------------------------------------------------------------------*
 ## Substitucijska šifra
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Substitucijska šifra je preprosta šifra, pri kateri črke abecede med seboj
 permutiramo. Na primer, če bi (angleško) abecedo permutirali kot

 ```
 A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
 T H E Q U I C K B R W N F X J M P S O V L A Z Y D G
 ```

 bi besedo `HELLO` šifrirali kot `KUNNJ`. Ključe, s katerimi šifriramo besedila
 bomo predstavili kar z nizi črk, v katere se slikajo črke abecede.
[*----------------------------------------------------------------------------*)

let quick_brown_fox = "THEQUICKBRWNFXJMPSOVLAZYDG"
let rot13 = "NOPQRSTUVWXYZABCDEFGHIJKLM"
(* val quick_brown_fox : string = "THEQUICKBRWNFXJMPSOVLAZYDG" *)
(* val rot13 : string = "NOPQRSTUVWXYZABCDEFGHIJKLM" *)

(*----------------------------------------------------------------------------*
 Včasih bomo v primerih uporabljali tudi krajše ključe, a vedno lahko
 predpostavite, da bodo ključi permutacije začetnega dela abecede. Prav tako si
 pri delu lahko pomagate s funkcijama `indeks` in `crka`:
[*----------------------------------------------------------------------------*)

let indeks c = Char.code c - Char.code 'A'
let crka i = Char.chr (i + Char.code 'A') 
(* val indeks : char -> int = <fun> *)
(* val crka : int -> char = <fun> *)

(*----------------------------------------------------------------------------*
 ### Šifriranje
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `sifriraj : string -> string -> string`, ki besedilo šifrira
 z danim ključem. Vse znake, ki niso velike tiskane črke, pustimo pri miru.
[*----------------------------------------------------------------------------*)

let sifriraj kljuc besedilo = 
  let funkcija_sifrira x = if Char.lowercase_ascii x != x then kljuc.[indeks x] else x in
String.map funkcija_sifrira besedilo


let primer_5_1 = sifriraj quick_brown_fox "HELLO, WORLD!"
(* val primer_5_1 : string = "KUNNJ, ZJSNQ!" *)

let primer_5_2 = "VENI, VIDI, VICI" |> sifriraj rot13
(* val primer_5_2 : string = "IRAV, IVQV, IVPV" *)

let primer_5_3 = "VENI, VIDI, VICI" |> sifriraj rot13 |> sifriraj rot13
(* val primer_5_3 : string = "VENI, VIDI, VICI" *)

(*----------------------------------------------------------------------------*
 ### Inverzni ključ
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `inverz : string -> string`, ki iz ključa izračuna njegov
 inverz.
[*----------------------------------------------------------------------------*)

let inverz niz = 
  let funkcija_inverz x = crka (String.index niz (crka ( String.index niz x ))) in
String.map funkcija_inverz niz

let primer_5_4 = inverz quick_brown_fox
(* val primer_5_4 : string = "VIGYCMZBFOHUPLSQDJRAETKNXW" *)

let primer_5_5 = inverz rot13
(* val primer_5_5 : string = "NOPQRSTUVWXYZABCDEFGHIJKLM" *)

let primer_5_6 = inverz "BCDEA"
(* val primer_5_6 : string = "EABCD" *)

(*----------------------------------------------------------------------------*
 ### Ugibanje ključa
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Včasih seveda ne poznamo ključa, a vemo, da je besedilo v angleščini. Tako
 lahko ključ poskusimo uganiti tako, da šifrirane besede paroma primerjamo z
 besedami iz slovarja, ki smo si ga sposodili [s
 spleta](https://gist.github.com/deekayen/4148741).
[*----------------------------------------------------------------------------*)

let besede = "the of to and a in is it you that he was for on are with as i his they be at one have this from or had by word but what some we can out other were all there when up use your how said an each she which do their time if will way about many then them write would like so these her long make thing see him two has look more day could go come did number sound no most people my over know water than call first who may down side been now find any new work part take get place made live where after back little only round man year came show every good me give our under name very through just form sentence great think say help low line differ turn cause much mean before move right boy old too same tell does set three want air well also play small end put home read hand port large spell add even land here must big high such follow act why ask men change went light kind off need house picture try us again animal point mother world near build self earth father head stand own page should country found answer school grow study still learn plant cover food sun four between state keep eye never last let thought city tree cross farm hard start might story saw far sea draw left late run don't while press close night real life few north open seem together next white children begin got walk example ease paper group always music those both mark often letter until mile river car feet care second book carry took science eat room friend began idea fish mountain stop once base hear horse cut sure watch color face wood main enough plain girl usual young ready above ever red list though feel talk bird soon body dog family direct pose leave song measure door product black short numeral class wind question happen complete ship area half rock order fire south problem piece told knew pass since top whole king space heard best hour better true . during hundred five remember step early hold west ground interest reach fast verb sing listen six table travel less morning ten simple several vowel toward war lay against pattern slow center love person money serve appear road map rain rule govern pull cold notice voice unit power town fine certain fly fall lead cry dark machine note wait plan figure star box noun field rest correct able pound done beauty drive stood contain front teach week final gave green oh quick develop ocean warm free minute strong special mind behind clear tail produce fact street inch multiply nothing course stay wheel full force blue object decide surface deep moon island foot system busy test record boat common gold possible plane stead dry wonder laugh thousand ago ran check game shape equate hot miss brought heat snow tire bring yes distant fill east paint language among grand ball yet wave drop heart am present heavy dance engine position arm wide sail material size vary settle speak weight general ice matter circle pair include divide syllable felt perhaps pick sudden count square reason length represent art subject region energy hunt probable bed brother egg ride cell believe fraction forest sit race window store summer train sleep prove lone leg exercise wall catch mount wish sky board joy winter sat written wild instrument kept glass grass cow job edge sign visit past soft fun bright gas weather month million bear finish happy hope flower clothe strange gone jump baby eight village meet root buy raise solve metal whether push seven paragraph third shall held hair describe cook floor either result burn hill safe cat century consider type law bit coast copy phrase silent tall sand soil roll temperature finger industry value fight lie beat excite natural view sense ear else quite broke case middle kill son lake moment scale loud spring observe child straight consonant nation dictionary milk speed method organ pay age section dress cloud surprise quiet stone tiny climb cool design poor lot experiment bottom key iron single stick flat twenty skin smile crease hole trade melody trip office receive row mouth exact symbol die least trouble shout except wrote seed tone join suggest clean break lady yard rise bad blow oil blood touch grew cent mix team wire cost lost brown wear garden equal sent choose fell fit flow fair bank collect save control decimal gentle woman captain practice separate difficult doctor please protect noon whose locate ring character insect caught period indicate radio spoke atom human history effect electric expect crop modern element hit student corner party supply bone rail imagine provide agree thus capital won't chair danger fruit rich thick soldier process operate guess necessary sharp wing create neighbor wash bat rather crowd corn compare poem string bell depend meat rub tube famous dollar stream fear sight thin triangle planet hurry chief colony clock mine tie enter major fresh search send yellow gun allow print dead spot desert suit current lift rose continue block chart hat sell success company subtract event particular deal swim term opposite wife shoe shoulder spread arrange camp invent cotton born determine quart nine truck noise level chance gather shop stretch throw shine property column molecule select wrong gray repeat require broad prepare salt nose plural anger claim continent oxygen sugar death pretty skill women season solution magnet silver thank branch match suffix especially fig afraid huge sister steel discuss forward similar guide experience score apple bought led pitch coat mass card band rope slip win dream evening condition feed tool total basic smell valley nor double seat arrive master track parent shore division sheet substance favor connect post spend chord fat glad original share station dad bread charge proper bar offer segment slave duck instant market degree populate chick dear enemy reply drink occur support speech nature range steam motion path liquid log meant quotient teeth shell neck"
(* val besede : string =
  "the of to and a in is it you that he was for on are with as i his they be at one have this from or had by word but what some we can out other were all there when up use your how said an each she which do their time if will way about many then them write would like so these her long make thing see h"... (* string length 5837; truncated *) *)

(*----------------------------------------------------------------------------*
 Sestavite vrednost `slovar : string list`, ki vsebuje vse besede iz slovarja,
 pretvorjene v velike tiskane črke.
[*----------------------------------------------------------------------------*)

let slovar =  String.split_on_char ' ' ( String.uppercase_ascii besede)

let primer_5_7 = take 42 slovar
(* val primer_5_7 : string list =
  ["THE"; "OF"; "TO"; "AND"; "A"; "IN"; "IS"; "IT"; "YOU"; "THAT"; "HE";
   "WAS"; "FOR"; "ON"; "ARE"; "WITH"; "AS"; "I"; "HIS"; "THEY"; "BE"; "AT";
   "ONE"; "HAVE"; "THIS"; "FROM"; "OR"; "HAD"; "BY"; "WORD"; "BUT"; "WHAT";
   "SOME"; "WE"; "CAN"; "OUT"; "OTHER"; "WERE"; "ALL"; "THERE"; "WHEN"; "UP"] *)

(* POZOR: Primer je zaenkrat zakomentiran, saj ob prazni rešitvi sproži izjemo *)
 let primer_5_8 = List.nth slovar 321 
(* val primer_5_8 : string = "MEASURE" *)

(*----------------------------------------------------------------------------*
 ### Razširjanje ključa s črko
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Med ugibanjem seveda ne bomo poznali celotnega ključa. V tem primeru bomo za
 neznane črke uporabili znak `_`. Na primer, če bi vedeli, da je črka `A` v
 besedilu šifrirana kot `X`, črka `C` pa kot `Y`, bi ključ zapisali kot
 `"X_Y_______________________"`.

 Napišite funkcijo `dodaj_zamenjavo : string -> char * char -> string option`,
 ki sprejme ključ ter ga poskusi razširiti z zamenjavo dane črke. Funkcija naj
 vrne `None`, če razširitev vodi v ključ, ki ni bijektiven (torej če ima črka že
 dodeljeno drugo zamenjavo ali če smo isto zamenjavo dodelili dvema različnima
 črkama).
[*----------------------------------------------------------------------------*)

(*Ker pri šifriranju ne spreminjamo ločil, definiramo funkcijo, ki nam pove, kdaj par ni primeren, 
  ker je le en izmed njih velika tiskana črka*)
let poskrbi_za_locila (x , y) =
match indeks x with
  | crka when crka >= 0 && crka <= 25 -> if indeks y >= 0 && indeks y <= 25 then true else false
  | _ -> if x = y then true else false

let dodaj_zamenjavo niz (x, y) =

(*Če je le en v paru velika tiskana črka, zamenjava ni primerna. Če noben v paru ni velika tiskana črka, želimo, da sta znaka enaka. 
S tem ne ugotovimo nič novega o ključu. *)
  if poskrbi_za_locila ( x, y ) = false then None else
    if indeks x < 0 || indeks x > 25 then Some niz 
    else 
  
  (*V primeru, da v ključu ni dovolj _, jih dodamo.*)
  let dodaj_  =
    if (indeks x) + 1 > String.length niz  then 
      niz ^ ( String.make ( ( indeks x ) + 1 - String.length niz) '_' )
    else niz in

  (*Definiramo funkcijo, ki nam zamenja znak na primernem mestu.*)
  let funkcija_zamenja i znak = match i with
  |a when a = indeks x -> y
  |_ -> znak in

   (*funkcija_zamenja uporabimo na ključu, kjer ločimo primere z nebijektivnim ključem.*) 
  match (String.index_opt niz y) with 
  | Some a when a != indeks x -> None
  | _ -> (
    match (dodaj_.[indeks x]) with
    | '_' -> Some ( String.mapi funkcija_zamenja dodaj_  )
    | a when a = y ->  Some ( String.mapi funkcija_zamenja dodaj_)  
    | _ -> None )


let primer_5_9 = dodaj_zamenjavo "AB__E" ('C', 'X')
(* val primer_5_9 : string option = Some "ABX_E" *)

let primer_5_10 = dodaj_zamenjavo "ABX_E" ('C', 'X')
(* val primer_5_10 : string option = Some "ABX_E" *)

let primer_5_11 = dodaj_zamenjavo "ABY_E" ('C', 'E')
(* val primer_5_11 : string option = None *)

(*----------------------------------------------------------------------------*
 ### Razširjanje ključa z besedo
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 S pomočjo funkcije `dodaj_zamenjavo` sestavite še funkcijo `dodaj_zamenjave :
 string -> string * string -> string option`, ki ključ razširi z zamenjavami, ki
 prvo besedo preslikajo v drugo.
[*----------------------------------------------------------------------------*)

let dodaj_zamenjave kljuc ( beseda1 , beseda2 ) = 
  if String.length beseda1 != String.length beseda2 then None 
  else
  
  (*Definiramo funkcijo, ki niz preslika v seznam znakov*)
  let rec v_seznam niz1 i acc1 =
     if i < 0 then acc1 else v_seznam niz1 ( i - 1 ) ( niz1.[i] :: acc1 ) in

  (*Definiramo funkcijo, ki iz 2 seznamov enake dolžine sestavi seznam parov.*)
  let rec v_pare ( sez1, sez2 ) acc2 =
    match ( sez1, sez2 ) with
    | ( [], [] ) -> acc2
    | ( prvi1 :: rep1, prvi2 :: rep2 ) -> v_pare ( rep1, rep2 ) ( ( prvi1, prvi2 ) :: acc2 )
    | ( _, _ ) -> failwith "Različne dolžine" in

  (*Definiramo funkcijo, ki naredi zamenjave parov v seznamu parov *)
  let rec naredi_zamenjave novi_kljuc seznam_parov = 
    match seznam_parov with
    | [] -> Some novi_kljuc
    | ( prvi, drugi ) :: rep -> 
     ( match dodaj_zamenjavo novi_kljuc ( prvi, drugi )  with
      | None -> None
      | Some dodano -> naredi_zamenjave dodano rep ) in

(*beseda1 in beseda2 damo v seznam, nato ta seznama razdelimo v pare in naredimo zamenjave*)
naredi_zamenjave kljuc ( v_pare ( v_seznam beseda1 ( String.length beseda1 - 1 ) [], v_seznam beseda2 ( String.length beseda2 - 1 ) []) [])


let primer_5_12 = dodaj_zamenjave "__________________________" ("HELLO", "KUNNJ")
(* val primer_5_12 : string option = Some "____U__K___N__J___________" *)

let primer_5_13 = dodaj_zamenjave "ABCDU_____________________" ("HELLO", "KUNNJ")
(* val primer_5_13 : string option = Some "ABCDU__K___N__J___________" *)

let primer_5_14 = dodaj_zamenjave "ABCDE_____________________" ("HELLO", "KUNNJ")
(* val primer_5_14 : string option = None *)

(*----------------------------------------------------------------------------*
 ### Vse možne razširitve
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Sestavite funkcijo `mozne_razsiritve : string -> string -> string list ->
 string list`, ki vzame ključ, šifrirano besedo ter slovar vseh možnih besed,
 vrne pa seznam vseh možnih razširitev ključa, ki šifrirano besedo slikajo v eno
 od besed v slovarju.
[*----------------------------------------------------------------------------*)

let mozne_razsiritve kljuc sifrirana_beseda slovar = 
  let rec preveri_vse vse_besede seznam_dobrih =
    match vse_besede with
    | [] -> List.rev seznam_dobrih
    | prvi :: rep -> 
       match dodaj_zamenjave kljuc ( sifrirana_beseda, prvi ) with
      | None -> preveri_vse rep seznam_dobrih
      | Some nov_kljuc -> preveri_vse rep ( nov_kljuc :: seznam_dobrih ) in
preveri_vse slovar []


 let primer_5_15 =
  slovar
  |> mozne_razsiritve (String.make 26 '_') "KUNNJ"
  |> List.map (fun kljuc -> (kljuc, sifriraj kljuc "KUNNJ"))  
(* val primer_5_15 : (string * string) list =
  [("_________YC__R______A_____", "CARRY");
   ("_________DS__O______T_____", "STOOD");
   ("_________NG__E______R_____", "GREEN");
   ("_________LW__E______H_____", "WHEEL");
   ("_________PS__E______L_____", "SLEEP");
   ("_________YH__P______A_____", "HAPPY");
   ("_________RF__O______L_____", "FLOOR");
   ("_________DS__E______P_____", "SPEED");
   ("_________DB__O______L_____", "BLOOD");
   ("_________YH__R______U_____", "HURRY");
   ("_________LS__E______T_____", "STEEL");
   ("_________TS__E______H_____", "SHEET")] *)

(*----------------------------------------------------------------------------*
 ### Odšifriranje
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `odsifriraj : string -> string option`, ki sprejme šifrirano
 besedilo in s pomočjo slovarja besed ugane odšifrirano besedilo. Funkcija naj
 vrne `None`, če ni mogoče najti nobenega ustreznega ključa.
[*----------------------------------------------------------------------------*)

(*Definiramo funkcijo, ki nam izbriše vse znake v besedilu razen velikih tiskanih črk in presledkov.
 Prav tako ohrani opuščaj, saj je ta del angleških besed.*)
 (*V primeru, da predpostavimo, da naše besedilo nima ločil oziroma, da so vsa ločila del besede (kot npr. opuščaji), lahko to funkcijo ignoriramo.
 V tem primeru je tudi ne uporabimo na besedilu na začetku naslednje funkcije.*)
let izbris_locil besedilo =
let rec izbrise_locila besedilo i acc = match i with 
| manjse_od_nic when manjse_od_nic < 0 -> acc
| _ -> match besedilo.[i] with
    | '\'' -> izbrise_locila besedilo (i - 1) ("'" ^ acc)
    | velika_crka when (indeks velika_crka <= 25) && (indeks velika_crka >= 0) -> izbrise_locila besedilo (i - 1) ( Char.escaped ( velika_crka ) ^ acc )
    | ' ' -> izbrise_locila besedilo (i - 1) ( " " ^ acc )
    | _ -> izbrise_locila besedilo ( i - 1 ) acc in
izbrise_locila besedilo ( String.length besedilo - 1 ) ""

(*Definiramo funkcijo, ki nam vrne vse možnosti za odšifriranje besedila*)
let vse_moznosti besedilo =  
  (*Iz besedila izbrišemo vsa ločila razen opuščaja in ga ločimo na besede*)
  let na_besede = String.split_on_char ' ' ( String.trim (izbris_locil besedilo )) in
  match na_besede with
  | [] -> failwith "Prazen niz"
  | [""] -> besedilo :: []
  | prva :: vse_razen_prve -> 
    (*Pogledamo si najprej vse možnosti za prvo besedo v besedilu*)
    let moznosti_za_prvo = mozne_razsiritve ( String.make 26 '_' ) prva slovar in

     (*Naredimo rekurzivno funkcijo po besedah in možnih ključih*) 
    let rec po_besedah besede acc1 =
      match besede with 
      | [] -> acc1
      | prva_beseda :: rep_besed -> 
        let rec po_kljucih stari_kljuci novi_kljuci =
          match stari_kljuci with
          | [] -> novi_kljuci
          | prvi_kljuc :: rep_kljucev -> po_kljucih rep_kljucev ((mozne_razsiritve prvi_kljuc prva_beseda slovar) @ novi_kljuci )  in
        po_besedah rep_besed ( po_kljucih acc1 [] ) in
  List.map ( fun f -> f besedilo ) (List.map sifriraj (po_besedah vse_razen_prve moznosti_za_prvo)  )

(*Če imamo kakšno možnost izberemo prvo, drugače funkcija vrne None*)  
let odsifriraj besedilo = 
  match vse_moznosti besedilo with
  | [] -> None
  | prva_moznost :: rep -> Some prva_moznost


let primer_5_16 = sifriraj quick_brown_fox "THIS IS A VERY HARD PROBLEM"
(* val primer_5_16 : string = "VKBO BO T AUSD KTSQ MSJHNUF" *)

 let primer_5_17 = odsifriraj "VKBO BO T AUSD KTSQ MSJHNUF"  
(* val primer_5_17 : string option = Some "THIS IS A VERY HARD PROBLEM" *)

