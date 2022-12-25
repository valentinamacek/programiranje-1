(* This is an OCaml editor.
   Enter your program here and send it to the toplevel using the "Eval code"
   button or [Ctrl-e]. *)

(* ========== Vaja 3: Definicije Tipov  ========== *)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Pri modeliranju denarja ponavadi uporabljamo racionalna števila. Problemi se
 pojavijo, ko uvedemo različne valute.
 Oglejmo si dva pristopa k izboljšavi varnosti pri uporabi valut.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Definirajte tipa [euro] in [dollar], kjer ima vsak od tipov zgolj en
 konstruktor, ki sprejme racionalno število.
 Nato napišite funkciji [euro_to_dollar] in [dollar_to_euro], ki primerno
 pretvarjata valuti (točne vrednosti pridobite na internetu ali pa si jih
 izmislite).

 Namig: Občudujte informativnost tipov funkcij.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dollar_to_euro;;
 - : dollar -> euro = <fun>
 # dollar_to_euro (Dollar 0.5);;
 - : euro = Euro 0.4305
[*----------------------------------------------------------------------------*)
type euro = Euro of float
  
type dollar = Dollar of float 
    
let dollar_to_euro = function 
  | Dollar x -> Euro (x *. 0.97)
 
                                 


(*----------------------------------------------------------------------------*]
 Definirajte tip [currency] kot en vsotni tip z konstruktorji za jen, funt
 in švedsko krono. Nato napišite funkcijo [to_pound], ki primerno pretvori
 valuto tipa [currency] v funte.

 Namig: V tip dodajte še švicarske franke in se navdušite nad dejstvom, da vas
        Ocaml sam opozori, da je potrebno popraviti funkcijo [to_pound].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # to_pound (Yen 100.);;
 - : currency = Pound 0.007
[*----------------------------------------------------------------------------*)
type currency = 
  | Yen of float 
  | Pound of float
  | Sv_krona of float 
  | Frank of float
      
let to_pound = function
  | Yen x  -> Pound ( x *. 0.00007 ) 
  | Sv_krona x -> Pound (x *. 545.43) 
  | Pound x -> Pound x 
  | Frank x -> Pound (x *. 2.24)
                    
(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Želimo uporabljati sezname, ki hranijo tako cela števila kot tudi logične
 vrednosti. To bi lahko rešili tako da uvedemo nov tip, ki predstavlja celo
 število ali logično vrednost, v nadaljevanju pa bomo raje konstruirali nov tip
 seznamov.

 Spomnimo se, da lahko tip [list] predstavimo s konstruktorjem za prazen seznam
 [Nil] (oz. [] v Ocamlu) in pa konstruktorjem za člen [Cons(x, xs)] (oz.
 x :: xs v Ocamlu).
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Definirajte tip [intbool_list] z konstruktorji za:
  1.) prazen seznam,
  2.) člen z celoštevilsko vrednostjo,
  3.) člen z logično vrednostjo.

 Nato napišite testni primer, ki bi predstavljal "[5; true; false; 7]".
[*----------------------------------------------------------------------------*)
(* definiras tip nabora ki ga vzame int  *)
type intbool_list = 
  | Prazen 
  | Int of (int * intbool_list ) 
  | Bool of (bool * intbool_list )
    
let primer = Int(5, Bool(true, Bool(false, Int(7, Prazen))))
    
type izraz =
  | Stevilo of int
  | Plus of izraz * izraz
  | Minus of izraz
  | Krat of izraz * izraz

  let i = Minus (
       Krat (Stevilo 5, Plus (Stevilo 2, Stevilo 7))
     )

(*----------------------------------------------------------------------------*]
 Funkcija [intbool_map f_int f_bool ib_list] preslika vrednosti [ib_list] v nov
 [intbool_list] seznam, kjer na elementih uporabi primerno od funkcij [f_int]
 oz. [f_bool].
[*----------------------------------------------------------------------------*)

let rec intbool_map f_int f_bool = function
  | Prazen -> Prazen
  | Int(x, ib_lst) ->  Int(f_int x, intbool_map (f_int) (f_bool) (ib_lst))
  | Bool(x, ib_lst) -> Bool(f_bool x, intbool_map (f_int) (f_bool) (ib_lst))
                         

(*----------------------------------------------------------------------------*]
 Funkcija [intbool_reverse] obrne vrstni red elementov [intbool_list] seznama.
 Funkcija je repno rekurzivna.
[*----------------------------------------------------------------------------*)

let rec intbool_reverse sez = 
  let rec reverse_aux acc = function
    | Prazen -> acc
    | Int( x, xs) -> reverse_aux (Int(x, acc)) (xs)
    | Bool(x, xs) -> reverse_aux (Bool(x, acc)) (xs)
  in 
  reverse_aux Prazen sez 
    
    

(*----------------------------------------------------------------------------*]
 Funkcija [intbool_separate ib_list] loči vrednosti [ib_list] v par [list]
 seznamov, kjer prvi vsebuje vse celoštevilske vrednosti, drugi pa vse logične
 vrednosti. Funkcija je repno rekurzivna in ohranja vrstni red elementov.
[*----------------------------------------------------------------------------*)
(* ko definiras nabor moras: (x , y ) tip nabora pa je: tipx * tipy *)
let rec intbool_separate ib_lst = 
  let rec separate_aux accbool accint ib_lst = 
    match ib_lst with
    | Prazen -> ( accbool , accint)
    | Int(x, xs) -> separate_aux (accbool) (x :: accint) (xs)
    | Bool(x, xs) -> separate_aux (x :: accbool) (accint) (xs)
  in 
  separate_aux [] [] ib_lst

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Določeni ste bili za vzdrževalca baze podatkov za svetovno priznano čarodejsko
 akademijo "Effemef". Vaša naloga je konstruirati sistem, ki bo omogočil
 pregledno hranjenje podatkov.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Čarodeje razvrščamo glede na vrsto magije, ki se ji posvečajo. Definirajte tip
 [magic], ki loči med magijo ognja, magijo ledu in magijo arkane oz. fire,
 frost in arcane.

 Ko se čarodej zaposli na akademiji, se usmeri v zgodovino, poučevanje ali
 raziskovanje oz. historian, teacher in researcher. Definirajte tip
 [specialisation], ki loči med temi zaposlitvami.
[*----------------------------------------------------------------------------*)
type magic = Fire | Ice | Arcana
             
type specialisation = Historian | Teacher | Researcher 
                      

(*----------------------------------------------------------------------------*]
 Vsak od čarodejev začne kot začetnik, nato na neki točki postane študent,
 na koncu pa SE lahko tudi zaposli.
 Definirajte tip [status], ki določa ali je čarodej:
  a.) začetnik [Newbie],
  b.) študent [Student] (in kateri vrsti magije pripada in koliko časa študira),
  c.) zaposlen [Employed] (in vrsto magije in specializacijo).

Nato definirajte zapisni tip [wizard] z poljem za ime in poljem za trenuten
  status.
    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # professor;;
- : wizard = {name = "Matija"; status = Employed (Fire, Teacher)}
  [*----------------------------------------------------------------------------*)
type status = 
  | Newbie 
  | Student of magic * int
  | Employed of magic * specialisation
                
type wizard = { name : string; status: status}
              
let profesor = {name = "Matija"; status = Employed (Fire, Teacher)}

(*----------------------------------------------------------------------------*]
 Želimo prešteti koliko uporabnikov posamezne od vrst magije imamo na akademiji.
 Definirajte zapisni tip [magic_counter], ki v posameznem polju hrani število
 uporabnikov magije.
 Nato definirajte funkcijo [update counter magic], ki vrne nov števec s
 posodobljenim poljem glede na vrednost [magic].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # update {fire = 1; frost = 1; arcane = 1} Arcane;;
 - : magic_counter = {fire = 1; frost = 1; arcane = 2}
[*----------------------------------------------------------------------------*)
type magic_counter = { fire : int; ice : int; arcana : int}
let update counter = function
  | Fire -> {counter with fire = counter.fire + 1}
  | Ice -> {counter with ice = counter.ice + 1}
  | Arcana -> {counter with arcana = counter.arcana + 1}


(*----------------------------------------------------------------------------*]
 Funkcija [count_magic] sprejme seznam čarodejev in vrne števec uporabnikov
 različnih vrst magij.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # count_magic [profesor; profesor; profesor];;
 - : magic_counter = {fire = 3; ice = 0; arcana = 0}
[*----------------------------------------------------------------------------*)
let count_magic sez = 
  let rec count_aux counter = function 
    | [] -> counter
    | {name; status} :: wizards -> ( match 
    status with 
        | Newbie -> count_aux (counter) (wizards)
        | Student(mag, int) -> count_aux (update (counter) mag ) (wizards)
        | Employed(mag,_) -> count_aux (update (counter) mag) (wizards) )
  in 
  count_aux {fire = 0; ice = 0; arcana = 0} sez
       




(*----------------------------------------------------------------------------*]
 Želimo poiskati primernega kandidata za delovni razpis. Študent lahko postane
 zgodovinar po vsaj treh letih študija, raziskovalec po vsaj štirih letih
 študija in učitelj po vsaj petih letih študija.
 Funkcija [find_candidate magic specialisation wizard_list] poišče prvega
 primernega kandidata na seznamu čarodejev in vrne njegovo ime, čim ustreza
 zahtevam za [specialisation] in študira vrsto [magic]. V primeru, da ni
 primernega kandidata, funkcija vrne [None].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let jaina = {name = "Jaina"; status = Student (Ice, 4)};;
 # find_candidate Ice Researcher [professor; jaina];;
 - : string option = Some "Jaina"
[*----------------------------------------------------------------------------*)
let ugodna_leta specialisation leta = 
       match specialisation with
       | Historian -> if leta >= 3 then true else false
       | Researcher -> if leta >= 4 then true else false
       | Teacher -> if leta >=5 then true else false

let rec find_candidate magic specialisation = function
  | [] -> None 
  | {name; status} :: wizardi -> (match status with
     | Newbie -> find_candidate (magic) (specialisation) (wizardi)
     | Student(mag, leto) -> if mag = magic && ugodna_leta (specialisation) (leto) then Some name else find_candidate (magic) (specialisation) (wizardi)
     | Employed(mag, _) -> find_candidate (magic) (specialisation) (wizardi)
  )
 
