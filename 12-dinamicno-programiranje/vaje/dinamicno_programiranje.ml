(* ========== Vaje 6: Dinamično programiranje  ========== *)


(*----------------------------------------------------------------------------*]
 Požrešna miška se nahaja v zgornjem levem kotu šahovnice. Premikati se sme
 samo za eno polje navzdol ali za eno polje na desno in na koncu mora prispeti
 v desni spodnji kot. Na vsakem polju šahovnice je en sirček. Ti sirčki imajo
 različne (ne-negativne) mase. Miška bi se rada kar se da nažrla, zato jo
 zanima, katero pot naj ubere.

 Funkcija [max_cheese cheese_matrix], ki dobi matriko [cheese_matrix] z masami
 sirčkov in vrne največjo skupno maso, ki jo bo miška požrla, če gre po
 optimalni poti.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # max_cheese test_matrix;;
 - : int = 13
[*----------------------------------------------------------------------------*)

let test_matrix = 
  [| [| 1 ; 2 ; 0 |];
     [| 2 ; 4 ; 5 |];
     [| 7 ; 0 ; 1 |] |]
(* 
let max_cheese cheese_matrix = 
   let max_cheese_aux cheese_matrix i j dolzina= 
      if (i < dolzina - 1) && (j < Array.length cheese_matrix - 1 ) then
         cheese_matrix.(i).(j) + max ((Array.init (dolzina-1) (fun k -> cheese_matrix.(i + 1 + k))), Array.init  ) *)
       
    
let dobi_stolpce matrix i j = 
   Array.init (Array.length matrix - i) (fun m -> Array.init ((Array.length matrix)-1 - j) (fun k -> matrix.(m + i ).(k +(j+1))))

let dobi_vrstice matrix i j = 
   Array.init ((Array.length matrix)-1 - i) (fun m -> Array.init (Array.length matrix - j) (fun k -> matrix.(m + i +1).(k+j)) )

(*DOdas se, da ce je na robu vrstice gleda samo dol, ce v zadnji vrstici pa samo desno ce je cis na koncu pa doda samo zadnbji element*)

(*----------------------------------------------------------------------------*]
 Poleg količine sira, ki jo miška lahko poje, jo zanima tudi točna pot, ki naj
 jo ubere, da bo prišla do ustrezne pojedine.

 Funkcija [optimal_path] naj vrne optimalno pot, ki jo mora miška ubrati, da se
 čim bolj nažre. Ker je takih poti lahko več, lahko funkcija vrne poljubno.
 Pripravite tudi funkcijo [convert_path], ki pot pretvori v seznam tež sirčkov
 na poti.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # optimal_path_bottom test_matrix;;
 - : mouse_direction list = [Right; Down; Down; Right; Down]
 # optimal_path test_matrix |> convert_path test_matrix;;
 - : int list = [1; 2; 4; 5; 1]
[*----------------------------------------------------------------------------*)

type mouse_direction = Down | Right


(*----------------------------------------------------------------------------*]
 Rešujemo problem sestavljanja alternirajoče obarvanih stolpov. Imamo štiri
 različne tipe gradnikov, dva modra in dva rdeča. Modri gradniki so višin 2 in
 3, rdeči pa višin 1 in 2.

 Funkcija [alternating_towers] za podano višino vrne število različnih stolpov
 dane višine, ki jih lahko zgradimo z našimi gradniki, kjer se barva gradnikov
 v stolpu izmenjuje (rdeč na modrem, moder na rdečem itd.). Začnemo z gradnikom
 poljubne barve.

 Namig: Uporabi medsebojno rekurzivni pomožni funkciji z ukazom [and].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # alternating_towers 10;;
 - : int = 35
[*----------------------------------------------------------------------------*)
let alternating_towers height = 
   let rec redbottom height  =
      if height < 0 then 0 else 
         if height <= 2 then 1 else  (*ker ce je 2 in je rdeci na vrsti lahko samo rdecega 2 vrne ker 2krat po 1 ne sme ker bosta dva rdec zapored*)  
         bluebottom (height - 1) +  bluebottom (height - 2)
   and 
   bluebottom height = 
      if height <= 1  then 0 else (*ce 1 potem to ni rešitev vrne 0 ker  1 ni noben od modrih*)
      if height <= 2 then 1 else
      redbottom (height - 2 ) + redbottom (height - 3 )
   in 
   redbottom height + bluebottom height


(*----------------------------------------------------------------------------*]
 Izračunali smo število stolpov, a naše vrle gradbince sedaj zanima točna
 konfiguracija. Da ne pride do napak pri sestavljanju, bomo stolpe predstavili
 kar kot vsotne tipe. 

 Stolp posamezne barve so temelji (Bottom), ali pa kot glava bloka pripadajoče
 barve in preostanek, ki je stolp nasprotne barve.

 Definirajte funkcijo [enumerate_towers], ki vrne seznam vseh stolpov podane
 dolžine. Stolpe lahko vrne v poljubnem vrstnem redu. Funkcija naj hitro (in
 brez) prekoračitve sklada deluje vsaj do višine 20.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # enumerate_towers 4;;
 - : tower list = 
    [Red (TopRed (Red2, TopBlue (Blue2, RedBottom)));
     Red (TopRed (Red1, TopBlue (Blue3, RedBottom)));
     Red (TopRed (Red1, TopBlue (Blue2, TopRed (Red1, BlueBottom))));
     Blue (TopBlue (Blue3, TopRed (Red1, BlueBottom)));
     Blue (TopBlue (Blue2, TopRed (Red2, BlueBottom)))]
[*----------------------------------------------------------------------------*)


type blue_block = Blue3 | Blue2
type red_block = Red2 | Red1

type red_tower = TopRed of red_block * blue_tower | RedBottom
and blue_tower = TopBlue of blue_block * red_tower | BlueBottom

type tower = Red of red_tower | Blue of blue_tower

(*----------------------------------------------------------------------------*]
 Vdrli ste v tovarno čokolade in sedaj stojite pred stalažo kjer so ena ob
 drugi naložene najboljše slaščice. Želite si pojesti čim več sladkorja, a
 hkrati poskrbeti, da vas ob pregledu tovarne ne odkrijejo. Da vas pri rednem
 pregledu ne odkrijejo, mora biti razdalija med dvema zaporednima slaščicama,
 ki ju pojeste vsaj `k`.

 Napišite funkcijo [ham_ham], ki sprejme seznam naravnih števil dolžine `n`, ki
 predstavljajo količino sladkorja v slaščicah v stalaži in parameter `k`,
 najmanjšo razdalijo med dvema slaščicama, ki ju še lahko varno pojeste.
 Funkcija naj vrne seznam zastavic `bool`, kjer je `i`-ti prižgan natanko tedaj
 ko v optimalni požrtiji pojemo `i`-to slaščico.

 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # ham_ham test_shelf 1;;
 - : bool list = [false; true; false; true; false; true; false; true; false]
 # ham_ham test_shelf 2;;
 - : bool list = [false; true; false; false; false; true; false; false; false]
[*----------------------------------------------------------------------------*)

let test_shelf = [1;2;-5;3;7;19;-30;1;0]
