module type OBSLUGA_KOLEJKI =
sig
  type 'a tk
  exception Pusta of string
  val tworz_pusta: unit -> 'a tk
  val do_kolejki: 'a * 'a tk -> 'a tk
  val z_kolejki: 'a tk -> 'a tk
  val pierwszy_element: 'a tk -> 'a
  val kolejka_pusta: 'a tk -> bool
end;;

module Kolejka : OBSLUGA_KOLEJKI =
struct
    type 'a tk = 'a list
    exception Pusta of string

    let tworz_pusta() = []
    let do_kolejki(e, queue) = queue @ [e]
    let z_kolejki queue =
        match queue with
        | h::t -> t
        | _ -> []
    let pierwszy_element queue =
        match queue with
        | h::t -> h
        | _ -> raise (Pusta "kolejka pusta")
    let kolejka_pusta queue =
        match queue with
        | [] -> true
        | _ -> false
end;;


(*testy QueueList*)
let qu = Kolejka.(do_kolejki(3, tworz_pusta()));;
Kolejka.pierwszy_element qu;;
Kolejka.(kolejka_pusta (z_kolejki qu));;
Kolejka.(pierwszy_element (z_kolejki qu));;


let qu1 = Kolejka.(do_kolejki(4, do_kolejki(5,qu)));;
Kolejka.pierwszy_element qu1;;
Kolejka.(pierwszy_element (z_kolejki qu1));;
