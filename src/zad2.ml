let sumFunc xss =
  let rec sumFunc_rec xs res =
    match xs with
      [] -> res
     |h::t -> sumFunc_rec t ((List.fold_left (fun sum el -> sum+el) 0 h)::res)
  in (List.rev (sumFunc_rec xss []));;

sumFunc [[1;3;5;1;1;1];[];[0];[2;2;2;3];[7;2;4;5]];;
