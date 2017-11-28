
type input = {x1 : int; x2 : int}
and train = input * bool
and train_list = train list
and weight = {w1 : float; w2 : float}

let somme i w = (float_of_int(i.x1)*.w.w1)+.(float_of_int(i.x2)*.w.w2) > 0.5

let up i w r y =
  let yy = if y then 1 else 0 in
  let rr = if r then 1 else 0 in
  w+.0.1*.float_of_int(rr-yy)*.float_of_int(i)

let update i w r y = {w1 = (up i.x1 w.w1 r y); w2 = (up i.x2 w.w2 r y) }

let rec training w tl =
  let res, ww = List.fold_left
      (fun (b,www) (i,r) ->
         let result = somme i www in
         if result == r
         then (b||false,www)
         else (true,(update i www r result)) )
      (false,w) tl in

  Printf.printf "w1 : %s; w2 : %s\n"
    (string_of_float ww.w1) (string_of_float ww.w2);
  if res then training ww tl else ww

(*Main*)
let () =
  let training_list =
    [({x1=0;x2=0},false);
     ({x1=0;x2=1},true);
     ({x1=1;x2=0},true);
     ({x1=1;x2=1},true);] in

  let we = training {w1 = 0.; w2 = 0.} training_list in
  let xe = {x1 = 1; x2 = 0} in
  Printf.printf "\n%d %d -> %b\n" xe.x1 xe.x2 (somme xe we)
