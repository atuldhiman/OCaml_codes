let eval_A i j = 1. /. float((i+j)*(i+j+1)/2+i+1)


let au_1 u v t1 t2=
  let n = Array.length v - 1 in
  for i = t1 to  t2 do
    let vi = ref 0. in
      for j = 0 to n do vi := !vi +. eval_A i j *. u.(j) done;
      v.(i) <- !vi
  done

(* let au_2 u v =
  let n = Array.length v - 1 in
  for i = n/2 to  n do
    let vi = ref 0. in
      for j = 0 to n do vi := !vi +. eval_A i j *. u.(j) done;
      v.(i) <- !vi
  done *)


let eval_A_times_u u v =
  let n = Array.length v - 1 in
  let f1 = Domain.spawn(fun () -> au_1 u v   0     (n/4)  )  in 
  let f2 = Domain.spawn(fun () -> au_1 u v  (n/4)   (n/2) ) in
  let f3 = Domain.spawn(fun () -> au_1 u v  (n/2)  ((3*n)/4)) in
  let f4 = Domain.spawn(fun () -> au_1 u v  ((3*n)/4)   n    )    in
  Domain.join f1; Domain.join f2;
  Domain.join f3; Domain.join f4



let aux_1 u v t1 t2 =
  let n = Array.length v - 1 in
  for i = t1 to t2 do
    let vi = ref 0. in
      for j = 0 to n do vi := !vi +. eval_A j i *. u.(j) done;
      v.(i) <- !vi
  done
(* let aux_2 u v =
  let n=Array.length v-1 in
  for i = n/2 to n do
    let vi = ref 0. in
      for j = 0 to n do vi := !vi +. eval_A j i *. u.(j) done;
      v.(i) <- !vi
  done *)


let eval_At_times_u u v =
  let n = Array.length v - 1 in
  let f1 = Domain.spawn(fun () -> aux_1 u v  0     (n/4) ) and 
  f2 = Domain.spawn(fun () -> aux_1 u v  (n/4)   (n/2) ) and
  f3 = Domain.spawn(fun () -> aux_1 u v  (n/2)  ((3*n)/4)) and
  f4 = Domain.spawn (fun () -> aux_1 u v  ((3*n)/4)  n) in
  Domain.join f1; Domain.join f2;
  Domain.join f3; Domain.join f4

let eval_AtA_times_u u v =
  let w = Array.make (Array.length u) 0.0 in
  eval_A_times_u u w; eval_At_times_u w v


let () =
  let n = try int_of_string(Array.get Sys.argv 1) with _ ->  2000 in
  let u = Array.make n 1.0  and  v = Array.make n 0.0 in
  for i = 0 to 9 do
    eval_AtA_times_u u v; eval_AtA_times_u v u
  done;

  let vv = ref 0.0  and  vBv = ref 0.0 in
  for i=0 to n-1 do
    vv := !vv +. v.(i) *. v.(i);
    vBv := !vBv +. u.(i) *. v.(i)
  done;
  Printf.printf "%0.9f\n" (sqrt(!vBv /. !vv))


(*   1.274224153

real  0m1.637s
user  0m3.197s
sys 0m0.056s *)