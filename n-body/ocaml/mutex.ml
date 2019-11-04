type section =
			| Free
			| Taken of Domain.id list

let create_lock ()=
	Atomic.make  Free

let rec section_lock r =
	Domain.Sync.critical_section (fun () ->
	match Atomic.get r with
	|Free->( if (Atomic.compare_and_set r Free (Taken [])) then ()
				else
				section_lock r

			(* print_string "*free of lock* " *)
			)


	|Taken l ->
		(

		let l'= (Domain.self())::l in 
		if (Atomic.compare_and_set  r  (Taken l)  (Taken l'))then
		(
			(* print_string" * taken of lock * "; *)
			Domain.Sync.wait ()
			
		);
		section_lock r
		)
	)
	


let rec section_unlock r =
	Domain.Sync.critical_section (fun () ->
	match Atomic.get r with
	| Free -> assert false
	| (Taken []) as v->
		(
		 if (Atomic.compare_and_set r v Free) then ()
		
		(* print_string  " * taken ([] to free) unlock * \n"; *)
		)

	| Taken(x::xs) as v->
		(
		if Atomic.compare_and_set r v (Taken xs) then
		Domain.Sync.notify x;
		print_string  " * taken (taken to taken) unlock * \n "
		);
		section_unlock r
	)







(* let task1 n  =
	for i = n to n+10 do
  		print_string "  ^  ";print_int i;print_string "  "
  	done;
  	print_string "  \n"




let task n mylock =
	(* Domain.Sync.critical_section (fun () -> *)
	section_lock mylock;
	for i = n to n+10 do
  		print_string "  ^  ";print_int i;print_string "  ";
  	done;
  	print_string "  \n";
  	section_unlock mylock
  (* ) *)
 

let () = 
	let mylock=create_lock () in
  	let f1 = Domain.spawn(fun () -> 
	  	(* (* task 1 mylock *)print_string " Domian 1 \n"; *)
	  	task 101 mylock
    )  and

    f2 = Domain.spawn(fun () -> 
	  	(* (* task 21 mylock *)print_string" Domian 2 \n"; *)
	  	task 2001 mylock
	  	(* section_lock mylock;   *)
		(* task1 2001   *)
		section_unlock mylock;
		
    ) and

    f3 = Domain.spawn(fun () -> 
	  	(* (* task 21 mylock *)print_string" Domian 3 \n"; *)
	  	task 3001 mylock
	  	(* section_lock mylock;   *)
		(* task1 3001   *)
		(* section_unlock mylock; *)
		
    ) and

    f4 = Domain.spawn(fun () -> 
	  	(* (* task 21 mylock *)print_string" Domian 4 \n"; *)
	  	task 4001 mylock
	  (* 	section_lock mylock;  
		task1 4001 ;
		section_unlock mylock; *)
		
    ) in


    
	(* section_lock mylock;  *)
	task 9001  mylock;
	(* section_unlock mylock;
	section_lock mylock;  *)
	task  800001 mylock  ;
	(* section_unlock mylock; *)



	Domain.join f1; Domain.join f2; Domain.join f3; Domain.join f4;
	print_string "done\n"
 *)	
