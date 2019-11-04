open Mutex
let co = Atomic.make 1
type status_of_barrier =
			| Up
			| Down of Domain.id list


let create_bar ()=
	Atomic.make Up			

 

let rec barrier b  =
(* print_string "start"; *)
	let x = ref false in
	Domain.Sync.critical_section (fun () ->
		(* section_lock mymutex; *)
		let p = Atomic.get b in
		match p with
		|Up->  (
			let li=(Domain.self())::[]in
			if (Atomic.compare_and_set b p (Down li)) then
			(* section_unlock mymutex; *)
			(	print_string "\nwait at up  \n";
				(* Domain.Sync.wait_for   2000000L; *)
				Domain.Sync.wait()
			)
			else
			(* barrier b	 *)
			(
				print_string "\n trying again \n";x:=true;
				(* Domain.Sync.wait_for   2000000L; *)
				print_string " moving "  
			)
		)	

		|Down l ->
			(* let mymutex= create_lock() in  *)
			(
				let ll=	List.length l in
				let count=Atomic.get co in		
				print_string"\n"; (* print_int ll; *)  print_string"\n";

				match (Atomic.compare_and_set co  ll  count)  with
				| true-> (  List.iter (fun x-> Domain.Sync.notify  x ) l;
							Atomic.compare_and_set b p Up;
							print_string"\n All are down\n"
						 )	

				| false -> 

					(  print_string "\nhere in false\n"; 
					let l'= (Domain.self())::l in

					print_string"\n";(* print_int (List.length l') *) print_string"\n";
					
					if ( Atomic.compare_and_set  b p  (Down l')) then
						(
							print_string "\n  wait at down  \n";
							Domain.Sync.wait()	
						)
					else
						(* barrier b 	 *)
						(
							print_string " trying again at down ";x:=true;
							(* Domain.Sync.wait_for   2000000L; *)
							(* print_string "" *)
						)
					)	

			)			
		

	);
	if ( !x )  then
	( x:= false;barrier b )

let task n  =
	for i = n to n+10 do
  		print_int i;print_string "  ";
  	done;
  	print_string "  \n"

let task1 mybar = 
	task 1  ;
	barrier mybar;
	print_string "  \n";
	task 101    	

(* let () =
print_string "1";
	 
print_string "2";
	(* let mymutex=create_lock () in *)
	let mybar=create_bar () in
print_string "3\n\n";  	
  	let f1 = Domain.spawn(fun () -> 
	  	task1 mybar  ;
		(* barrier mybar mymutex;
		task 101  *) 
    )  and

    f2 = Domain.spawn(fun () -> 
	  	task1 mybar  ;
		barrier mybar mymutex;
		task 101 
    )  and
    f3= Domain.spawn(fun () -> 
	  	task1 mybar  ;
		(* barrier mybar mymutex;
		task 101  *)
    ) and

    f4 = Domain.spawn(fun () -> 
	  	task1 mybar ; 
    )   in
    (* print_string"hello "; *)
    (* task1 mybar  ;  *)

  Domain.join f1; Domain.join f2; Domain.join f3;Domain.join f4; 
  print_string "done\n"	  *)