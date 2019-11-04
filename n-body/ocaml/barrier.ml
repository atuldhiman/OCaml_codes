let v = Atomic.make true
let mutex= ref []
let barrier= ref []
let b = Atomic.make 1

let up ()=
	if(Atomic.get v =false )then
	if not ((List.length mutex)=0) then
	let temp=List.hd mutex in
	mutex:=List.tl mutex; 
	Domain.Sync.notify temp
	

let do_ ()=
	Atomic.decr b;
	if ((Atomic.get b) = 0) then
	begin
		List.iter (fun x-> Domain.Sync.notify x ) !barrier;
		Atomic.set v true;
		()
	end	
	else
	begin
		barrier := ( Domain.self() ) ::barrier; 
		up();
		Atomic.set v true;
		Domain.Sync.wait()
	end

let rec down ()=
	Domain.Sync.critical_section (fun () ->
			if(Atomic.compare_and_set v true false)then
			begin
				do_ ()
				(* up(); *)
			end
			
			else
			begin
				mutex:= (Domain.self()) ::mutex; 
				Domain.Sync.wait(); 	
				down()
			end
	)

let task n =
	for i = n to n+10 do
  		print_int i;print_string "  ";
  	done


let () =
  let f1 = Domain.spawn(fun () -> 
  	task 1;
  	down();
  	task 101
    )  and

  f2 = Domain.spawn(fun () -> 
  	task 21;
  	down();
  	task 2001
    ) in
  (* down (); *)
  Domain.join f1; Domain.join f2;
  print_string "done"