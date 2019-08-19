open Domain

let swap arr i j = 
	let temp=arr.(i) in
	arr.(i)<-arr.(j);
	arr.(j)<-temp


let partition arr low high = 
	let x = arr.(high) and  i =ref (low-1) in
	if (high-low > 0) then
	begin
		for j= low to (high-1) do
			if (arr.(j)<=x) then
			begin
				i:= !i+1;
				swap arr !i j
			end
		done 
    end;
    swap arr (!i+1) high;
    !i+1

let rec quicksort arr low high =
	match (high - low) <= 0 with
	| true  -> ()
	| false   ->
		let q = partition arr low high in
		let f = Domain.spawn(fun () -> quicksort arr low (q-1)) and s = Domain.spawn (fun () -> quicksort arr (q+1) high) in
		Domain.join f; Domain.join s   

let () =
	let arr=Array.make 300 0 in
	for i=0 to (Array.length arr -1) do
		arr.(i)<- Random.int 1048576
	done;
	quicksort arr 0 (Array.length arr - 1);
	for i=0 to (Array.length arr -1) do
		print_int arr.(i);print_string "  ";
	done		