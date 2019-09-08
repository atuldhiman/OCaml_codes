let niter = 50
let limit = 4.
let workers = 64

let worker w h_lo h_hi =
  let buf =
    Bytes.create ((w / 8 + (if w mod 8 > 0 then 1 else 0)) * (h_hi - h_lo))
  and ptr = ref 0 in
  let fw = float w /. 2. in
  let fh = fw in
  let red_w = w - 1 and red_h_hi = h_hi - 1 and byte = ref 0 in
  for y = h_lo to red_h_hi do
    let ci = float y /. fh -. 1. in
    for x = 0 to red_w do
      let cr = float x /. fw -. 1.5
      and zr = ref 0. and zi = ref 0. and trmti = ref 0. and n = ref 0 in
      begin try
	while true do
	  zi := 2. *. !zr *. !zi +. ci;
	  zr := !trmti +. cr;
	  let tr = !zr *. !zr and ti = !zi *. !zi in
	  if tr +. ti > limit then begin
	    byte := !byte lsl 1;
	    raise Exit
	  end else if incr n; !n = niter then begin
	    byte := (!byte lsl 1) lor 0x01;
	    raise Exit
	  end else
	    trmti := tr -. ti
	done
      with Exit -> ()
      end;
      if x mod 8 = 7 then begin
	Bytes.set buf !ptr (Char.chr !byte);
	incr ptr;
	byte := 0
      end
    done;
    let rem = w mod 8 in
    if rem != 0 then begin
      Bytes.set buf !ptr (Char.chr (!byte lsl (8 - rem)));
      incr ptr;
      byte := 0
    end
  done;
  buf

let aux w rows rem t1 t2 num =
  let mybuf =  Bytes.create num in 
  let ctr= ref 0 in
  for i = t1 to t2 do
    let red_i = i - 1 in
    let buf = worker w (red_i * rows + min red_i rem) (i * rows + min i rem) in
    
    for i= 0 to ((Bytes.length buf)-1)   do
      let temp = Bytes.get buf i in
      Bytes.set mybuf (!ctr) temp;
      ctr:= !ctr + 1;
    done

  done;
  mybuf

let _ = 
  let w = int_of_string (Array.get Sys.argv 1) in
  let rows = w / workers and rem = w mod workers in
  Printf.printf "P4\n%i %i\n%!" w w;

  let f1 = Domain.spawn(fun () -> aux  w rows rem   1    (workers/4)  8000000) and
  f2 = Domain.spawn(fun () -> aux w rows rem  ((workers/4)+1)   (workers/2)   8000000) and
  f3 = Domain.spawn(fun () -> aux w rows rem  ((workers/2)+1)  ((3*workers)/4) 8000000) and
  f4 = Domain.spawn (fun () -> aux  w rows rem  (((3*workers)/4)+1)  workers  8000000) in
  let buf1=Domain.join f1 and 
  buf2=Domain.join f2 and
  buf3=Domain.join f3 and 
  buf4=Domain.join f4 in
  Printf.printf "%a%!" output_bytes buf1;
  Printf.printf "%a%!" output_bytes buf2;
  Printf.printf "%a%!" output_bytes buf3;
  Printf.printf "%a%!" output_bytes buf4
  (* for i = 0 to (Bytes.length buf1 ) do
    print_int cnt.(i);print_string "  ";
  done *)

  (* 8000000  8500000  8500000  8500000 *)