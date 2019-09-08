let pi = 3.141592653589793
let solar_mass = 4. *. pi *. pi
let days_per_year = 365.24
let ctr= Atomic.make 0 
let arr = Array.make 1 (Some (Domain.self ())) 
let jn=Atomic.make false

type planet = { mutable x : float;  mutable y : float;  mutable z : float;
                mutable vx: float;  mutable vy: float;  mutable vz: float;
                mass : float }
(* type barrier = { } *)

let barrier nt =
    Domain.Sync.critical_section (fun () ->
    if Atomic.get ctr = nt-1 then
    begin
      for i=0 to nt-2 do
        match arr.(i) with
        | Some a-> Domain.Sync.notify a
        | None -> ()
        
      done;
      Atomic.set ctr 0;
    end
      
    else 
      begin
        match arr.(Atomic.get ctr) with
        | Some a-> arr.(Atomic.get ctr)<- Some (Domain.self ())
        | None -> ();
        (* arr.(Atomic.get ctr)<- Domain.self (); *)
        Atomic.incr ctr;
        Domain.Sync.wait (); 
      end 
    )



let advance bodies dt st ed nt =
  (* let n = Array.length bodies - 1 in *)
  for i = st to ed do
    let b = bodies.(i) in
    for j = 0 to Array.length bodies - 1 do
      let b' = bodies.(j) in
      let dx = b.x -. b'.x  and dy = b.y -. b'.y  and dz = b.z -. b'.z in
      let dist2 = dx *. dx +. dy *. dy +. dz *. dz in
      let mag = dt /. (dist2 *. sqrt(dist2)) in
      if i!=j then
      begin
        b.vx <- b.vx -. dx *. b'.mass *. mag;
        b.vy <- b.vy -. dy *. b'.mass *. mag;
        b.vz <- b.vz -. dz *. b'.mass *. mag;
      end
      (* b'.vx <- b'.vx +. dx *. b.mass *. mag;
      b'.vy <- b'.vy +. dy *. b.mass *. mag;
      b'.vz <- b'.vz +. dz *. b.mass *. mag; *)
    done
  done;
  barrier nt;
  for i = st to ed do
    let b = bodies.(i) in
    b.x <- b.x +. dt *. b.vx;
    b.y <- b.y +. dt *. b.vy;
    b.z <- b.z +. dt *. b.vz;
  done

let aux bodies dt nt =
  let n = Array.length bodies - 1 in
  let f1 = Domain.spawn(fun () -> advance bodies dt   0     (n/2)  nt )  and 
  f2 = Domain.spawn(fun () -> advance bodies dt  (n/2+1)   n  nt ) in
  Domain.join f1; Domain.join f2

let energy bodies =
  let e = ref 0. in
  for i = 0 to Array.length bodies - 1 do
    let b = bodies.(i) in
    e := !e +. 0.5 *. b.mass *. (b.vx *. b.vx +. b.vy *. b.vy +. b.vz *. b.vz);
    for j = i+1 to Array.length bodies - 1 do
      let b' = bodies.(j) in
      let dx = b.x -. b'.x  and dy = b.y -. b'.y  and dz = b.z -. b'.z in
      let distance = sqrt(dx *. dx +. dy *. dy +. dz *. dz) in
      e := !e -. (b.mass *. b'.mass) /. distance
    done
  done;
  !e


let offset_momentum bodies =
  let px = ref 0. and py = ref 0. and pz = ref 0. in
  for i = 0 to Array.length bodies - 1 do
    px := !px +. bodies.(i).vx *. bodies.(i).mass;
    py := !py +. bodies.(i).vy *. bodies.(i).mass;
    pz := !pz +. bodies.(i).vz *. bodies.(i).mass;
  done;
  bodies.(0).vx <- -. !px /. solar_mass;
  bodies.(0).vy <- -. !py /. solar_mass;
  bodies.(0).vz <- -. !pz /. solar_mass


let jupiter = { x = 4.84143144246472090e+00;
                y = -1.16032004402742839e+00;
                z = -1.03622044471123109e-01;
                vx = 1.66007664274403694e-03 *. days_per_year;
                vy = 7.69901118419740425e-03 *. days_per_year;
                vz = -6.90460016972063023e-05 *. days_per_year;
                mass = 9.54791938424326609e-04 *. solar_mass;    }

let saturn = { x = 8.34336671824457987e+00;
               y = 4.12479856412430479e+00;
               z = -4.03523417114321381e-01;
               vx = -2.76742510726862411e-03 *. days_per_year;
               vy = 4.99852801234917238e-03 *. days_per_year;
               vz = 2.30417297573763929e-05 *. days_per_year;
               mass = 2.85885980666130812e-04 *. solar_mass;     }

let uranus = { x = 1.28943695621391310e+01;
               y = -1.51111514016986312e+01;
               z = -2.23307578892655734e-01;
               vx = 2.96460137564761618e-03 *. days_per_year;
               vy = 2.37847173959480950e-03 *. days_per_year;
               vz = -2.96589568540237556e-05 *. days_per_year;
               mass = 4.36624404335156298e-05 *. solar_mass;     }

let neptune = { x = 1.53796971148509165e+01;
                y = -2.59193146099879641e+01;
                z = 1.79258772950371181e-01;
                vx = 2.68067772490389322e-03 *. days_per_year;
                vy = 1.62824170038242295e-03 *. days_per_year;
                vz = -9.51592254519715870e-05 *. days_per_year;
                mass = 5.15138902046611451e-05 *. solar_mass;   }

let sun = { x = 0.;  y = 0.;  z = 0.;  vx = 0.;  vy = 0.; vz = 0.;
            mass = solar_mass; }

let bodies = [| sun; jupiter; saturn; uranus; neptune |]

let () =  
  let n = int_of_string(Sys.argv.(1)) and nt=int_of_string(Sys.argv.(2)) in
  offset_momentum bodies;
  Printf.printf "%.9f\n" (energy bodies);
  for i = 1 to n do 
    aux bodies 0.01 nt 
  done;

  Printf.printf "%.9f\n" (energy bodies)
