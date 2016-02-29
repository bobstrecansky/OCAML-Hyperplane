(* Bob Strecansky *)
(*   sde3.caml    *)

type trainData = float list list
type lr = float
type maxiterations = int
type winit = float list

(*Initialize random number sequence *)
let () = Random.self_init();;

(* get a random number from -0.5 to 0.5 *)
let get_rand() = (Random.float 1.0) -. 0.5;;

(* gets # of columns *)
let head_length trainData =	List.length(List.hd trainData)

(*initialize a new list *) 

let create_winit trainData =
	let rec loop w_initial = function 1 -> w_initial | x -> loop (get_rand() :: w_initial) (x - 1) in loop [] (head_length(trainData))
;;
    
(*save only the last element from the one row *)	
let remove_head x = List.hd(List.rev x)

(*save only the last element from each row to create the vector class*)
let create_vector_class matrix = List.map remove_head matrix

(*remove the last element from the one row *)	
let remove_last x = List.rev (List.tl (List.rev x))

(*remove the last element from each row to create the input vector*)
let create_input_vector matrix = List.map remove_last matrix

(* this will be inner.caml *)
let rec inner = function [],[] -> 0.0 | i::is,w::ws -> i*.w +. inner (is,ws) | _ -> failwith "different length lists"

(*multiply a matrix and a vector together *)	
let rec multiply_test = function  [],_ -> [] | m::ms, v -> inner(m,v) :: multiply_test(ms,v)

(* create the current class vector *)
let cur_class_vect inData w = multiply_test(create_input_vector inData, w)

let matrix_multiply inData w = multiply_test(inData, w)

(*Compares 2 vectors: if correct = true then bool = "true" *)
let compare_vc_current_class input1 input2 = List.for_all2 (fun x y -> x > 0.0 && y > 0.0 || x < 0.0 && y < 0.0) input1 input2

(*Add 2 vectors together *)
let vector_add input1 input2 = List.map2 (+.) input1 input2

(*Mult 2 vectors together *)
let vector_mult input1 input2 = List.map2 ( *. ) input1 input2

(*duplicate the learning rate to create a learning rate vector *)
(* n = int  := number of times you want to duplicate*)
(* x = float:= you want to duplicate*)
let dup n x = 
	let rec f n accum =
		if n <= 0 then accum
		else f (n - 1) (x :: accum) in f n []

let signal_of_floats a b =
  if a < 0.0 && b < 0.0 then
    0.0
  else if a > 0.0 && b > 0.0 then
    0.0
  else
    a

let rec my_compare a b =
  match a, b with
    | [], [] -> []
    | a::b, []
    | [], a::b -> failwith "my_compare: the lists has different sizes"
    | a::r, b::s -> (signal_of_floats a b)::(my_compare r s)
    
(*let dotproduct = List.fold_left2 (fun x a b -> x +. a *. b) 0.0;; *)
let transpose m = List.map List.rev (List.fold_left (List.map2 (fun xs x -> x::xs)) (List.map (fun x -> [x]) (List.hd m)) (List.tl m));;
(* let mul m1 m2 = List.map (fun v1 -> List.map (fun v2 -> dotproduct v1 v2) (transpose m2)) m1;; *)

let ( *- ) a b = List.fold_left2 (fun x a b -> x +. a *. b) 0.0 a b	
let mult matrix vector = List.map (fun x -> x *- vector) matrix

let make_delayed_printer print_function =
  let temporary = ref None in
  let delayed_match value = match !temporary with
     | None -> Some value
     | Some stored -> print_function stored; Some value in
  let delayed_printer value =
    temporary := (delayed_match value) in
  delayed_printer

let print_one_entry (count, w_current) =
  Printf.printf "\nAt iter: %d current weights are:\n" count;
  List.iter (Printf.printf "%f ") w_current;	
  Printf.printf "\n\n"

let print_it_delayed =
  make_delayed_printer print_one_entry

(* sde3 function *)
let sde3(trainData, lr, maxiterations, winit)=
	
	let print_it_delayed = make_delayed_printer print_one_entry in

	let lr_size = (head_length(trainData)-1) in
		if lr_size = 0 then failwith "lr_size can't be 0"
	;

	let lr_vector = dup lr_size lr in
		if lr_vector = [] then failwith "lr vector can't be empty"
	;
	
	(*CREATE THE INPUT VECTOR*)
	let input_vector  = create_input_vector trainData in
		if input_vector = [] then failwith "input_vector can't be empty"
	;	

	(*CREATE VECTOR CLASS*)
	let vector_class  = create_vector_class trainData in
		if vector_class = [] then failwith "vector class cant be empty"
	;

	(*CREATE W_CURRENT*)
	let w_current = winit in
		if w_current = [] then failwith "w_current cant be empty"
	;
	
	(*CREATE THE CURRENT CLASS*)
	let current_class = cur_class_vect trainData winit in
		if current_class = [] then failwith "current class cant be empty"
	;		
	
	(*Printf.printf "Vector Class: [";
	List.iter (Printf.printf "%f ") vector_class;	
	Printf.printf "]\n";
	
	Printf.printf "Current Class: [";
	List.iter (Printf.printf "%f ") current_class;	
	Printf.printf "]\n\n\n";
	*)
	
	let new_mult = my_compare vector_class current_class in
		if new_mult = [] then failwith "new_mult cant be empty"
	;

	(*Printf.printf "new_mult: ";
	List.iter (Printf.printf "%f ") new_mult;	
	Printf.printf "\n";
	*)
	
	let new_vect = matrix_multiply (transpose(create_input_vector trainData)) new_mult in
		if new_vect = [] then failwith "new_vect cant be empty"
	;

	(*
	Printf.printf "new_vect: ";
	List.iter (Printf.printf "%f ") new_vect;	
	Printf.printf "\n";
	*)
	let error_vect = 	vector_mult lr_vector new_vect in
		if error_vect = [] then failwith "error_vect cant be empty"
	;	
	
	(*
	Printf.printf "error_vect: ";
	List.iter (Printf.printf "%f ") error_vect;	
	Printf.printf "\n";
	*)

	let rec finish w_current current_class new_mult new_vect error_vect count= 
		
		let is_finished =  compare_vc_current_class vector_class current_class in
		

			
			if is_finished = true || count = maxiterations then  w_current
			else begin
			
			print_it_delayed(count, w_current);

				
				(*
				Printf.printf "at At iter: %d w_current: " !count;
				List.iter (Printf.printf "%f ") w_current;	
				Printf.printf "\n";

				Printf.printf "at At iter: %d current_class: " !count;
				List.iter (Printf.printf "%f ") current_class;	
				Printf.printf "\n";
																
				Printf.printf "at At iter: %d new_mult: " !count;
				List.iter (Printf.printf "%f ") new_mult;	
				Printf.printf "\n";
			
				Printf.printf "at At iter: %d new_vector is: " !count;
				List.iter (Printf.printf "%f ") new_vect;	
				Printf.printf "\n";	

				Printf.printf "at At iter: %d lr_vector is: " !count;
				List.iter (Printf.printf "%f ") lr_vector;	
				Printf.printf "\n";
							
				Printf.printf "at At iter: %d error_vect: " !count;
				List.iter (Printf.printf "%f ") error_vect;	
				Printf.printf "\n";
				*)				

				let current_class = cur_class_vect trainData w_current in
				let new_mult = my_compare vector_class current_class in
				let new_vect = matrix_multiply (transpose(create_input_vector trainData)) new_mult in
				let error_vect = vector_mult lr_vector new_vect in
				let w_current = vector_add error_vect w_current in

				finish w_current current_class new_mult new_vect error_vect (count+1)
			end
	in
  	finish w_current current_class new_mult new_vect error_vect 0
;

