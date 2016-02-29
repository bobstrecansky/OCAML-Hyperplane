## Filename hyperplane.ml
#### Objective: Hyperplane implementation in OCAML [http://mathworld.wolfram.com/Hyperplane.html]
#### Installation Instructions: 
###### Install ocaml software (sudo apt-get install ocaml opam)
###### Switch to a newer version of ocaml for linting (opam switch 4.02.1)
###### Install ocamllint (opam install ocamllint)

Execution: 

Caml version 4.01.0

# #use "anddata.ml";;
val anddata : float list list =
  [[1.; -1.; -1.; -1.]; [1.; -1.; 1.; -1.]; [1.; 1.; -1.; -1.];
   [1.; 1.; 1.; 1.]]
# #use "hyperplane.ml";;
type trainData = float list list
type lr = float
type maxiterations = int
type winit = float list
val get_rand : unit -> float = <fun>
val head_length : 'a list list -> int = <fun>
val create_winit : 'a list list -> float list = <fun>
val remove_head : 'a list -> 'a = <fun>
val create_vector_class : 'a list list -> 'a list = <fun>
val remove_last : 'a list -> 'a list = <fun>
val create_input_vector : 'a list list -> 'a list list = <fun>
val inner : float list * float list -> float = <fun>
val multiply_test : float list list * float list -> float list = <fun>
val cur_class_vect : float list list -> float list -> float list = <fun>
val matrix_multiply : float list list -> float list -> float list = <fun>
val compare_vc_current_class : float list -> float list -> bool = <fun>
val vector_add : float list -> float list -> float list = <fun>
val vector_mult : float list -> float list -> float list = <fun>
val dup : int -> 'a -> 'a list = <fun>
val signal_of_floats : float -> float -> float = <fun>
val my_compare : float list -> float list -> float list = <fun>
val transpose : 'a list list -> 'a list list = <fun>
val ( *- ) : float list -> float list -> float = <fun>
val mult : float list list -> float list -> float list = <fun>
val make_delayed_printer : ('a -> 'b) -> 'a -> unit = <fun>
val print_one_entry : int * float list -> unit = <fun>
val print_it_delayed : int * float list -> unit = <fun>
val sde3 : float list list * float * int * float list -> float list = <fun>
# sde3(anddata,0.25,10,[-0.45237295454474924; -0.171649577008294862; 0.266880082826698506]);;

At iter: 0 current weights are:
-0.452373 -0.171650 0.266880 


At iter: 1 current weights are:
-0.202373 0.078350 0.516880 

- : float list =
[-0.45237295454474924; 0.328350422991705138; 0.266880082826698506]


