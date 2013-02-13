let cartesian_product xs ys =
	List.concat (List.map (fun x -> List.map (fun y -> (x, y)) ys) xs)

let (|>) x f = f x
let (<<) f g x = f(g(x))

let id = fun x -> x

let fst3 (x, _, _) = x
let trd (_, _, x) = x

let list_make i x =
  if i < 0 then invalid_arg "list_make";
  let rec make' x = function
    | 0 -> []
    | i -> x :: make' x (i-1)
  in
  make' x i
	
let (---) lower upper =
	let rec helper u i =
		if i > u then [] else i :: (helper u (i + 1))
	in helper upper lower

let (--) lower upper = lower --- (upper - 1)

module StringMap = Map.Make(String)

let uncurry f = fun (x, y) -> f x y
let curry f = fun x y -> f (x, y) 

let dualize f (xs, ys) =
	f xs, f ys

let dual_map f (xs, ys) = dualize (List.map f) (xs, ys)

let dual_fold_left f acc (xs, ys) = dualize (List.fold_left f acc) (xs, ys)