open Utils

exception BottomException

module WithBottom(L : Sig.LATTICE) =
struct
	type property = Bottom | Some of L.property
	
	let bottom = Bottom
	
	let is_maximal = function
		| Bottom -> false
		| Some a -> L.is_maximal a
	
	let equal x y = match (x, y) with
		| (Bottom, Bottom) -> true
		| (Some _, Bottom)
		| (Bottom, Some _) -> false
		| (Some a, Some b) -> L.equal a b
	
	let lub x y = match (x, y) with
		| (Some a, Some b) -> Some (L.lub a b)
		| (Bottom, a)
		| (a, Bottom) -> a
	
	let extract = function
		| Bottom -> raise BottomException
		| Some x -> x
	
	let to_string = function
		| Bottom -> "_"
		| Some x -> L.to_string x
end

module WithTop(L : Sig.LATTICE) =
struct
	type property = Top | Some of L.property
	
	let bottom = L.bottom
	
	let is_maximal = function
		| Top 	 -> true
		| Some a -> false
	
	let equal x y = match (x, y) with
		| (Top, Top) 			 -> true
		| (Some _, Top)
		| (Top, Some _) 	 -> false
		| (Some a, Some b) -> L.equal a b
	
	let lub x y = match (x, y) with
		| (Some a, Some b) -> Some (L.lub a b)
		| (Top, _)
		| (_, Top) -> Top
	
	let to_string = function
		| Top -> "¯"
		| Some x -> L.to_string x
end

module Sequence(L : Sig.LATTICE) =
struct
	type property = L.property list
	
	let bottom : property = []
	
	let is_maximal x = List.for_all L.is_maximal x
	
	let equal x y = List.length x = List.length y && List.for_all2 L.equal x y
	
	let rec lub a b = match (a, b) with
		| (x::xs, y::ys) -> (L.lub x y)::(lub xs ys)
		| (xs, [])
		| ([], xs)       -> xs
	
	let to_string x = List.map L.to_string x |> String.concat ", "
end

module Tuple(L1 : Sig.LATTICE)(L2 : Sig.LATTICE) =
struct
	type property = L1.property * L2.property
	
	let bottom = (L1.bottom, L2.bottom)
	
	let is_maximal (a, b) = L1.is_maximal a && L2.is_maximal b
	
	let equal (a, b) (a', b') = L1.equal a a' && L2.equal b b'
	
	let lub (a, b) (a', b') = (L1.lub a a', L2.lub b b')
	
	let to_string (a, b) = Printf.sprintf "(%s, %s)" (L1.to_string a) (L2.to_string b) 
end

module FunctionSpace
  (S : sig type domain val to_string : domain -> string end)
  (L : Sig.LATTICE) =
struct
	module Map = Map.Make(struct type t = S.domain let compare = compare end)
	module Range = L
	type property = L.property Map.t
	
	let bottom : property = Map.empty
	
	let is_maximal (_ : property) = false
	
	let equal =	Map.equal (L.equal)
	
	let to_string x =
    let build_string =
      fun k v acc -> acc ^ Printf.sprintf "%s -> %s | " (S.to_string k) (L.to_string v)
    in Map.fold build_string x "["
	
	let lub (x : property) (y : property) : property =
		let placewise_lub k v acc =
      Map.add k (if Map.mem k acc then L.lub v (Map.find k acc) else v) acc
    in Map.fold placewise_lub x y
	
	let set x k v = Map.add k v x
	
	let get x k = Map.find k x
end 
