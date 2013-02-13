open Utils

module Interval =
struct
	type property = [`NInf | `Int of int] * [`Int of int | `PInf]
	let equal = (=)
	
	let is_maximal x = x = (`NInf, `PInf)
	let bottom = (`Int 0, `Int 0)
	
	let to_string (inf, sup) =
		Printf.sprintf "[%s, %s]"
		(match inf with `Int n -> string_of_int n | `NInf -> "-inf")
		(match sup with `Int n -> string_of_int n | `PInf -> "inf") 
	
	let minInf x y = match (x, y) with
		| (`NInf, _)
		| (_, `NInf) -> `NInf
		| (`Int a, `Int b) -> `Int (min a b)
	
	let maxInf x y = match (x, y) with
		| (`NInf, a)
		| (a, `NInf) -> a
		| (`Int a, `Int b) -> `Int (max a b)
	
	let minSup x y = match (x, y) with
		| (`PInf, a)
		| (a, `PInf) -> a
		| (`Int a, `Int b) -> `Int (min a b)

	let maxSup x y = match (x, y) with
		| (`PInf, _)
		| (_, `PInf) -> `PInf
		| (`Int a, `Int b) -> `Int (max a b)
	
	let lub (inf, sup) (inf', sup') = (min inf inf', max sup sup')
	
	let glb (inf, sup) (inf', sup') = match (maxInf inf inf', minSup sup sup') with
		| (`Int a, `Int b) when a > b -> failwith "Empty glb!"
		| x -> x
	
	let as_ints : property -> int * int = function
		| (`Int a, `Int b) -> (a, b)
		| _ -> failwith "Interval is unbounded!"
	
	let lower = fst << as_ints
	let upper = snd << as_ints
	
	let infOp op x y = match (x, y) with
		| (`Int a, `Int b) -> `Int (op a b)
		| (`Int _, inf)
		| (inf, _) -> inf
	
	let intervalOp op (inf, sup) (inf', sup') =
		(infOp op inf inf', infOp op sup sup')
	
	let plus   = intervalOp ( + )
	let minus  = intervalOp ( - )
	let times  = intervalOp ( * )
	let divide = intervalOp ( / )
end

module IntervalWithBottom =
struct
	include Lattices.WithBottom(Interval)

	let safeBinOp op x y = match (x, y) with
		| (Bottom, _)
		| (_, Bottom) -> Bottom
		| (Some a, Some b) -> Some (op a b)
	
	let plus   = safeBinOp Interval.plus
	let minus  = safeBinOp Interval.minus
	let times  = safeBinOp Interval.times
	let divide = safeBinOp Interval.divide
	
	let glb = safeBinOp Interval.glb
	
	let create inf sup = Some(`Int inf, `Int sup)
end