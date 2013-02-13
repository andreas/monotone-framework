open Utils
open Facile
open Easy
open Refract
open Intervals

let intervalToFd (x : Interval.property) = match x with
	| (`NInf, `PInf) 			 -> Domain.interval min_int max_int
	| (`NInf, `Int sup) 	 -> Domain.interval min_int sup
	| (`Int inf, `Int sup) -> Domain.interval inf sup
	| (`Int inf, `PInf) 	 -> Domain.interval inf max_int

let iwbToFd = function
	| IntervalWithBottom.Bottom -> Domain.empty
	| IntervalWithBottom.Some int -> intervalToFd int

let rec aexprToCstr (vars : (string, Var.Fd.t) Hashtbl.t) (state : string -> IntervalWithBottom.property) (bufsize : string -> IntervalWithBottom.property) a =
	let recurse = aexprToCstr vars state bufsize in
	match a with
	| AST.AVar x -> 
		if Hashtbl.mem vars x
		then Hashtbl.find vars x |> Arith.fd2e
		else let var = state x |> iwbToFd |> Var.Fd.create ~name:x in
				 Hashtbl.replace vars x var; Arith.fd2e var
	| AST.ChanSize c ->
		if Hashtbl.mem vars c
		then Hashtbl.find vars c |> Arith.fd2e
		else let var = bufsize c |> iwbToFd |> Var.Fd.create ~name:c in
				 Hashtbl.replace vars c var; Arith.fd2e var
	| AST.IConst n 			-> Arith.i2e n
	| AST.UMinus a 			-> Arith.i2e 0 -~ recurse a
	| AST.Minus(a, a') 	-> recurse a -~ recurse a'
	| AST.Plus(a, a') 	-> recurse a +~ recurse a'
	| AST.Times(a, a')  -> recurse a *~ recurse a'
	| AST.Divide(a, a') -> recurse a /~ recurse a'

let rec bexprToCstr vars (state : string -> IntervalWithBottom.property) bufsize = function
	| AST.True 			 -> Cstr.one
	| AST.False 		 -> Cstr.zero
	| AST.BVar _ 		 -> failwith "bvars not supported"
	| AST.Not b 		 -> Reify.not(bexprToCstr vars state bufsize b)
	| AST.Eq(a, a')  -> aexprToCstr vars state bufsize a =~   aexprToCstr vars state bufsize a'
	| AST.Lt(a, a')  -> aexprToCstr vars state bufsize a <~   aexprToCstr vars state bufsize a'
	| AST.Gt(a, a')  -> aexprToCstr vars state bufsize a >~   aexprToCstr vars state bufsize a'
	| AST.And(b, b') -> bexprToCstr vars state bufsize b &&~~ bexprToCstr vars state bufsize b'
	| AST.Or(b, b')  -> bexprToCstr vars state bufsize b ||~~ bexprToCstr vars state bufsize b'

let solve (state : string -> IntervalWithBottom.property) bufsize b =
	try
		let vars = Hashtbl.create 10 in
		let cstr = bexprToCstr vars state bufsize b in
		Cstr.post cstr;
		let result = Hashtbl.fold (fun k v acc -> StringMap.add k (IntervalWithBottom.create (Var.Fd.min v) (Var.Fd.max v)) acc) vars StringMap.empty in
		StringMap.iter (fun k v -> Printf.printf "%s: %s\n" k (IntervalWithBottom.to_string v)) result;
		result
	with Stak.Fail _ -> Printf.printf "Solve failed\n"; StringMap.empty 
