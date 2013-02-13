open Utils

module AST =
struct
	type typename =
		| Int
		| Bool
	
	type var_decl  = VarDecl of typename * string * int option
	
	type chan_decl = ChanDecl of typename * string * int
	
	type aexpr =
		| AVar		 of string
		| ChanSize of string
		| IConst	 of int
		| UMinus	 of aexpr
		| Minus		 of aexpr * aexpr
		| Plus		 of aexpr * aexpr
		| Times		 of aexpr * aexpr
		| Divide	 of aexpr * aexpr
	
	type bexpr =
		| True
		| False
		| BVar of string
		| Not  of bexpr
		| Eq   of aexpr * aexpr
		| Lt   of aexpr * aexpr
		| Gt   of aexpr * aexpr
		| And  of bexpr * bexpr
		| Or   of bexpr * bexpr
	
	type expr =
		| AExpr of aexpr
		| BExpr of bexpr
	
	type comm =
		| Receive of string * string
		| Send    of string * aexpr
	
	type guard = int * comm option * bexpr
	
	and stm =
		| Skip   of int
		| Assign of int * string * aexpr
		| Comm   of int * comm
		| Seq    of stm * stm
		| Select of int * (guard * stm) list
		| Repeat of int * (guard * stm) list
		| Choose of int * (int * float * stm) list
	
	type process = string * var_decl list * stm
	
	type model = chan_decl list * process list
end

module FlowGraph =
struct
		module B =
		struct
      type model = AST.model
      
			type t =
				| Skip
				| Assign  of string * AST.aexpr
				| Receive of string * string
				| Send    of string * AST.aexpr
				| Guard   of AST.comm option * AST.bexpr
				| Prob    of float
			
			let rec bexp_to_string = function
				| AST.True 			-> "true"
				| AST.False 		-> "false"
				| AST.BVar x 		-> x
				| AST.Not b 		-> "!(" ^ (bexp_to_string b) ^ ")"
				| AST.Eq(a, b)  -> Printf.sprintf "%s = %s" (aexp_to_string a) (aexp_to_string b) 
				| AST.Lt(a, b)  -> Printf.sprintf "%s < %s" (aexp_to_string a) (aexp_to_string b) 
				| AST.Gt(a, b)  -> Printf.sprintf "%s > %s" (aexp_to_string a) (aexp_to_string b) 
				| AST.And(a, b) -> Printf.sprintf "%s & %s" (bexp_to_string a) (bexp_to_string b) 
				| AST.Or(a, b)  -> Printf.sprintf "%s | %s" (bexp_to_string a) (bexp_to_string b) 
			and aexp_to_string = function
				| AST.AVar x			 -> x
				| AST.ChanSize c   -> "|" ^ c ^ "|"
				| AST.IConst i 		 -> string_of_int i
				| AST.UMinus a     -> "-(" ^ (aexp_to_string a) ^ ")"
				| AST.Minus(a, b)  -> Printf.sprintf "%s - %s" (aexp_to_string a) (aexp_to_string b)
				| AST.Plus(a, b)   -> Printf.sprintf "%s + %s" (aexp_to_string a) (aexp_to_string b)
				| AST.Times(a, b)  -> Printf.sprintf "%s * %s" (aexp_to_string a) (aexp_to_string b)
				| AST.Divide(a, b) -> Printf.sprintf "%s / %s" (aexp_to_string a) (aexp_to_string b)
			and comm_to_string = function
				| AST.Receive(c, x) -> Printf.sprintf "%s?%s" c x
				| AST.Send(c, a) 		-> Printf.sprintf "%s!%s" c (aexp_to_string a)
			and to_string = function
				| Skip 					 -> "skip"
				| Assign(x, a)   -> Printf.sprintf "%s := %s" x (aexp_to_string a)
				| Receive(c, x)  -> Printf.sprintf "%s?%s" c x
				| Send(c, a) 		 -> Printf.sprintf "%s!%s" c (aexp_to_string a)
				| Guard(None, b) -> bexp_to_string b
				| Guard(Some comm, AST.True) ->  comm_to_string comm
				| Guard(Some comm, b) ->  (bexp_to_string b) ^ " && " ^ (comm_to_string comm)
				| Prob p -> string_of_float p
		end
	
	include FlowGraph.Make2(B)
	
	let rec simplify = function
		| AST.Or(b, AST.False)
		| AST.Or(AST.False, b)
		| AST.And(AST.True, b)
		| AST.And(b, AST.True)  	-> simplify b
		| AST.And(AST.False, _)
		| AST.And(_, AST.False) 
		| AST.Not(AST.True)     	-> AST.False
		| AST.Not(AST.False)    	-> AST.True
		| AST.Not(AST.And(b, b')) -> simplify(AST.Or(simplify(AST.Not(b)), simplify(AST.Not(b'))))
		| AST.Not(AST.Or(b, b'))  -> simplify(AST.And(simplify(AST.Not(b)), simplify(AST.Not(b'))))
		| b                     	-> b
	
	let rec initial = function
		| AST.Skip(l)
		| AST.Assign(l, _, _)
		| AST.Comm(l, _) 
		| AST.Repeat(l, _)
		| AST.Choose(l, _)
		| AST.Select(l, _)		-> l
		| AST.Seq(s, _) 			-> initial s
	
	let rec initials = function
		| AST.Skip(l)
		| AST.Assign(l, _, _)
		| AST.Comm(l, _)			-> [l]
		| AST.Seq(s, _) 			-> initials s
		| AST.Select(_, gs)		-> gs |> List.map (fst3 << fst)
		| AST.Repeat(l, gs)		-> l :: (gs |> List.map (fst3 << fst))
		| AST.Choose(_, gs)		-> gs |> List.map fst3
	
	let rec finals = function
		| AST.Skip(l) 				-> [l]
		| AST.Assign(l, _, _) -> [l]
		| AST.Comm(l, _) 			-> [l]
		| AST.Seq(_, s)				-> finals s
		| AST.Select(_, gs)		-> gs |> List.map (fun (_, s) -> finals(s)) |> List.concat
		| AST.Repeat(l, _)		-> [l]
		| AST.Choose(_, gs)		-> gs |> List.map (fun (_, _, s) -> finals(s)) |> List.concat
	
	let modelFinals (_, processes) =
		processes
		|> List.map trd
		|> List.map finals
		|> List.concat
	
	let negate_guard = function
		| (_, None, b) -> AST.Not(b)
		| (_, Some(AST.Receive(c, _)), b) -> AST.Not(AST.And(AST.Gt(AST.ChanSize(c), AST.IConst 0), b))
		| (_, Some(AST.Send(c, a)), b) -> AST.Not(AST.And(AST.Lt(AST.ChanSize(c), AST.IConst 20), b))
	
	let rec blocks = function
		| AST.Skip(l) 									 -> [(l, B.Skip)]
		| AST.Assign(l, x, a) 					 -> [(l, B.Assign(x, a))]
		| AST.Comm(l, AST.Receive(c, x)) ->	[(l, B.Receive(c, x))]
		| AST.Comm(l, AST.Send(c, a)) 	 -> [(l, B.Send(c, a))]
		| AST.Seq(s, s') 								 -> blocks(s) @ blocks(s')
		| AST.Select(_, gs) ->
			gs
			|> List.map (fun ((l, comm, b), s) -> (l, B.Guard(comm, b))::(blocks s))
			|> List.concat
		| AST.Repeat(l, gs) ->
			let elseGuard = (l, B.Guard(None, simplify(gs |> List.map (negate_guard << fst) |> List.fold_left (fun acc b -> AST.And(acc, b)) AST.True))) in
			elseGuard::(gs
			|> List.map (fun ((l, comm, b), s) -> (l, B.Guard(comm, b))::(blocks s))
			|> List.concat)
		| AST.Choose(_, gs) ->
			gs
			|> List.map (fun (l, p, s) -> (l, B.Prob(p))::(blocks s))
			|> List.concat
	
	let receives blocks c =
		let recFilter = function
			| (_, B.Receive(c', _)) 
			| (_, B.Guard(Some(AST.Receive(c', _)), _)) when c = c' -> true
			| _ -> false
		in List.filter recFilter blocks |> List.map fst
	
	let modelFlow ((chans, processes) : AST.model) =
		let graph = create() in
		let pBlocks = Hashtbl.create 10 in
		processes |> List.map (initials << trd)
							|> List.concat
							|> List.iter (extremal graph);
		List.iter (fun (pid, _, stm) -> Hashtbl.replace pBlocks pid (blocks stm)) processes;
		Hashtbl.iter (fun pid blocks -> List.iter (add graph pid) blocks) pBlocks;
		
		let add_edge (i, j) = connect graph i j in
		let add_edges ls    = ls |> List.concat |> List.iter add_edge in
		let rec stmFlow = function
				| AST.Skip _
				| AST.Assign _
				| AST.Comm(_, AST.Receive _) -> ()
				| AST.Comm(l, AST.Send(c, _)) ->
						cartesian_product [l] (Hashtbl.fold (fun _ v acc -> (receives v c)@acc) pBlocks [])
						|> List.iter (uncurry(connect graph ~label:E.Comm))
				| AST.Seq(s, s') ->
						stmFlow s;
						cartesian_product (finals s) (initials s')
						|> List.iter add_edge;
						stmFlow s';
				| AST.Select(_, gs) ->
						gs
						|> List.map (fun ((l, _, _), s) -> stmFlow s; cartesian_product [l] (initials s))
						|> add_edges
				| AST.Repeat(l, gs) ->
						gs
						|> List.map (fun ((l, _, _), s) -> cartesian_product [l] (initials s))
						|> add_edges;
						
						List.iter (stmFlow << snd) gs;
						
						let guard_labels = l::(gs |> List.map (fst3 << fst)) in
						let final_labels = gs	|> List.map (finals << snd) |> List.concat in
						cartesian_product final_labels guard_labels |> List.iter add_edge
				| AST.Choose(_, gs) ->
						gs
						|> List.map (fun (l, _, s) -> stmFlow s; cartesian_product [l] (initials s))
						|> add_edges
		in
		let processFlow (pid, _, stm) =
			let blocks = Hashtbl.find pBlocks pid in
			Hashtbl.replace pBlocks pid [];
			stmFlow stm;
			Hashtbl.replace pBlocks pid blocks
		in
		List.iter processFlow processes;
		graph
end
