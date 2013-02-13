open Utils
open Intervals
module R = Refract.AST
module FG = Refract.FlowGraph

type prob = float
type update = string * R.aexpr
type command = R.bexpr * (prob * update list) list

type process = (R.var_decl list) * (command list)
type model   = (R.chan_decl list) * (process list)

let cur_proc_id = ref "NULL"

let fill c = c ^ "_fill"
let chan_at c i = c ^ "_" ^ (string_of_int i)
let decr_fill c i = fill c, R.IConst (i-1)
let incr_fill c i = fill c, R.IConst (i+1)
let set_pc i = "pc_" ^ !cur_proc_id, R.IConst i
let ensure_pc i = R.Eq(R.AVar("pc_" ^ !cur_proc_id), R.IConst i)
let ensure_bufsize c i = R.Eq(R.AVar(fill c), R.IConst i)
let initial = FG.initial

let rec conj = function
	| [] -> R.True
	| [b] -> b
	| b::t -> R.And(b, conj t)

let receive_command pc cap c x b pc' =
	(1 --- cap)
	|> List.map (fun i -> (conj [ensure_pc pc; b; ensure_bufsize c i]), [(1.0, [x, R.AVar(chan_at c i); decr_fill c i; set_pc pc'])])

let send_command pc cap c a b pc' =
	(0 -- cap)
	|> List.map (fun i -> (conj [ensure_pc pc; b; ensure_bufsize c i]), [(1.0, [chan_at c (i+1), a; incr_fill c i; set_pc pc'])])

let negate_branch_guard cap = function
	| (_, Some R.Receive(c, _), b) -> R.Not(R.And(R.Gt(R.AVar (fill c), R.IConst 0), R.Not b))
	|	(_, Some R.Send(c, _), b) -> R.Not(R.And(R.Lt(R.AVar (fill c), R.IConst (cap c)), R.Not b))
	| (_, None, b) -> R.Not b

let rec branch_to_commands cap pc pc' = function
	| (_, Some R.Receive(c, x), b), stm -> receive_command pc (cap c) c x b (initial stm) @ (compileStm cap pc' stm)
	| (_, Some R.Send(c, x), b), stm -> send_command pc (cap c) c x b (initial stm) @ (compileStm cap pc' stm)
	| (_, None, b), stm -> [conj [ensure_pc pc; b], [1.0, [set_pc (initial stm)]]] @ (compileStm cap pc' stm)
and compileStm (cap : string -> int) pc' = function
	| R.Skip l ->
		([ensure_pc l, [1.0, [set_pc pc']]] : command list)
	| R.Assign(l, x, a) ->
		[ensure_pc l, [1.0, [x, a; set_pc pc']]]
	| R.Comm(l, R.Receive(c, x)) ->
		receive_command l (cap c) c x R.True pc'
	| R.Comm(l, R.Send(c, a)) ->
		send_command l (cap c) c a R.True pc'
	| R.Seq(stm, stm') ->
		compileStm cap (initial stm') stm @ compileStm cap pc' stm'
	| R.Select(l, branches) ->
		List.map (branch_to_commands cap l pc') branches |> List.concat
	| R.Repeat(l, branches) ->
		let else_guard = conj ((ensure_pc l) :: (branches |> List.map fst |> List.map (negate_branch_guard cap)))
		and branch_commands = List.map (branch_to_commands cap l l) branches |> List.concat in
		(else_guard, [1.0, [set_pc pc']]) :: branch_commands
	| R.Choose(l, branches) ->
		let branch_commands = branches |> List.map trd |> List.map (compileStm cap pc') |> List.concat in
		(ensure_pc l, List.map (fun (_, p, stm) -> p, [set_pc (initial stm)]) branches) :: branch_commands

let rec bexp_to_prism = function
	| R.True 			-> "true"
	| R.False 		-> "false"
	| R.BVar x 		-> x
	| R.Not b 		-> "!(" ^ (bexp_to_prism b) ^ ")"
	| R.Eq(a, b)  -> Printf.sprintf "(%s = %s)" (aexp_to_prism a) (aexp_to_prism b) 
	| R.Lt(a, b)  -> Printf.sprintf "(%s < %s)" (aexp_to_prism a) (aexp_to_prism b) 
	| R.Gt(a, b)  -> Printf.sprintf "(%s > %s)" (aexp_to_prism a) (aexp_to_prism b) 
	| R.And(a, b) -> Printf.sprintf "(%s & %s)" (bexp_to_prism a) (bexp_to_prism b) 
	| R.Or(a, b)  -> Printf.sprintf "(%s | %s)" (bexp_to_prism a) (bexp_to_prism b) 
and aexp_to_prism = function
	| R.AVar x			 -> x
	| R.IConst i 		 -> string_of_int i
	| R.ChanSize c	 -> fill c
	| R.UMinus a     -> "-(" ^ (aexp_to_prism a) ^ ")"
	| R.Minus(a, b)  -> Printf.sprintf "(%s - %s)" (aexp_to_prism a) (aexp_to_prism b)
	| R.Plus(a, b)   -> Printf.sprintf "(%s + %s)" (aexp_to_prism a) (aexp_to_prism b)
	| R.Times(a, b)  -> Printf.sprintf "(%s * %s)" (aexp_to_prism a) (aexp_to_prism b)
	| R.Divide(a, b) -> Printf.sprintf "(%s / %s)" (aexp_to_prism a) (aexp_to_prism b)

let command_to_prism (b, transitions) =
	let update_to_prism (x, a) = Printf.sprintf "(%s'=%s)" x (aexp_to_prism a) in
	let updates_to_prism updates = List.map update_to_prism updates |> String.concat "&" in
	let transitions_to_prism transitions = List.map (fun (p, updates) -> Printf.sprintf "%f:%s" p (updates_to_prism updates)) transitions |> String.concat " + " in
	Printf.sprintf "[] %s -> %s;" (bexp_to_prism b) (transitions_to_prism transitions)

let chan_decls_to_prism (cap : string -> int) chan_range chan_decls =
	let declare_global x inf sup init = Printf.sprintf "global %s : [%d..%d] init %d;" x inf sup init in
	let declare_chan (R.ChanDecl(_, c, _)) =
		(declare_global (c ^"_fill") 0 (cap c) 0) ::
		(List.map (fun i -> declare_global (chan_at c (i+1)) (chan_range c i |> Interval.lower) (chan_range c i |> Interval.upper) (chan_range c i |> Interval.lower)) (0 -- (cap c))) in
	List.map declare_chan chan_decls |> List.flatten |> String.concat "\n"

let declare_var x inf sup init = Printf.sprintf "%s : [%d..%d] init %d;" x inf sup init

let var_decls_to_prism range var_decls =
	let lower = Interval.lower << range
	and upper = Interval.upper << range in
	let init_value x = function None -> lower x | Some n -> n in
	List.map (fun (R.VarDecl(_, x, initial)) -> declare_var x (lower x) (upper x) (init_value x initial)) var_decls |> String.concat "\n" 

let compileProcess cap range (id, var_decls, stm) =
	cur_proc_id := id;
	Printf.sprintf "module %s\n%s\n%s\n%s\nendmodule"
								 id
								 (var_decls_to_prism range var_decls)
								 (declare_var ("pc_" ^ !cur_proc_id)  (initial stm) (FG.finals stm |> List.fold_left max 0) (initial stm))
								 (compileStm cap 0 stm |> List.map command_to_prism |> String.concat "\n")

let compileModel cap chan_range var_range (chan_decls, processes) =
	(chan_decls_to_prism cap chan_range chan_decls) ^ "\n" ^ (List.map (compileProcess cap var_range) processes |> String.concat "\n\n")

