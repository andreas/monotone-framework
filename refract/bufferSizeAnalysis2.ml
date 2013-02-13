open Sig
open Intervals
open Refract

module IWB = IntervalWithBottom

module Reachability =
struct
	type property = Reachable | Unreachable
	
	let bottom = Unreachable
	
	let is_maximal x = Reachable = x
	
	let equal = (=)
	
	let lub x y = if x = Reachable then x else y
	
	let to_string = function
		| Reachable   -> "Reachable"
		| Unreachable -> "Unreachable"
end

module Lattice =
struct
	include Lattices.Tuple(BufferSizeAnalysis.Lattice)(Reachability)
end

module Transfer =
struct
	module V = Refract.FlowGraph.B
  type model = Refract.AST.model
	type vertex = Refract.FlowGraph.vertex
	type state = Lattice.property
	type aux = string -> int
	
	module StringMap = Map.Make(String)
	
	let initial_state model =
		let s = (BufferSizeAnalysis.Transfer.initial_state model, Reachability.Reachable) in
		Printf.printf "Initial state in BS2: %s\n" (Lattice.to_string s); s
	
	let bottom_state model =
		(BufferSizeAnalysis.Transfer.bottom_state model, Reachability.Unreachable)
	
	let preprocess (chans, processes) =
		let bufsizes = List.fold_left (fun acc (AST.ChanDecl(_, c, bufsize)) -> StringMap.add c bufsize acc) StringMap.empty chans in
		fun c -> StringMap.find c bufsizes
	
	let combine ctrl_state comm_state = comm_state
	
	let f (cap : aux) (l : int) (v : vertex) ((bufsize, r) : state) =
		if r = Reachability.Unreachable then (bufsize, r) else
		match v with
		| V.Skip _
		| V.Guard(None, _)
		| V.Prob _
		| V.Assign(_, _)  -> (bufsize, r)
		| V.Guard(Some Refract.AST.Receive(c, _), _)
		| V.Receive(c, _) ->
			(match BufferSizeAnalysis.Lattice.get bufsize c with
				| IWB.Some(_, `Int n) when n > 0 -> (BufferSizeAnalysis.Lattice.decr bufsize c, r)
				| _ 								             -> (bufsize, Reachability.Unreachable))
		| V.Guard(Some Refract.AST.Send(c, _), _)
		| V.Send(c, _) ->
			match BufferSizeAnalysis.Lattice.get bufsize c with
				| IWB.Some(`Int n, _) when n < cap c -> (BufferSizeAnalysis.Lattice.incr bufsize c (cap c), r)
				| _ 										             -> (bufsize, Reachability.Unreachable)
end

module Instance = MonotoneFramework.Make(Lattice)(Refract.FlowGraph)(Transfer)
