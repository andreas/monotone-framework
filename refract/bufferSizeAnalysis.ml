open Intervals
open Utils

module IWB = IntervalWithBottom

module Lattice =
struct
	include Lattices.FunctionSpace(struct type domain = string let to_string x = x end)(IWB)

	let incr x c cap = set x c (IWB.glb (IWB.plus (get x c) (IWB.create 1 1)) (IWB.Some(`NInf, `Int cap)))
	
	let decr x c = set x c (IWB.glb (IWB.plus (get x c) (IWB.create (-1) (-1))) (IWB.Some(`Int 0, `PInf)))
end

module Transfer =
struct
	module R = Refract.AST
	module V = Refract.FlowGraph.B
  type model = Refract.AST.model
	type vertex = Refract.FlowGraph.vertex
	type state  = Lattice.property
	type aux = string -> int
	
	let preprocess (chans, _) =
		let mapping = List.fold_left (fun acc (R.ChanDecl(_, c, cap)) -> StringMap.add c cap acc) StringMap.empty chans in
		fun c -> StringMap.find c mapping
	
	let initial_state (chans, _) =
		List.fold_left (fun acc (R.ChanDecl(_, c, _)) -> Lattice.set acc c (IWB.create 0 0)) Lattice.bottom chans
	
	let bottom_state (chans, _) =
		List.fold_left (fun acc (R.ChanDecl(_, c, _)) -> Lattice.set acc c (IWB.Bottom)) Lattice.bottom chans
	
	let combine ctrl_state comm_state = comm_state
	
	let f (cap : aux) _ (v : vertex) (p : state) = match v with
		| V.Skip _
		| V.Guard(None, _)
		| V.Prob _
		| V.Assign(_, _)  -> p
		| V.Guard(Some Refract.AST.Receive(c, _), _)
		| V.Receive(c, _) -> Lattice.decr p c
		| V.Guard(Some Refract.AST.Send(c, _), _)
		| V.Send(c, _) 		-> Lattice.incr p c	(cap c)
end

module Instance = MonotoneFramework.Make(Lattice)(Refract.FlowGraph)(Transfer)
