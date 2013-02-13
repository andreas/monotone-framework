open Utils
open Intervals
open Facile
open Easy
open Refract

module Lattice =
struct
	module AST = Refract.AST
	module IWB = IntervalWithBottom
	module Names = struct type domain = string let to_string x = x end
	module Variables = Lattices.FunctionSpace(Names)(IntervalWithBottom)
	module Channels = Lattices.FunctionSpace(Names)(Lattices.Sequence(IntervalWithBottom))
	
	include Lattices.Tuple(Variables)(Channels)
	
	let rec eval state bufsize = function
		| AST.IConst n 			-> IWB.create n n
		| AST.AVar x 				-> Variables.get state x
		| AST.ChanSize c		-> bufsize c
		| AST.UMinus e 			-> IWB.minus  (IWB.create 0 0) (eval state bufsize e)
		| AST.Plus(e, e') 	-> IWB.plus   (eval state bufsize e) (eval state bufsize e')
		| AST.Minus(e, e')  -> IWB.minus  (eval state bufsize e) (eval state bufsize e')
		| AST.Times(e, e')  -> IWB.times  (eval state bufsize e) (eval state bufsize e')
		| AST.Divide(e, e') -> IWB.divide (eval state bufsize e) (eval state bufsize e')
end

module Transfer =
struct
	module R = Refract.AST
	module V = Refract.FlowGraph.B
  type model = Refract.AST.model
	type vertex = Refract.FlowGraph.vertex
	type state  = Lattice.property
	type aux    = int -> string -> IntervalWithBottom.property
	
	let initial_state (chans, procs) =
		let bottom_seq n = List.map (fun _ -> IntervalWithBottom.Bottom) (1 --- n) in
		let kappah = List.fold_left (fun acc (R.ChanDecl(_, c, cap)) -> Lattice.Channels.set acc c (bottom_seq cap)) Lattice.Channels.bottom chans in
		let init_value = function None -> IntervalWithBottom.Bottom | Some n -> IntervalWithBottom.create n n in
		let sigmah = procs
							   |> List.map (fun (_, vdecls, _) -> vdecls)
								 |> List.flatten
								 |> List.fold_left (fun acc (R.VarDecl(_, x, n)) -> Lattice.Variables.set acc x (init_value n)) Lattice.Variables.bottom in
		(sigmah, kappah)
	
	let bottom_state ((chans, procs) : R.model) =
		let bottom_seq n = List.map (fun _ -> IntervalWithBottom.Bottom) (1 --- n) in
		let kappah = List.fold_left (fun acc (R.ChanDecl(_, c, cap)) -> Lattice.Channels.set acc c (bottom_seq cap)) Lattice.Channels.bottom chans in
		let sigmah = procs
							   |> List.map (fun (_, vdecls, _) -> vdecls)
								 |> List.flatten
								 |> List.fold_left (fun acc (R.VarDecl(_, x, n)) -> Lattice.Variables.set acc x IntervalWithBottom.Bottom) Lattice.Variables.bottom in
		(sigmah, kappah)
	
	let preprocess model =
		let bsa2 = BufferSizeAnalysis2.Instance.solve model in
		fun l c -> match bsa2 (`Circ l) with (_, BufferSizeAnalysis2.Reachability.Unreachable) -> IntervalWithBottom.Bottom | (bufsizes, _) -> BufferSizeAnalysis.Lattice.get bufsizes c
	
	let combine (sigmah, _) (_, kappah) =	(sigmah, kappah)
	
	let f (bufsize : aux) (l : int) (v : vertex) ((vars, chans) : state) = match v with
		| V.Skip _
		| V.Prob _ -> (vars, chans)
		| V.Guard(None, b) ->
			let new_vars = StringMap.fold (fun k v acc -> Lattice.Variables.set acc k v) (Constraints.solve (Lattice.Variables.get vars) (bufsize l) b) vars in
			(new_vars, chans)
		| V.Assign(x, e) -> (Lattice.Variables.set vars x (Lattice.eval vars (bufsize l) e), chans)
		| V.Guard(Some Refract.AST.Receive(c, x), _)
		| V.Receive(c, x) ->
			let buffer = Lattice.Channels.get chans c in
			(Lattice.Variables.set vars x (List.hd buffer), Lattice.Channels.set chans c (List.tl buffer))
		| V.Guard(Some Refract.AST.Send(c, e), _)
		| V.Send(c, e) ->
			match bufsize l c with
				| IntervalWithBottom.Some (`Int min_size, `Int max_size) ->
					let new_buffer = (list_make min_size IntervalWithBottom.bottom) @ (list_make (max_size - min_size + 1) (Lattice.eval vars (bufsize l) e)) in
					let lubbed_buffer = Lattice.Channels.Range.lub (Lattice.Channels.get chans c) new_buffer in
					(vars, Lattice.Channels.set chans c lubbed_buffer)
				| IntervalWithBottom.Bottom -> failwith "Considered unreachable"
				| _ -> failwith ("Unbounded buffersize for " ^ c ^ " in IntervalAnalysis2.Transfer.f")
end

module Instance = MonotoneFramework.Make(Lattice)(Refract.FlowGraph)(Transfer) 
