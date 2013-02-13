open Intervals
open Constraints

module Lattice =
struct
	module AST = Refract.AST
	module IWB = IntervalWithBottom
	
	include Lattices.FunctionSpace(struct type domain = string let to_string x = x end)(IntervalWithBottom)
	
	let rec eval state = function
		| AST.IConst n 			-> IWB.create n n
		| AST.AVar x 				-> get state x
		| AST.UMinus e 			-> IWB.minus  (IWB.create 0 0) (eval state e)
		| AST.Plus(e, e') 	-> IWB.plus   (eval state e) (eval state e')
		| AST.Minus(e, e')  -> IWB.minus  (eval state e) (eval state e')
		| AST.Times(e, e')  -> IWB.times  (eval state e) (eval state e')
		| AST.Divide(e, e') -> IWB.divide (eval state e) (eval state e')
end

module Transfer =
struct
	module V = Refract.FlowGraph.B
  type model = Refract.AST.model
	type vertex = Refract.FlowGraph.vertex
	type state  = Lattice.property
	type aux = int
	
	let preprocess _ = (0 : aux)
	
	let f _ _ (v : vertex) (p : state) = match v with
		| V.Skip _
		| V.Prob _ 				-> p
		| V.Guard(None, b) -> Constraints.solveAndPrint (Lattice.get p) b; p
		| V.Assign(x, e)  -> Lattice.set p x (Lattice.eval p e)
		| V.Guard(Some Refract.AST.Receive(c, x), _)
		| V.Receive(c, x) -> Lattice.set p x (Lattice.get p c)
		| V.Guard(Some Refract.AST.Send(c, e), _)
		| V.Send(c, e) 		-> Lattice.set p c (Lattice.eval p e)
end

module Instance = MonotoneFramework.Make(Lattice)(Refract.FlowGraph)(Transfer)
