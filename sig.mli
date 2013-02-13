module type FLOWGRAPH =
sig
  type model
	type vertex
	type edge_label
	type t
	
	val create : unit -> t
	val get : t -> int -> vertex
	val add : t -> string -> (int * vertex) -> unit
	val connect : t -> ?label:edge_label -> int -> int -> unit
	val inflow : t -> int -> int list
	val extremal : t -> int -> unit
	val is_extremal : t -> int -> bool
	val show : t -> unit
	val modelFlow : model -> t
end

module type TRANSFER =
sig
  type model
	type vertex
	type state
	type aux
	
	val initial_state : model -> state
	val bottom_state : model -> state
	val preprocess : model -> aux
	
	val combine : state -> state -> state
	val f : aux -> int -> vertex -> state -> state
end

module type LATTICE =
sig
	include Fix.PROPERTY
	
	val lub : property -> property -> property
	val to_string : property -> string
end
