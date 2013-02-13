open Sig
open Utils

module ImperativeMap(K : sig type key end)
= struct
	type key = K.key
	type 'data t = (key, 'data) Hashtbl.t
	
	let create ()  = Hashtbl.create 10
	let clear ht   = Hashtbl.clear ht
	let add k v ht = Hashtbl.replace ht k v
	let find k ht  = Hashtbl.find ht k
	let iter f ht  = Hashtbl.iter f ht
end

module Make
	(L : LATTICE)
	(G : FLOWGRAPH)
	(F : TRANSFER with type vertex = G.vertex and type state = L.property and type model = G.model)
= struct
	type variable = [`Circ of int | `Bullet of int]
	module FIX = Fix.Make(ImperativeMap(struct type key = variable end))(L)
	
	let generate_equations model =
		let initial_state = F.initial_state model
		and bottom_state = F.bottom_state model
		and aux = F.preprocess model
		and fg  = G.modelFlow model in
		fun var state ->
			match var with
			| `Circ l ->
				G.inflow fg l
				|> List.map (fun l' -> state(`Bullet l'))
				|> List.fold_left L.lub (if G.is_extremal fg l then initial_state else bottom_state)
			| `Bullet l ->
				F.f aux l (G.get fg l) (state (`Circ l))
	
	let solve model =
		FIX.lfp (generate_equations model)
end
