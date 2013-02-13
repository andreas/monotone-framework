open Utils

module Make2(B : sig type model type t val to_string : t -> string end) =
struct
	module V =
	struct
		type t = int
		let compare = compare
		let hash = Hashtbl.hash
		let equal = (=)
	end
	
	module E =
	struct
		type t = Ctrl | Comm
		let compare = compare
		let default = Ctrl
		
		let to_string = function
			| Ctrl -> "Ctrl"
			| Comm -> "Comm"
	end
	
	module G = Graph.Imperative.Digraph.ConcreteLabeled(V)(E)
	
  type model = B.model
	type vertex = B.t
	type edge_label = E.t
	type t = {
		blocks: (int, B.t) Hashtbl.t;
		flow: G.t;
		processes: (int, string) Hashtbl.t;
		mutable extremals: int list
	}
	
	let create() = {blocks= Hashtbl.create 10; flow= G.create(); processes= Hashtbl.create 10; extremals= []}
	let inflow {flow=g} l = G.pred g l
	let extremal t l = t.extremals <- l::t.extremals
	let is_extremal t l = List.mem l t.extremals
	let get t l = Hashtbl.find t.blocks l
	let add {blocks=b; processes=p} id (l, v) = Hashtbl.replace b l v; Hashtbl.replace p l id
	let connect {flow=g} ?(label = E.default) l l' = G.add_edge_e g (G.E.create l label l')
	
	module Display(X : sig val label_to_subgraph : int -> Graph.Graphviz.DotAttributes.subgraph val label_to_dot_label : int -> string end) = struct
    include G
		
    let vertex_name v = string_of_int v
    let graph_attributes _ = []
    let default_vertex_attributes _ = [`Shape(`Box); `Fontname("Courier")]
    let vertex_attributes l = [`Label(X.label_to_dot_label l)]
    let default_edge_attributes _ = []
    let edge_attributes e = []
    let get_subgraph l = Some(X.label_to_subgraph l)
  end

  let dot_output {blocks=b; flow=g; processes=p} f =
		let module Helper =
			struct
				let label_to_dot_label l = Printf.sprintf "[%s]^%d" (B.to_string(Hashtbl.find b l)) l
				let label_to_subgraph l =
					let pid = Hashtbl.find p l in
					{Graph.Graphviz.DotAttributes.sg_name=pid; sg_attributes=[`Label pid]}
			end
		in
		let module Dot_ = Graph.Graphviz.Dot(Display(Helper)) in
    let oc = open_out f in
    Dot_.output_graph oc g;
    close_out oc
	
  let display_with_gv g =
		let tmp_dot = Filename.temp_file "graph" ".dot" in
    dot_output g tmp_dot;
		let tmp_ps = Filename.temp_file "graph" ".ps" in
		ignore(Sys.command ("dot -Tps " ^ tmp_dot ^ " > " ^ tmp_ps ^ "; evince " ^ tmp_ps));
		Sys.remove tmp_dot
	
	let show = display_with_gv
end

module Make(B : sig type t end) =
struct
	type vertex = B.t
	type t = (int, B.t) Hashtbl.t * (int, int list) Hashtbl.t
	
	let create() = (Hashtbl.create 10, Hashtbl.create 10)
	let get (dict, _) l = Hashtbl.find dict l
	let inflow (_, adj) l = try Hashtbl.find adj l with Not_found -> []
	let is_extremal t l = l = 1
	let connect (dict, adj) (l, v) (l', v') =
		Hashtbl.replace dict l v;
		Hashtbl.replace dict l' v';
		Hashtbl.replace adj l' (l::(try Hashtbl.find adj l' with Not_found -> []))
	let show _ = ()
end

