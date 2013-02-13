open Utils
open Intervals

module BS  = BufferSizeAnalysis
module BS2 = BufferSizeAnalysis2
module IA2 = IntervalAnalysis2
module IWB = IntervalWithBottom

let parse filename = Parser.model Lexer.token (Lexing.from_channel (open_in filename))

let compile filename =
	let model = parse filename in
	let fg = Refract.FlowGraph.modelFlow model in
	let _ = Refract.FlowGraph.show fg in
	let final_labels = Refract.FlowGraph.modelFinals model |> List.map (fun x -> `Bullet x) in
	let bs_result = BS2.Instance.solve model in
	let acc_bs_result = final_labels |> List.map bs_result |> List.fold_left BS2.Lattice.lub BS2.Lattice.bottom in
	let ia_result = IA2.Instance.solve model in
	let acc_ia_result = final_labels |> List.map ia_result |> List.fold_left IA2.Lattice.lub IA2.Lattice.bottom in
	let cap = Interval.upper << IWB.extract << (acc_bs_result |> fst |> BS.Lattice.get) in
	let chan_range c = IWB.extract << (acc_ia_result |> snd |> fun kappah -> IA2.Lattice.Channels.get kappah c |> List.nth) in
	let var_range = IWB.extract << (acc_ia_result |> fst |> IA2.Lattice.Variables.get) in
	Prism.compileModel cap chan_range var_range model
;;

let input = if Array.length Sys.argv > 1 then Sys.argv.(1) else	"programs/myprogram.refract" in
print_string (compile input)