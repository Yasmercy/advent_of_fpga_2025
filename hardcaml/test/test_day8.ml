open Core
open Hardcaml
open Day8

let read_points filename =
  In_channel.with_file filename ~f:(fun channel ->
      In_channel.input_lines channel
      |> List.map ~f:(fun line ->
          let parts = String.split line ~on:',' |> List.map ~f:String.strip in
          match parts with
          | [ x; y; z ] ->
              (* Convert x, y, z to bitstrings and concatenate them *)
              (* Assuming 18-bit widths as per your Part1 definition *)
              let to_int s = Int.of_string s in
              (to_int z lsl 36) lor (to_int y lsl 18) lor to_int x
          | _ -> failwith "Invalid CSV format"))

let part1 =
  let cwd = Stdlib.Sys.getcwd () in
  Stdio.print_endline cwd;

  let points = read_points "../../../../data/sample.in" in

  let module Sim = Cyclesim.With_interface (Part1.I) (Part1.O) in
  let sim = Sim.create Part1.create in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in

  let cycle () = Cyclesim.cycle sim in

  inputs.clr := Bits.vdd;
  cycle ();
  inputs.clr := Bits.gnd;

  inputs.input_done := Bits.gnd;
  List.iteri points ~f:(fun i point_data ->
      inputs.mem_raddr := Bits.of_int ~width:Part1.lg_n i;
      inputs.mem_rdata := Bits.of_int ~width:Part1.point_width point_data;
      cycle ());

  inputs.input_done := Bits.vdd;

  let max_cycles = 100_000 in
  let rec wait_loop n =
    if n > max_cycles then failwith "Simulation timed out!";
    if Bits.is_vdd !(outputs.output_done) then
      Stdio.printf "Solution: %s\n" (Bits.to_string !(outputs.output_sol))
    else (
      cycle ();
      wait_loop (n + 1))
  in
  wait_loop 0
