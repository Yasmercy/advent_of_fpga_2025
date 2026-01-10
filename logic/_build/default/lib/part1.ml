open Core
open Hardcaml
open! Hardcaml_waveterm
open Signal

module Mod100 = struct
  let width = 64

  (* let create (inputs : Signal.t) =
    inputs *)

  let create (inputs : Signal.t) =
    let z = Signal.zero width in
    let hundred = Signal.of_int ~width 100 in

    let res0 = mux2 (inputs >+ hundred) (inputs -:. 100) inputs in
    let res1 = mux2 (res0 >+ hundred) (res0 -:. 100) res0 in
    let res2 = mux2 (res1 >+ hundred) (res1 -:. 100) res1 in
    let res3 = mux2 (res2 >+ hundred) (res2 -:. 100) res2 in
    let res4 = mux2 (res3 >+ hundred) (res3 -:. 100) res3 in
    let res5 = mux2 (res4 >+ hundred) (res4 -:. 100) res4 in
    let res6 = mux2 (res5 >+ hundred) (res5 -:. 100) res5 in
    let res7 = mux2 (res6 >+ hundred) (res6 -:. 100) res6 in
    let res8 = mux2 (res7 >+ hundred) (res7 -:. 100) res7 in
    let res9 = mux2 (res8 >+ hundred) (res8 -:. 100) res8 in

    let res0 = mux2 (res9 <+ z) (res9 +:. 100) res9 in
    let res1 = mux2 (res0 <+ z) (res0 +:. 100) res0 in
    let res2 = mux2 (res1 <+ z) (res1 +:. 100) res1 in
    let res3 = mux2 (res2 <+ z) (res2 +:. 100) res2 in
    let res4 = mux2 (res3 <+ z) (res3 +:. 100) res3 in
    let res5 = mux2 (res4 <+ z) (res4 +:. 100) res4 in
    let res6 = mux2 (res5 <+ z) (res5 +:. 100) res5 in
    let res7 = mux2 (res6 <+ z) (res6 +:. 100) res6 in
    let res8 = mux2 (res7 <+ z) (res7 +:. 100) res7 in
    let res9 = mux2 (res8 <+ z) (res8 +:. 100) res8 in

    res9
end

module Solution = struct
  let width = 64

  module I = struct
    type 'a t = {
      clk : 'a;
      clr : 'a;
      sign : 'a; [@bits 1]
      magnitude : 'a; [@bits width]
    }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { counter : 'a; [@bits width] position : 'a [@bits width] }
    [@@deriving sexp_of, hardcaml]
  end

  let create (inputs : _ I.t) =
    let spec = Reg_spec.create ~clock:inputs.clk ~clear:inputs.clr () in
    let position =
      Signal.reg_fb spec ~enable:Signal.vdd ~width ~f:(fun d ->
          let res =
            mux2
              (inputs.sign ==: Signal.zero 1)
              (d -: inputs.magnitude) (d +: inputs.magnitude)
          in
          Mod100.create res)
    in
    let enb = position ==:. 50 in
    let counter = Signal.reg_fb spec ~enable:enb ~width ~f:(fun d -> d +:. 1) in
    { O.counter; O.position }
end

(*
let%expect_test "part1 sample" =
  let module Sim = Cyclesim.With_interface (Solution.I) (Solution.O) in
  let sim = Sim.create Solution.create in
  let waveform, sim = Waveform.create sim in

  let inputs = Cyclesim.inputs sim in
  inputs.clk := Bits.vdd;

  (* cycle 0 *)
  inputs.clr := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clr := Bits.gnd;

  (* cycle 1 *)
  inputs.magnitude := Bits.of_int ~width:8 68;
  inputs.sign := Bits.of_int ~width:1 0;
  Cyclesim.cycle sim;

  (* cycle 2 *)
  inputs.magnitude := Bits.of_int ~width:8 30;
  inputs.sign := Bits.of_int ~width:1 0;
  Cyclesim.cycle sim;

  (* cycle 3 *)
  inputs.magnitude := Bits.of_int ~width:8 48;
  inputs.sign := Bits.of_int ~width:1 1;
  Cyclesim.cycle sim;

  (* cycle 4 *)
  inputs.magnitude := Bits.of_int ~width:8 5;
  inputs.sign := Bits.of_int ~width:1 0;
  Cyclesim.cycle sim;

  (* cycle 5 *)
  inputs.magnitude := Bits.of_int ~width:8 60;
  inputs.sign := Bits.of_int ~width:1 1;
  Cyclesim.cycle sim;

  (* cycle 6 *)
  inputs.magnitude := Bits.of_int ~width:8 55;
  inputs.sign := Bits.of_int ~width:1 0;
  Cyclesim.cycle sim;

  (* cycle 7 *)
  inputs.magnitude := Bits.of_int ~width:8 1;
  inputs.sign := Bits.of_int ~width:1 0;
  Cyclesim.cycle sim;

  (* cycle 8 *)
  inputs.magnitude := Bits.of_int ~width:8 99;
  inputs.sign := Bits.of_int ~width:1 0;
  Cyclesim.cycle sim;

  (* cycle 9 *)
  inputs.magnitude := Bits.of_int ~width:8 14;
  inputs.sign := Bits.of_int ~width:1 1;
  Cyclesim.cycle sim;

  (* cycle 10 *)
  inputs.magnitude := Bits.of_int ~width:8 82;
  inputs.sign := Bits.of_int ~width:1 0;
  Cyclesim.cycle sim;

  (* cycle 11 *)
  inputs.clr := Bits.vdd;
  Cyclesim.cycle sim;

  Waveform.print ~display_width:120 waveform;
  [%expect
    {|
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────────────────────────┐
    │clk               ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌─│
    │                  ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘ │
    │clr               ││────────┐                                                                               ┌───────  │
    │                  ││        └───────────────────────────────────────────────────────────────────────────────┘         │
    │                  ││────────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────────────  │
    │magnitude         ││ 00     │44     │1E     │30     │05     │3C     │37     │01     │63     │0E     │52               │
    │                  ││────────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────────────  │
    │sign              ││                        ┌───────┐       ┌───────┐                       ┌───────┐                 │
    │                  ││────────────────────────┘       └───────┘       └───────────────────────┘       └───────────────  │
    │                  ││────────────────────────────────────────┬───────────────────────┬───────────────┬───────────────  │
    │counter           ││ 00                                     │01                     │02             │03               │
    │                  ││────────────────────────────────────────┴───────────────────────┴───────────────┴───────────────  │
    │                  ││────────────────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────  │
    │position          ││ 00             │20     │02     │32     │2D     │05     │32     │31     │32     │40     │52       │
    │                  ││────────────────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────  │
    │                  ││                                                                                                  │
    │                  ││                                                                                                  │
    │                  ││                                                                                                  │
    └──────────────────┘└──────────────────────────────────────────────────────────────────────────────────────────────────┘
    |}]
*)

let%expect_test "part1 input" =
  let testbench () =
    let module Sim = Cyclesim.With_interface (Solution.I) (Solution.O) in
    let sim = Sim.create Solution.create in

    let inputs = Cyclesim.inputs sim in
    inputs.clk := Bits.vdd;

    (* cycle 0 *)
    inputs.clr := Bits.vdd;
    Cyclesim.cycle sim;
    inputs.clr := Bits.gnd;

    let lines =
      In_channel.read_lines
        "/home/yasmercy/src/advent_of_code/hardcaml_2025/logic/part1.in"
    in

    List.iter lines ~f:(fun line ->
        let hd = String.prefix line 1 in
        let tl = String.drop_prefix line 1 in

        (inputs.sign :=
           match hd with
           | "L" -> Bits.gnd
           | "R" -> Bits.vdd
           | _ -> failwith "invalid input");
        inputs.magnitude := Bits.of_int ~width:64 (Int.of_string tl);

        (* let outputs = Cyclesim.outputs sim in *)
        (* let counter = Bits.to_int !(outputs.counter) in *)
        (* Printf.printf "%d\n" counter; *)
        Cyclesim.cycle sim);

    let outputs = Cyclesim.outputs sim in
    Printf.printf "%d\n" (Bits.to_int !(outputs.counter))
  in
  testbench ()
