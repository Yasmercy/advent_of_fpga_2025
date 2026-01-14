open Core
open Hardcaml
open Signal
open Hardcaml_waveterm

let width = 18

let dist p1 p2 =
  let unpack p =
    let z = select p (width - 1) 0 in
    let y = select p ((2 * width) - 1) width in
    let x = select p ((3 * width) - 1) (2 * width) in
    (x, y, z)
  in
  let x1, y1, z1 = unpack p1 in
  let x2, y2, z2 = unpack p2 in
  let dx = sresize (x1 -: x2) (2 * width) in
  let dy = sresize (y1 -: y2) (2 * width) in
  let dz = sresize (z1 -: z2) (2 * width) in
  (dx *: dx) +: (dy *: dy) +: (dz *: dz)

module MinimumSpanningTree = struct
  let mem_width = 10
  (* let mem_depth = 1000 *)

  module I = struct
    type 'a t = {
      clk : 'a;
      clr : 'a;
      input_done : 'a;
      mem_rdata : 'a; [@bits 3 * width]
    }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = {
      mem1_waddr : 'a; [@bits mem_width]
      mem1_wdata : 'a; [@bits 3 * width]
      mem1_we : 'a;
      mem2_waddr : 'a; [@bits mem_width]
      mem2_wdata : 'a; [@bits 3 * width]
      mem2_we : 'a;
    }
    [@@deriving sexp_of, hardcaml]
  end

  let create (inputs : _ I.t) =
    let mem1_waddr = Signal.of_int ~width:mem_width 0 in
    let mem1_wdata = Signal.of_int ~width:(3 * width) 0 in
    let mem1_we = Signal.of_int ~width:1 0 in
    let mem2_waddr = Signal.of_int ~width:mem_width 0 in
    let mem2_wdata = Signal.of_int ~width:(3 * width) 0 in
    let mem2_we = Signal.of_int ~width:1 0 in
    {
      O.mem1_waddr;
      O.mem1_wdata;
      O.mem1_we;
      O.mem2_waddr;
      O.mem2_wdata;
      O.mem2_we;
    }
end

let%expect_test "l2 dist" =
  let make_point x y z =
    let p = x + Int.shift_left y width + Int.shift_left z (2 * width) in
    Signal.of_int ~width:(3 * width) p
  in
  let p1 = make_point 162 817 812 in
  let p2 = make_point 57 618 57 in
  let d = dist p1 p2 in
  Printf.printf "%d\n" (Signal.to_int d);
  [%expect {| 620651 |}]

let%expect_test "test" =
  let testbench () =
    let module Sim =
      Cyclesim.With_interface (MinimumSpanningTree.I) (MinimumSpanningTree.O)
    in
    let sim = Sim.create MinimumSpanningTree.create in

    let inputs = Cyclesim.inputs sim in
    inputs.clk := Bits.vdd;
    inputs.clr := Bits.vdd;
    inputs.input_done := Bits.vdd;
    inputs.mem_rdata := Bits.zero (3 * width)
    (* Printf.printf "%d\n" (Bits.to_int !(inputs.clk)) *)
  in
  testbench ()
