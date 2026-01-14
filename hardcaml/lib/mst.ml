open Core
open Hardcaml
open Signal
open Hardcaml_waveterm

module Distance = struct
  let width = 18

  module I = struct
    type 'a t = { p1 : 'a; [@bits 3 * width] p2 : 'a [@bits 3 * width] }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { dist : 'a [@bits 2 * width] }
  end

  let unpack p =
    let z = select p (width - 1) 0 in
    let y = select p ((2 * width) - 1) width in
    let x = select p ((3 * width) - 1) (2 * width) in
    (x, y, z)

  let create (inputs : _ I.t) =
    let x1, y1, z1 = unpack inputs.p1 in
    let x2, y2, z2 = unpack inputs.p2 in
    let dx = sresize (x1 -: x2) (2 * width) in
    let dy = sresize (y1 -: y2) (2 * width) in
    let dz = sresize (z1 -: z2) (2 * width) in
    let dist = (dx *: dx) +: (dy *: dy) +: (dz *: dz) in
    { O.dist }
end

module MinimumSpanningTree = struct
  let width = 18
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
    let mem1_waddr = Bits.of_int ~width:mem_width 0 in
    let mem1_wdata = Bits.of_int ~width:(3 * mem_width) 0 in
    let mem1_we = Bits.of_int ~width:(3 * mem_width) 0 in
    let mem2_waddr = Bits.of_int ~width:mem_width 0 in
    let mem2_wdata = Bits.of_int ~width:(3 * mem_width) 0 in
    let mem2_we = Bits.of_int ~width:(3 * mem_width) 0 in
    {
      O.mem1_waddr;
      O.mem1_wdata;
      O.mem1_we;
      O.mem2_waddr;
      O.mem2_wdata;
      O.mem2_we;
    }
end

let%expect_test "test" =
  let testbench () =
    let module Sim =
      Cyclesim.With_interface (MinimumSpanningTree.I) (MinimumSpanningTree.O)
    in
    let sim = Sim.create MinimumSpanningTree.create in

    let inputs = Cyclesim.inputs sim in
    inputs.clk := Bits.vdd;

    Printf.printf "%d\n" (Bits.to_int !(inputs.clk))
  in
  testbench ()
