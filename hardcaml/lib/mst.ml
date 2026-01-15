open Base
open Core
open Hardcaml
open Signal
open Hardcaml_waveterm
open Hardcaml_circuits

let width = 18

let distance p1 p2 =
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

let prng component seed =
  let lfsr state = Lfsr.create (module Signal) state in
  let s1 = lfsr component in
  let s2 = lfsr seed in
  Signal.to_bool (s1 ^: s2)

module MinimumSpanningTree = struct
  let lg_nodes = 10
  let nodes = 1000
  (* let mem_depth = 1000 *)

  module I = struct
    type 'a t = {
      clk : 'a;
      clr : 'a;
      input_done : 'a;
      mem_rdata : 'a; [@bits 3 * width]
      mem_raddr : 'a; [@bits lg_nodes]
    }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = {
      mem1_waddr : 'a; [@bits lg_nodes]
      mem1_wdata : 'a; [@bits 3 * width]
      mem1_we : 'a;
      mem2_waddr : 'a; [@bits lg_nodes]
      mem2_wdata : 'a; [@bits 3 * width]
      mem2_we : 'a;
    }
    [@@deriving sexp_of, hardcaml]
  end

  let create (inputs : _ I.t) =
    (* TODO: change these to reg_fb and have the proper control signals *)
    let clk = inputs.clk in
    let clr = inputs.clr in
    let spec = Reg_spec.create ~clear:clr ~clock:clk () in
    let seed = reg_fb spec ~enable:vdd ~width ~f:(fun x -> x +:. 1) in

    let start = inputs.input_done &: ~:clr in
    let c_spec = Reg_spec.create ~clear:~:start ~clock:clk () in
    let cycles = reg_fb c_spec ~enable:vdd ~width ~f:(fun x -> x +:. 1) in

    let stage0 = ~:start in
    let stage1 = cycles <=:. nodes in
    let stage2 = ~:stage1 &: (cycles <=:. nodes + lg_nodes) in
    let stage3 = ~:stage1 &: ~:stage2 in

    (* stage 0:  *)
    let positions =
      Array.init nodes ~f:(fun idx ->
          let enable = stage0 &: (inputs.mem_raddr ==:. idx) in
          Signal.reg spec ~enable inputs.mem_rdata)
    in

    (* stage 1: *)
    let current_idx = Signal.mux2 stage0 cycles (Signal.zero lg_nodes) in

    let component_wires = Array.init nodes ~f:(fun _ -> Signal.wire lg_nodes) in
    let components = Array.init nodes ~f:(fun idx -> component_wires.(idx)) in

    let dist_wires = Array.init nodes ~f:(fun _ -> Signal.wire (2 * width)) in
    let distances = Array.init nodes ~f:(fun idx -> dist_wires.(idx)) in

    let nearest =
      Array.init nodes ~f:(fun idx ->
          let cur_idx = Signal.to_int current_idx in
          let outgoing = components.(idx) <>: components.(cur_idx) in
          let dist = distance positions.(idx) positions.(cur_idx) in
          let shorter = dist <: distances.(idx) in
          let enable = stage1 &: outgoing &: shorter in

          let uninit = dist_wires.(idx) <>:. 0 in
          let new_dist = mux2 (uninit |: enable) dist_wires.(idx) dist in
          Signal.assign dist_wires.(idx) new_dist;

          Signal.reg spec ~enable current_idx)
    in

    (* stage 2: *)
    let reduced =
      let combine prev i j =
        let same_component = components.(i) ==: components.(j) in
        let smaller = distances.(j) <: distances.(i) in
        Signal.mux2 (smaller &: same_component) prev.(i) prev.(j)
      in
      let rec loop stage prev =
        if stage = lg_nodes then prev
        else
          let stride = 1 lsl stage in
          let next =
            Array.init nodes ~f:(fun i ->
                let j = (i + stride) % nodes in
                let combined = combine prev i j in
                Signal.reg spec combined)
          in
          loop (stage + 1) next
      in
      loop 0 nearest
    in

    (* stage 3: *)
    let rec contract idx =
      if idx = nodes then ()
      else
        let flip_me = prng components.(idx) seed in
        let flip_other = prng reduced.(idx) seed in

        if flip_me && not flip_other then
          Signal.assign component_wires.(idx) reduced.(idx);

        contract (idx + 1)
    in
    contract 0;

    (* TODO: input reading and output writing *)
    let mem1_waddr = Signal.of_int ~width:lg_nodes 0 in
    let mem1_wdata = Signal.of_int ~width:(3 * width) 0 in
    let mem1_we = Signal.of_int ~width:1 0 in
    let mem2_waddr = Signal.of_int ~width:lg_nodes 0 in
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
  let d = distance p1 p2 in
  Printf.printf "%d\n" (Signal.to_int d)

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
