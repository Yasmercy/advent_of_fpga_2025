open Core
open Base
open Hardcaml
open Signal
open Hardcaml_circuits

module Part1 = struct
  let width = 18
  let lg_n = 10
  let nodes = 1000

  module I = struct
    type 'a t = {
      clk : 'a;
      clr : 'a;
      input_done : 'a;
      mem_rdata : 'a; [@bits 3 * width]
      mem_raddr : 'a; [@bits lg_n]
    }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { output_done : 'a; output_sol : 'a [@bits width] }
    [@@deriving sexp_of, hardcaml]
  end

  module Dist = struct
    module I = struct
      type 'a t = { clk : 'a; clr : 'a; start : 'a; v1 : 'a; v2 : 'a }
      [@@deriving hardcaml, sexp_of]
    end

    module O = struct
      type 'a t = { dist : 'a; finished : 'a } [@@deriving hardcaml, sexp_of]
    end

    let create ~width (inputs : _ I.t) =
      let spec = Reg_spec.create ~clock:inputs.clk ~clear:inputs.clr () in

      (* delay propagation counter *)
      let count =
        reg_fb spec ~width:3 ~f:(fun c ->
            mux2 inputs.start (Signal.zero 3) (c +:. 1))
      in

      let unpack v =
        ( select v (width - 1) 0,
          select v ((2 * width) - 1) width,
          select v ((3 * width) - 1) (2 * width) )
      in

      let v1x, v1y, v1z = unpack inputs.v1 in
      let v2x, v2y, v2z = unpack inputs.v2 in

      let cur_v1 = mux count [ v1x; v1y; v1z; v1z; v1z; v1z; v1z; v1z ] in
      let cur_v2 = mux count [ v2x; v2y; v2z; v2z; v1z; v1z; v1z; v1z ] in

      let diff = cur_v1 -: cur_v2 |> reg spec in
      let sq = diff *+ diff |> pipeline ~n:2 spec in
      let valid = count >=:. 4 in
      let acc =
        reg_fb spec ~width:(2 * width) ~f:(fun d ->
            mux2 valid (Signal.zero (2 * width)) (d +: sq))
      in

      (* cycles/dimension = 1(diff) + 2(sq) + 1(acc) = 4
         cycles/3 dimensions = cycles + (3-1) = 6 *)
      let is_done = count ==:. 6 in

      { O.dist = acc; finished = is_done }
  end

  (* the shortest edge with length > threshold *)
  let min_edge clk clr threshold positions =
    let spec = Reg_spec.create ~clock:clk ~clear:clr () in

    let incr = Signal.wire 1 in
    let start = Signal.wire 1 in

    let cur_idx = reg_fb ~enable:incr spec ~width:lg_n ~f:(fun x -> x +:. 1) in
    let cur_pos = mux cur_idx (Array.to_list positions) in

    let dists_to_cur =
      Array.init nodes ~f:(fun idx ->
          Dist.create ~width
            { clk; clr; start; v1 = positions.(idx); v2 = cur_pos })
    in

    incr <== dists_to_cur.(0).finished;
    start <== reg spec (clr |: incr);

    let dist_max = Signal.ones (2 * width) in
    let spec_max =
      Reg_spec.create ~clock:clk ~clear:clr ()
      |> Reg_spec.override ~reset_to:dist_max
    in

    let nearest =
      Array.init nodes ~f:(fun idx ->
          let best_dist = Signal.wire (2 * width) in
          let cur_dist = dists_to_cur.(idx).dist in

          let better = cur_dist <: best_dist &: cur_dist >: threshold in
          let enable = incr &: better in

          let dist = reg spec_max ~enable cur_dist in
          let idx = reg spec ~enable cur_idx in

          best_dist <== dist;
          (idx, dist))
    in

    let reduce = function
      | [ a; b ] ->
          let asrc, adst, adist = a in
          let bsrc, bdst, bdist = b in
          let pred = adist <: bdist in
          (mux2 pred asrc bsrc, mux2 pred adst bdst, mux2 pred adist bdist)
      | [ a ] -> a
      | _ -> failwith "arity issue"
    in

    let leaves =
      List.init nodes ~f:(fun i ->
          let src = Signal.of_int ~width:lg_n i in
          let dst, dist = nearest.(i) in
          (src, dst, dist))
    in

    Signal.tree ~arity:2 ~f:reduce leaves

  module MinEdge = struct
    module I = struct
      type 'a t = {
        clk : 'a;
        clr : 'a;
        start_all : 'a;
        threshold : 'a; [@bits 2 * width]
        positions : 'a array; [@length nodes]
      }
      [@@deriving hardcaml]
    end

    module O = struct
      type 'a t = {
        src : 'a; [@bits lg_n]
        dst : 'a; [@bits lg_n]
        dist : 'a; [@bits 2 * width]
        all_done : 'a;
      }
      [@@deriving hardcaml]
    end

    let create (inputs : _ I.t) =
      let spec = Reg_spec.create ~clock:inputs.clk ~clear:inputs.clr () in

      let enable = Signal.wire 1 in
      let start = Signal.wire 1 in

      let cur_idx = reg_fb ~enable spec ~width:lg_n ~f:(fun x -> x +:. 1) in
      let cur_pos = mux cur_idx (Array.to_list inputs.positions) in
      let last = cur_idx ==:. nodes - 1 in

      let dists_to_cur =
        Array.init nodes ~f:(fun idx ->
            Dist.create ~width
              {
                clk = inputs.clk;
                clr = inputs.clr;
                start;
                v1 = inputs.positions.(idx);
                v2 = cur_pos;
              })
      in

      enable <== dists_to_cur.(0).finished;
      start <== reg spec (inputs.start_all |: (enable &: ~:last));

      let dist_max = Signal.ones (2 * width) in
      let spec_max = Reg_spec.override spec ~reset_to:dist_max in

      let nearest =
        Array.init nodes ~f:(fun src ->
            let best_d = Signal.wire (2 * width) in
            let cur_d = dists_to_cur.(src).dist in

            let better = cur_d <: best_d &: (cur_d >: inputs.threshold) in
            let update_en = enable &: better in

            let dist = reg spec_max ~enable:update_en cur_d in
            let dst = reg spec ~enable:update_en cur_idx in
            let src = Signal.of_int ~width:lg_n src in

            best_d <== dist;
            (src, dst, dist))
      in

      let reduce = function
        | [ a; b ] ->
            let asrc, adst, adist = a in
            let bsrc, bdst, bdist = b in
            let a_is_closer = adist <: bdist in
            ( mux2 a_is_closer asrc bsrc,
              mux2 a_is_closer adst bdst,
              mux2 a_is_closer adist bdist )
        | [ a ] -> a
        | _ -> failwith "Tree arity error"
      in

      let leaves = Array.to_list nearest in
      let final_src, final_tgt, final_dist =
        Signal.tree ~arity:2 ~f:reduce leaves
      in

      let all_done = reg spec (last &: enable) in
      { O.src = final_src; dst = final_tgt; dist = final_dist; all_done }
  end

  let create (inputs : _ I.t) =
    let clk = inputs.clk in
    let clr = inputs.clr in
    let spec = Reg_spec.create ~clear:clr ~clock:clk () in

    let loading = ~:(inputs.input_done) in
    let positions =
      Array.init nodes ~f:(fun idx ->
          let enable = loading &: (inputs.mem_raddr ==:. idx) in
          Signal.reg spec ~enable inputs.mem_rdata)
    in

    let output_done = Signal.gnd in
    let output_sol = Signal.zero width in
    { O.output_done; O.output_sol }
end
