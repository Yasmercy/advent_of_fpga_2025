open Base
open Core
open Hardcaml
open Signal
open Hardcaml_waveterm
open Hardcaml_circuits
open Hardcaml
open Hardcaml.Signal

module MinimumSpanningTree = struct
  let nodes = 1000
  let lg_nodes = 10
  let width = 18

  let dist spec p1 p2 =
    let unpack p =
      ( select p ((3 * width) - 1) (2 * width),
        select p ((2 * width) - 1) width,
        select p (width - 1) 0 )
    in
    let x1, y1, z1 = unpack p1 in
    let x2, y2, z2 = unpack p2 in
    let dx, dy, dz =
      (reg spec (x1 -: x2), reg spec (y1 -: y2), reg spec (z1 -: z2))
    in
    let sq x = reg spec (sresize (x *: x) (2 * width)) in
    let dx2, dy2, dz2 = (sq dx, sq dy, sq dz) in
    reg spec (dx2 +: dy2 +: dz2)

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
      finished : 'a;
    }
    [@@deriving sexp_of, hardcaml]
  end

  module State = struct
    type t = IDLE | LOAD | FIND_NEAREST | REDUCE | CONTRACT | WRITE_OUT | DONE
    [@@deriving enumerate, sexp_of, compare]
  end

  let is_star_center spec component_id seed =
    let lfsr_val state =
      Lfsr.create ~config:Lfsr.Config.Galois (module Signal) state
    in
    let state = reg spec ~enable:vdd (lfsr_val (component_id ^: seed)) in
    msb state

  let create (inputs : _ I.t) =
    let open Always in
    let spec = Reg_spec.create ~clear:inputs.clr ~clock:inputs.clk () in
    let sm = State_machine.create (module State) spec in

    let current_ids =
      Array.init nodes ~f:(fun _ -> Variable.reg spec ~width:lg_nodes)
    in
    let positions =
      Array.init nodes ~f:(fun i ->
          let load_enable = sm.is State.LOAD &: (inputs.mem_raddr ==:. i) in
          Variable.reg spec ~enable:load_enable ~width:(3 * width))
    in
    let vertex_min_dist =
      Array.init nodes ~f:(fun _ -> Variable.reg spec ~width:(2 * width))
    in
    let vertex_best_nb =
      Array.init nodes ~f:(fun _ -> Variable.reg spec ~width:lg_nodes)
    in
    let seed = reg_fb spec ~enable:vdd ~width:10 ~f:(fun c -> c +:. 1) in

    let cur_idx = Variable.reg spec ~width:lg_nodes in
    let target_pos =
      mux cur_idx.value
        (Array.to_list (Array.map positions ~f:(fun v -> v.value)))
    in
    let target_id =
      mux cur_idx.value
        (Array.to_list (Array.map current_ids ~f:(fun v -> v.value)))
    in

    let pipe_valid = pipeline spec ~n:3 (sm.is FIND_NEAREST) in
    let pipe_target_id = pipeline spec ~n:3 target_id in
    let pipe_cur_idx = pipeline spec ~n:3 cur_idx.value in

    Array.iteri positions ~f:(fun i pos ->
        let d = dist spec pos.value target_pos in
        let update =
          pipe_valid
          &: (current_ids.(i).value <>: pipe_target_id)
          &: (d <: vertex_min_dist.(i).value)
        in
        let _ =
          if_ update
            [ vertex_min_dist.(i) <-- d; vertex_best_nb.(i) <-- pipe_cur_idx ]
            []
        in
        ());

    let shuffle_stages_dist =
      Array.make_matrix ~dimx:(lg_nodes + 1) ~dimy:nodes (zero (2 * width))
    in
    let shuffle_stages_nb =
      Array.make_matrix ~dimx:(lg_nodes + 1) ~dimy:nodes (zero lg_nodes)
    in

    for i = 0 to nodes - 1 do
      shuffle_stages_dist.(0).(i) <- vertex_min_dist.(i).value;
      shuffle_stages_nb.(0).(i) <- vertex_best_nb.(i).value
    done;

    for k = 0 to lg_nodes - 1 do
      let stride = 1 lsl k in
      for i = 0 to nodes - 1 do
        let j = (i + stride) mod nodes in
        let neighbor_dist = shuffle_stages_dist.(k).(j) in
        let neighbor_nb = shuffle_stages_nb.(k).(j) in
        let neighbor_id = current_ids.(j).value in
        let update =
          sm.is REDUCE
          &: (current_ids.(i).value ==: neighbor_id)
          &: (neighbor_dist <: shuffle_stages_dist.(k).(i))
        in
        shuffle_stages_dist.(k + 1).(i) <-
          reg spec (mux2 update neighbor_dist shuffle_stages_dist.(k).(i));
        shuffle_stages_nb.(k + 1).(i) <-
          reg spec (mux2 update neighbor_nb shuffle_stages_nb.(k).(i))
      done
    done;

    let edge_valid =
      Array.init nodes ~f:(fun i ->
          let neighbor_id =
            mux vertex_best_nb.(i).value
              (Array.to_list (Array.map current_ids ~f:(fun v -> v.value)))
          in
          let my_h =
            msb
              (reg spec
                 (Lfsr.create
                    (module Signal)
                    ~config:Galois
                    (current_ids.(i).value ^: seed)))
          in
          let nb_h =
            msb
              (reg spec
                 (Lfsr.create
                    (module Signal)
                    ~config:Galois (neighbor_id ^: seed)))
          in
          let can_contract =
            my_h ==: gnd &: (nb_h ==: vdd)
            &: (vertex_min_dist.(i).value <>: ones (2 * width))
          in
          let _ =
            if_
              (sm.is CONTRACT &: can_contract)
              [ current_ids.(i) <-- neighbor_id ]
              []
          in
          current_ids.(i).value ==:. i &: can_contract)
    in

    let write_offsets =
      Prefix_sum.eval ~config:Prefix_sum.Config.Sklansky ~operator:( +: )
        (Array.to_list (Array.map edge_valid ~f:(fun v -> uresize v lg_nodes)))
    in

    let total_edges = Variable.reg spec ~width:lg_nodes in
    let drain_ptr = Variable.reg spec ~width:lg_nodes in

    let all_nodes_merged =
      Pipelined_tree_reduce.create ~f:( &: ) ~enable:vdd ~arity:2 spec
        (Array.to_list
           (Array.map current_ids ~f:(fun id ->
                id.value ==: current_ids.(0).value)))
    in

    compile
      [
        sm.switch
          [
            (IDLE, [ if_ inputs.input_done [ sm.set_next LOAD ] [] ]);
            ( LOAD,
              [
                (* registers are listening to inputs.mem_rdata distributedly *)
                if_ inputs.input_done
                  [ sm.set_next FIND_NEAREST; cur_idx <--. 0 ]
                  [];
              ] );
            ( FIND_NEAREST,
              [
                if_
                  (cur_idx.value ==:. nodes - 1)
                  [ sm.set_next REDUCE; cur_idx <--. 0 ]
                  [ cur_idx <-- cur_idx.value +:. 1 ];
              ] );
            ( REDUCE,
              [
                if_
                  (cur_idx.value ==:. lg_nodes)
                  [ sm.set_next CONTRACT; cur_idx <--. 0 ]
                  [ cur_idx <-- cur_idx.value +:. 1 ];
              ] );
            ( CONTRACT,
              [
                total_edges <-- List.last_exn write_offsets;
                sm.set_next WRITE_OUT;
                drain_ptr <--. 0;
              ] );
            ( WRITE_OUT,
              [
                if_
                  (drain_ptr.value ==: total_edges.value)
                  [
                    if_
                      (all_nodes_merged.valid &: all_nodes_merged.value)
                      [ sm.set_next DONE ]
                      [ sm.set_next FIND_NEAREST; cur_idx <--. 0 ];
                  ]
                  [ drain_ptr <-- drain_ptr.value +:. 1 ];
              ] );
            (DONE, []);
          ];
      ];

    let final_edges_src =
      Array.init nodes ~f:(fun i -> of_int ~width:lg_nodes i)
    in
    let final_edges_dst = shuffle_stages_nb.(lg_nodes) in

    {
      O.mem1_waddr = drain_ptr.value;
      O.mem1_we = sm.is WRITE_OUT &: (total_edges.value >: zero lg_nodes);
      O.mem1_wdata =
        mux drain_ptr.value
          (Array.to_list (Array.map final_edges_src ~f:(fun s -> s)));
      O.mem2_waddr = drain_ptr.value;
      O.mem2_we = sm.is WRITE_OUT &: (total_edges.value >: zero lg_nodes);
      O.mem2_wdata =
        mux drain_ptr.value
          (Array.to_list (Array.map final_edges_dst ~f:(fun d -> d)));
      O.finished = sm.is DONE;
    }
end

let%expect_test "MST simulation" =
  let module Sim =
    Cyclesim.With_interface (MinimumSpanningTree.I) (MinimumSpanningTree.O)
  in
  let sim = Sim.create MinimumSpanningTree.create in
  let inputs, outputs = (Cyclesim.inputs sim, Cyclesim.outputs sim) in

  let read_coords filename =
    In_channel.read_lines filename
    |> List.map ~f:(fun line ->
        let parts = String.split line ~on:',' |> List.map ~f:Int.of_string in
        match parts with
        | [ x; y; z ] ->
            let open Int64 in
            (of_int x lsl 36) + (of_int y lsl 18) + of_int z
        | _ -> failwith "Invalid format")
  in

  let coords = read_coords "sample.in" in

  Cyclesim.reset sim;
  inputs.clr := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clr := Bits.gnd;

  List.iteri coords ~f:(fun i coord ->
      inputs.mem_raddr := Bits.of_int ~width:10 i;
      inputs.mem_rdata := Bits.of_int64 ~width:54 coord;
      Cyclesim.cycle sim);
  inputs.input_done := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.input_done := Bits.gnd;

  let captured_edges = ref [] in

  let finished = Bits.to_bool !(outputs.finished) in
  while not finished do
    Cyclesim.cycle sim;

    if Bits.is_vdd !(outputs.mem1_we) then begin
      let src = Bits.to_int !(outputs.mem1_wdata) in
      let dst = Bits.to_int !(outputs.mem2_wdata) in
      captured_edges := (src, dst) :: !captured_edges
    end
  done;

  printf "MST Edges Found:\n";
  List.rev !captured_edges
  |> List.sort ~compare:[%compare: int * int]
  |> List.iter ~f:(fun (u, v) -> printf "  Edge: %d -> %d\n" u v)
