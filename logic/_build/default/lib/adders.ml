open Hardcaml
open Hardcaml_waveterm
open Signal

module FullAdder = struct
  module I = struct
    type 'a t = { a : 'a; b : 'a; cin : 'a } [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { sum : 'a; cout : 'a } [@@deriving sexp_of, hardcaml]
  end

  let create (inputs : _ I.t) =
    let sum = inputs.a ^: inputs.b ^: inputs.cin in
    let cout = inputs.a &: inputs.b |: (inputs.cin &: inputs.a ^: inputs.b) in
    { O.sum; cout }
end

module Adder4Bit = struct
  let width = 4

  module I = struct
    type 'a t = { a : 'a; [@bits width] b : 'a [@bits width] }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { sum : 'a; [@bits width] carry : 'a }
    [@@deriving sexp_of, hardcaml]
  end

  let create (inputs : _ I.t) =
    let rec connect i cin sums =
      if i = width then { O.sum = concat_msb sums; carry = cin }
      else
        let fa =
          FullAdder.create
            { a = select inputs.a i i; b = select inputs.b i i; cin }
        in
        connect (i + 1) fa.cout (fa.sum :: sums)
    in
    connect 0 gnd []
end

(* let%expect_test "adder test" =
  let testbench () =
    (* simulation code goes here *)
    let module Sim = Cyclesim.With_interface (Adder4Bit.I) (Adder4Bit.O) in
    let sim = Sim.create Adder4Bit.create in

    let inputs = Cyclesim.inputs sim in
    let outputs = Cyclesim.outputs sim in

    (* Set input values *)
    inputs.a := Bits.of_int ~width:4 5;
    inputs.b := Bits.of_int ~width:4 3;

    (* Run one simulation cycle *)
    Cyclesim.cycle sim;

    (* Check outputs *)
    Printf.printf "Sum: %d, Carry: %d\n"
      (Bits.to_int !(outputs.sum))
      (Bits.to_int !(outputs.carry))
  in
  testbench ();
  [%expect {| Sum: 8, Carry: 0 |}] *)

let%expect_test "adder waveform" =
  let module Sim = Cyclesim.With_interface (Adder4Bit.I) (Adder4Bit.O) in
  let sim = Sim.create Adder4Bit.create in
  let waveform, sim = Waveform.create sim in

  let inputs = Cyclesim.inputs sim in

  for i = 0 to 3 do
    inputs.a := Bits.of_int ~width:4 (i + 1);
    inputs.b := Bits.of_int ~width:4 (i * 2);
    Cyclesim.cycle sim
  done;

  Waveform.print waveform;
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │               ││────────┬───────┬───────┬───────                   │
    │a              ││ 1      │2      │3      │4                         │
    │               ││────────┴───────┴───────┴───────                   │
    │               ││────────┬───────┬───────┬───────                   │
    │b              ││ 0      │2      │4      │6                         │
    │               ││────────┴───────┴───────┴───────                   │
    │carry          ││                                                   │
    │               ││────────────────────────────────                   │
    │               ││────────┬───────┬───────┬───────                   │
    │sum            ││ 1      │4      │7      │A                         │
    │               ││────────┴───────┴───────┴───────                   │
    │               ││                                                   │
    │               ││                                                   │
    │               ││                                                   │
    │               ││                                                   │
    │               ││                                                   │
    │               ││                                                   │
    │               ││                                                   │
    └───────────────┘└───────────────────────────────────────────────────┘
    |}]
