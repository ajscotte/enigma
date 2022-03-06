open OUnit2
open Enigma

(** [index_test name input expected_output] constructs an OUnit test
    named [name] that asserts the quality of [expected_output] with
    [index input]. *)
let index_test (name : string) (input : char) (expected_output : int) :
    test =
  name >:: fun _ ->
  (* the [printer] tells OUnit how to convert the output to a string *)
  assert_equal expected_output (index input) ~printer:string_of_int

(* You will find it helpful to write functions like [make_index_test]
   for each of the other functions you are testing. They will keep your
   lists of tests below very readable, and will also help you to avoid
   repeating code. You will also find it helpful to create [~printer]
   functions for the data types in use. *)

(** [map_r_l_test name input expected_output] constructs an OUnit test
    named [name] that asserts the quality of [expected_output] with
    [map_r_l input]. *)
let map_r_l_test
    (name : string)
    (wire : string)
    (top : char)
    (input_pos : int)
    (expected_output : int) : test =
  name >:: fun _ ->
  (* the [printer] tells OUnit how to convert the output to a string *)
  assert_equal expected_output
    (map_r_to_l wire top input_pos)
    ~printer:string_of_int

(** [pos_mod_test name input expected_output] constructs an OUnit test
    named [name] that asserts the quality of [expected_output] with
    [pos_mod input]. *)
let pos_mod_test
    (name : string)
    (number : int)
    (operator : int)
    (expected_output : int) : test =
  name >:: fun _ ->
  (* the [printer] tells OUnit how to convert the output to a string *)
  assert_equal expected_output
    (pos_mod number operator)
    ~printer:string_of_int

(** [map_l_r_test name input expected_output] constructs an OUnit test
    named [name] that asserts the quality of [expected_output] with
    [map_l_r input]. *)
let map_l_r_test
    (name : string)
    (wire : string)
    (top : char)
    (input_pos : int)
    (expected_output : int) : test =
  name >:: fun _ ->
  (* the [printer] tells OUnit how to convert the output to a string *)
  assert_equal expected_output
    (map_l_to_r wire top input_pos)
    ~printer:string_of_int

(** [map_refl_test name input expected_output] constructs an OUnit test
    named [name] that asserts the quality of [expected_output] with
    [map_refl input]. *)
let map_refl_test
    (name : string)
    (wire : string)
    (input_pos : int)
    (expected_output : int) : test =
  name >:: fun _ ->
  (* the [printer] tells OUnit how to convert the output to a string *)
  assert_equal expected_output
    (map_refl wire input_pos)
    ~printer:string_of_int

(** [map_plug_test name input expected_output] constructs an OUnit test
    named [name] that asserts the quality of [expected_output] with
    [map_plug input]. *)
let map_plug_test
    (name : string)
    (pair : (char * char) list)
    (character : char)
    (expected_output : char) : test =
  name >:: fun _ ->
  (* the [printer] tells OUnit how to convert the output to a string *)
  assert_equal expected_output (map_plug pair character)

(** [cipher_char_test name input expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output] with
    [cipher_char input]. *)
let cipher_char_test
    (name : string)
    (config : config)
    (character : char)
    (expected_output : char) : test =
  name >:: fun _ ->
  (* the [printer] tells OUnit how to convert the output to a string *)
  assert_equal expected_output (cipher_char config character)

(** [step_test name input expected_output] constructs an OUnit test
    named [name] that asserts the quality of [expected_output] with
    [step input]. *)
let step_test
    (name : string)
    (config : config)
    (expected_output : config) : test =
  name >:: fun _ ->
  (* the [printer] tells OUnit how to convert the output to a string *)
  assert_equal expected_output (step config)

(** [cipher_test name input expected_output] constructs an OUnit test
    named [name] that asserts the quality of [expected_output] with
    [step input]. *)
let cipher_test
    (name : string)
    (config : config)
    (message_enc : string)
    (expected_output : string) : test =
  name >:: fun _ ->
  (* the [printer] tells OUnit how to convert the output to a string *)
  assert_equal expected_output (cipher config message_enc)

(* Creates a rotor record given the inputs*)
let rotor_maker (wire : string) (turnover : char) : rotor =
  { wiring = wire; turnover }

(* Creates a oriented_rotor record given the inputs*)
let rec oriented_rotor_maker (rotor : rotor) (top_letter : char) :
    oriented_rotor =
  { rotor; top_letter }

(* Creates an oriented rotor list given the inputs*)
let rec rotors_list_maker rotor_lst (lst : oriented_rotor list) =
  match rotor_lst with
  | [] -> lst
  | (a, b, d) :: c ->
      rotors_list_maker c
        (oriented_rotor_maker (rotor_maker a d) b :: lst)

(* Creates a config record given the previous functions and inputs*)
let config_maker
    refl
    (rotor_lst : (string * char * char) list)
    plugboard
    lst =
  { refl; rotors = rotors_list_maker rotor_lst lst; plugboard }

(*index test casses*)
let index_tests =
  [
    index_test "index of A is 0" 'A' 0;
    index_test "index of B is 1" 'B' 1;
    index_test "index of Z is 25" 'Z' 25;
    index_test "index of G is 6" 'G' 6;
    index_test "index of O is 14" 'O' 14;
  ]

(*pos_mod test casses*)
let pos_mod_tests =
  [
    pos_mod_test " -51%20 to 9 " (-51) 20 9;
    pos_mod_test " -51%20 to 9 " (-50) 20 10;
    pos_mod_test " normal test " 11 5 1;
    pos_mod_test " zero test " 10 5 0;
    pos_mod_test "less than mod number" 10 20 10;
  ]

(*map_r_l test casses*)
let map_rl_tests =
  [
    map_r_l_test "map_r_l normal" "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 'A' 0 0;
    map_r_l_test "map_r_l rotor 1" "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 'B' 0 9;
    map_r_l_test "map_r_l rotor 2" "AJDKSIRUXBLHWTMCQGZNPYFVOE" 'Z' 9 24;
    map_r_l_test "map_r_l rotor 3 test" "BDFHJLCPRTXVZNYEIWGAKMUSQO" 'O'
      14 17;
    map_r_l_test "map_r_l different wire1" "BACDEFGHIJKLMNOPQRSTUVWXYZ "
      'G' 14 14;
  ]

(*map_l_r test casses*)
let map_lr_tests =
  [
    map_l_r_test "map_l_r simple test" "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 'A'
      0 0;
    map_l_r_test "map_l_r little offset rotor 1"
      "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 'B' 0 21;
    map_l_r_test "map_l_r larger offset rotor 1"
      "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 'F' 10 14;
    map_l_r_test "map_l_r rotor 2" "AJDKSIRUXBLHWTMCQGZNPYFVO" 'D' 10 16;
    map_l_r_test "map_l_r rotor 3" "BDFHJLCPRTXVZNYEIWGAKMUSQO" 'J' 10 0;
  ]

(*map_relf test casses*)
let map_refl_tests =
  [
    map_refl_test "Normal alphabete test" "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 0
      0;
    map_refl_test "reflector B" "YRUHQSLDPXNGOKMIEBFZCWVJAT" 0 24;
    map_refl_test "reflector B reflection" "YRUHQSLDPXNGOKMIEBFZCWVJAT"
      24 0;
    map_refl_test "middle reflector C" "FVPJIAOYEDRZXWGCTKUQSBNMHL" 10
      17;
    map_refl_test "middle reflector C reflection"
      "FVPJIAOYEDRZXWGCTKUQSBNMHL" 17 10;
  ]

(*map_plug test casses*)
let map_plug_tests =
  [
    map_plug_test "Simple test no plugboard" [] 'G' 'G';
    map_plug_test "Simple test" [ ('A', 'Z'); ('X', 'Y') ] 'A' 'Z';
    map_plug_test "Simple test oppos" [ ('A', 'Z'); ('X', 'Y') ] 'Z' 'A';
    map_plug_test "Every letter doubled"
      [
        ('A', 'Z');
        ('B', 'Y');
        ('C', 'X');
        ('D', 'W');
        ('E', 'V');
        ('F', 'U');
        ('G', 'T');
        ('H', 'R');
        ('I', 'S');
        ('J', 'Q');
        ('K', 'P');
        ('L', 'O');
        ('M', 'N');
      ]
      'M' 'N';
    map_plug_test "Every letter doubled reversed"
      [
        ('A', 'Z');
        ('B', 'Y');
        ('C', 'X');
        ('D', 'W');
        ('E', 'V');
        ('F', 'U');
        ('G', 'T');
        ('H', 'R');
        ('I', 'S');
        ('J', 'Q');
        ('K', 'P');
        ('L', 'O');
        ('M', 'N');
      ]
      'N' 'M';
  ]

(* Creating outlines for various configurations *)
let config_simple : config =
  config_maker "ABCDEFGHIJKLMNOPQRSTUVWXYZ" [] [] []

let config_1_rotor top turn =
  config_maker "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    [ ("ABCDEFGHIJKLMNOPQRSTUVWXYZ", top, turn) ]
    [] []

let config_all_rotors top1 top2 top3 turn1 turn2 turn3 plugs =
  config_maker "YRUHQSLDPXNGOKMIEBFZCWVJAT"
    [
      ("BDFHJLCPRTXVZNYEIWGAKMUSQO", top3, turn3);
      ("AJDKSIRUXBLHWTMCQGZNPYFVOE", top2, turn2);
      ("EKMFLGDQVZNTOWYHXUSPAIBRCJ", top1, top1);
    ]
    plugs []

let config_4_rotors =
  config_maker "FVPJIAOYEDRZXWGCTKUQSBNMHL"
    [
      ("ABCDEFGHIJKLMNOPQRSTUVWXYZ", 'A', 'A');
      ("BDFHJLCPRTXVZNYEIWGAKMUSQO", 'G', 'A');
      ("AJDKSIRUXBLHWTMCQGZNPYFVOE", 'S', 'A');
      ("EKMFLGDQVZNTOWYHXUSPAIBRCJ", 'W', 'A');
    ]
    [ ('A', 'Q'); ('V', 'Z'); ('T', 'R'); ('G', 'B') ]
    []

(*chipher_char test casses*)
let cipher_char_tests =
  [
    cipher_char_test "Simple test2" config_simple 'A' 'A';
    cipher_char_test "Simple test2" (config_1_rotor 'A' 'A') 'A' 'A';
    cipher_char_test "Simple test3"
      (config_all_rotors 'A' 'A' 'A' 'A' 'A' 'A' [])
      'G' 'P';
    cipher_char_test "with plugs"
      (config_all_rotors 'A' 'A' 'A' 'A' 'A' 'A'
         [ ('A', 'Q'); ('V', 'Z'); ('T', 'R') ])
      'A' 'Z';
    cipher_char_test "complex offsets" config_4_rotors 'T' 'G';
  ]

(* configuring the rotors for the step test*)
let step_test_1_init = config_1_rotor 'A' 'A'

let step_test_1_ans = config_1_rotor 'B' 'A'

let step_test_2_init = config_all_rotors 'K' 'D' 'O' 'V' 'E' 'Q' []

let step_test_2_ans = config_all_rotors 'K' 'D' 'P' 'V' 'E' 'Q' []

let step_test_3_init = config_all_rotors 'K' 'E' 'R' 'V' 'E' 'Q' []

let step_test_3_ans = config_all_rotors 'K' 'F' 'S' 'V' 'E' 'Q' []

let step_test_4_init = config_all_rotors 'V' 'E' 'Q' 'V' 'E' 'Q' []

let step_test_4_ans = config_all_rotors 'W' 'F' 'R' 'V' 'E' 'Q' []

let step_test_5_init = config_all_rotors 'A' 'A' 'A' 'A' 'A' 'A' []

let step_test_5_ans = config_all_rotors 'B' 'B' 'B' 'A' 'A' 'A' []

(*step test casses*)
let step_tests =
  [
    step_test "1 rotor" step_test_1_init step_test_1_ans;
    step_test "3 rotor 1 rotation" step_test_2_init step_test_2_ans;
    step_test "3 rotor 2 rotoation" step_test_3_init step_test_3_ans;
    step_test "3 rotor all rotate" step_test_4_init step_test_4_ans;
    step_test "3 rotor all rotate again" step_test_5_init
      step_test_5_ans;
  ]

(*configuration for cipher test*)
let initial_config =
  config_all_rotors 'F' 'U' 'N' 'Q' 'E' 'V' [ ('A', 'Z') ]

let hard_config =
  config_all_rotors 'V' 'E' 'Q' 'V' 'E' 'Q'
    [ ('A', 'H'); ('P', 'Y'); ('D', 'Z') ]

(*chipher test casses*)
let cipher_tests =
  [
    cipher_test "simple test" config_simple "HELLO WORLD" "HELLO WORLD";
    cipher_test "Three rotors word 1" initial_config "HUMMUS" "RMRXYO";
    cipher_test "Three rotors word 2" initial_config
      "HELLOMYBABYHELLOMYHONEY" "RDPUTCHAEFGKHGMCIQKDTGL";
    cipher_test "Three rotors word 4 " initial_config "YNGXQ" "OCAML";
    cipher_test "three rotors word 3" hard_config "HAPPYDAY" "LJAGSPGJ";
  ]

let tests =
  "test suite for A1"
  >::: List.flatten
         [
           index_tests;
           map_rl_tests;
           pos_mod_tests;
           map_lr_tests;
           map_refl_tests;
           map_plug_tests;
           cipher_char_tests;
           step_tests;
           cipher_tests;
         ]

let _ = run_test_tt_main tests
