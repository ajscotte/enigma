(************************************************************
   Copyright (C) 2021 Cornell University.
   Created by Michael Clarkson (mrc26@cornell.edu) and CS 3110 course staff.
   You may not redistribute this assignment, distribute any derivatives,
   or use it for commercial purposes.
 ************************************************************)

(* TODO: change the name and NetID below to your own, then delete this
   TODO comment. *)

(** CS 3110 Fall 2021 Assignment A1 Enigma

    @Alexander Scotte (ajs667) *)

(* TODO: complete the academic integrity statement below, then delete
   this TODO comment. *)

(************************************************************

  Academic Integrity Statement

  I, the person named in the author comment above, have fully reviewed
  the course academic integrity policies. I have adhered to those
  policies in solving the assignment.

  The policies do permit some limited collaboration among students
  currently enrolled in the course. If I did engage in such
  collaboration, here is the list of other students with whom I
  collaborated, and a brief summary of that collaboration:

  - none

  ************************************************************)

(** [index c] is the 0-based index of [c] in the alphabet. Requires: [c]
    is an uppercase letter in A..Z. *)
let index (c : char) : int = Char.code c - 65

(* finds the positive modulus *)
let rec pos_mod (number : int) (operator : int) =
  let result = number mod operator in
  if result >= 0 then result else pos_mod (number + operator) operator

(** [map_r_to_l wiring top_letter input_pos] is the left-hand output
    position at which current would appear when current enters at
    right-hand input position [input_pos] to a rotor whose wiring
    specification is given by [wiring]. The orientation of the rotor is
    given by [top_letter], which is the top letter appearing to the
    operator in the rotor's present orientation. Requires: [wiring] is a
    valid wiring specification, [top_letter] is in A..Z, and [input_pos]
    is in 0..25. *)
let map_r_to_l (wiring : string) (top_letter : char) (input_pos : int) :
    int =
  let top_index = index top_letter in
  let index_pos = top_index + input_pos in
  pos_mod
    (index (String.get wiring (pos_mod index_pos 26)) - top_index)
    26
(* finds a character given the index*)
let reverse_index (index : int) = Char.chr (index + 65)
(** [map_l_to_r] computes the same function as [map_r_to_l], except for
    current flowing left to right. *)
let map_l_to_r (wiring : string) (top_letter : char) (input_pos : int) :
    int =
    let top_index = index top_letter in
    let character = reverse_index (pos_mod ((input_pos + (top_index))) 26) in
    pos_mod (String.index wiring character - top_index) 26
(** [map_refl wiring input_pos] is the output position at which current
    would appear when current enters at input position [input_pos] to a
    reflector whose wiring specification is given by [wiring]. Requires:
    [wiring] is a valid reflector specification, and [input_pos] is in
    0..25. *)
let map_refl (wiring : string) (input_pos : int) : int =
  map_r_to_l wiring 'A' input_pos

(** [map_plug plugs c] is the letter to which [c] is transformed by the
    plugboard [plugs]. Requires: [plugs] is a valid plugboard, and [c]
    is in A..Z. *)
let rec map_plug (plugs : (char * char) list) (c : char) =
  match plugs with
    | [] -> c
    | (a, b) :: d -> if a == c then b else (if b == c then a else map_plug d c)


type rotor = {
  wiring : string;  (** A valid rotor wiring specification. *)
  turnover : char;
      (** The turnover of the rotor, which must be an uppercase letter.
          This field will not be used in the assignment until you
          implement stepping in the excellent scope. *)
}
(** [rotor] represents an Enigma rotor. *)

type oriented_rotor = {
  rotor : rotor;  (** The rotor. *)
  top_letter : char;  (** The top letter showing on the rotor. *)
}
(** [oriented_rotor] represents a rotor that is installed on the spindle
    hence has a top letter. *)

type config = {
  refl : string;  (** A valid reflector wiring specification. *)
  rotors : oriented_rotor list;
      (** The rotors as they are installed on the spindle from left to
          right. There may be any number of elements in this list: 0, 1,
          2, 3, 4, 5, etc. The order of elements in list represents the
          order in which the rotors are installed on the spindle, **from
          left to right**. So, the head of the list is the leftmost
          rotor on the spindle, and the last element of the list is the
          rightmost rotor on the spindle. *)
  plugboard : (char * char) list;
      (** A valid plugboard. The order of characters in the pairs does
          not matter, and the order of pairs in the list does not
          matter. *)
}
(** [config] represents the configuration of an Enigma machine. *)



let rec map_r_l_helper (lst) ( c : char ) : char =
   match lst with 
      | [] -> c
      | a :: b -> map_r_l_helper b (reverse_index( map_r_to_l a.rotor.wiring a.top_letter (index c)))

let rec map_l_r_helper lst ( c : char ) : char = 
  match lst with 
      | [] -> c
      | a :: b -> map_l_r_helper b (reverse_index( map_l_to_r a.rotor.wiring a.top_letter (index c)))

(** [cipher_char config c] is the letter to which the Enigma machine
    ciphers input [c] when it is in configuration [config]. Requires:
    [config] is a valid configuration, and [c] is in A..Z. *)
let cipher_char (config : config) (c : char) : char =
  let first_plug_letter =  map_plug config.plugboard c in
  let oriented_rotors = List.rev config.rotors in 
  let first_set_rotors = map_r_l_helper oriented_rotors first_plug_letter in
  let map = reverse_index (map_refl (config.refl) (index (first_set_rotors))) in
  let second_set_rotors = map_l_r_helper (config.rotors) (map) in 
  map_plug config.plugboard second_set_rotors

(** [step config] is the new configuration to which the Enigma machine
    transitions when it steps beginning in configuration [config].
    Requires: [config] is a valid configuration. *)
let step (config : config) : config =
  raise (Failure "Unimplemented: Enigma.step")

(** [cipher config s] is the string to which [s] enciphers when the
    Enigma machine begins in configuration [config]. Requires: [config]
    is a valid configuration, and [s] contains only uppercase letters. *)
let rec cipher (config : config) (s : string) : string =
  raise (Failure "Unimplemented: Enigma.cipher")

(* TODO: set the value below to the number of hours you spent working on
   this assignment, rounded to the nearest integer, then delete this
   TODO comment. *)

let hours_worked = 18
