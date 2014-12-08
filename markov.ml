open Core.Std
open Core.Core_list
open Str

(* Tiny utility function I wrote because it was really early in the morning. *)
let my_list_to_string lst =
    let list_with_spaces = intersperse lst " " in
    fold_left list_with_spaces ~init:"" ~f:(^)

(* Return a list of strings, each string being a line read from stdin. I don't think this is tail-recursive. *)
let rec get_lines str =
    let line = In_channel.input_line In_channel.stdin in
    match line with
    | None -> []
    | Some x -> x :: (get_lines str)

(* Given a list of strings and a hash, insert every two strings as a key of (string * string) and append every third string to a list as that key's value. *)
let rec get_pairs lst hash =
    match lst with
    | [] -> ()
    | _::[] -> ()
    | _::_::[] -> ()
    | first :: second :: next :: tl ->
        let key = (first, second) in
            Hashtbl.change hash key (function None -> Some [next] | Some x -> Some (List.append x [next]));
            get_pairs (second :: next :: tl) hash

(* Get a random element from a list. *)
let rand_element lst =
    match lst with
    | x ->
        let list_length = Core.Core_list.length x in
        let rand_num = Random.int list_length in
            Core.Core_list.nth x rand_num

(* Get a random key from a hash table. *)
let rand_key hash =
    let key_list = Hashtbl.keys hash in
    rand_element key_list

(* Given a (string * string) key, get a random word from that key's value list, or None if it doesn't exist. *)
let next_word key hash =
    let value = Hashtbl.find hash key in
    match value with
    | Some x -> rand_element x
    | _ -> None

(* Main Markov function. If count is 0, return the chain. Otherwise, using the current key, get the next word in the chain and the next key. *)
let rec markov key hash chain count =
    match count with
    | 0 -> chain
    | _ -> begin
                match key with
                | (first, second) -> 
                    let word = next_word key hash in
                    begin
                        match word with
                        | Some x -> 
                            let cont_chain = chain ^ x in
                            let next_key = (second, x) in
                            let count = count - 1 in
                            markov next_key hash (cont_chain ^ " ") count
                        | None -> chain
                    end
            end

(* This chunk feels hacky and janky, but I can't think of another way to do it. *)
let () =
    let () = Random.self_init () in
    let line_list = get_lines "" in
    let text = my_list_to_string line_list in
    let markov_table : (string * string, string list) Hashtbl.t = Hashtbl.Poly.create () in
    let string_list = Str.split (Str.regexp " ") text in
    let () = get_pairs string_list markov_table in
    let first_key = match rand_key markov_table with
    | Some x -> x
    | None -> ("", "") in
    let markov_chain = markov first_key markov_table "" 1000 in
    printf "%s\n" markov_chain