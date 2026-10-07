open Module_model
(** One authoritative encoding for module-owned declaration identities. Do not
    call this for main or an external C linker symbol. *)

(* Source identifiers cannot start with '_'. A count and lengths make the
   encoding reversible, distinct from _Z methods and existing closure names. *)
let prefix = "_BLM"

let encode ~owner ~name : (string, diagnostic) result =
  if owner = [] || List.exists (( = ) "") (name :: owner) then
    Error
      { loc = Util.Range.norange; message = "Empty module symbol component" }
  else
    let component value = string_of_int (String.length value) ^ value in
    Ok
      (prefix
      ^ string_of_int (List.length owner)
      ^ "_"
      ^ String.concat "" (List.map component owner)
      ^ "_" ^ component name)

(* Decode names embedded inside diagnostics/types without a global cache. *)
let decode_at text start =
  try
    let cursor = ref (start + String.length prefix) in
    let number () =
      let begin_at = !cursor in
      while
        !cursor < String.length text
        && text.[!cursor] >= '0'
        && text.[!cursor] <= '9'
      do
        incr cursor
      done;
      if begin_at = !cursor then raise Exit;
      int_of_string (String.sub text begin_at (!cursor - begin_at))
    in
    let separator () =
      if text.[!cursor] <> '_' then raise Exit;
      incr cursor
    in
    let component () =
      let length = number () in
      if length <= 0 then raise Exit;
      let value = String.sub text !cursor length in
      cursor := !cursor + length;
      value
    in
    let count = number () in
    if count <= 0 || count > String.length text then raise Exit;
    separator ();
    let owner = List.init count (fun _ -> component ()) in
    separator ();
    let name = component () in
    Some (String.concat "." (owner @ [ name ]), !cursor)
  with Exit | Invalid_argument _ | Failure _ -> None

let display_names text =
  let output = Buffer.create (String.length text) in
  let rec loop index =
    if index < String.length text then
      let decoded =
        if
          index + String.length prefix <= String.length text
          && String.sub text index (String.length prefix) = prefix
        then decode_at text index
        else None
      in
      match decoded with
      | Some (display, next) ->
          Buffer.add_string output display;
          loop next
      | None ->
          Buffer.add_char output text.[index];
          loop (index + 1)
  in
  loop 0;
  Buffer.contents output
