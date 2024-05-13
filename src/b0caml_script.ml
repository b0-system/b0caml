(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0caml programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B0_std
open B0_text

let pp_bold pp = Fmt.st' [`Bold] pp

(* Syntactic metadata *)

let pp_loc = Tloc.pp_ocaml

type smeta = Tloc.t
let smeta ~loc = loc
let loc m = m

let loc_err_fmt ffmt m fmt =
  ffmt ("@[<v>%a:@,@[%a: " ^^ fmt ^^ "@]@]")
    pp_loc (loc m) (Fmt.st [`Fg `Red; `Bold ]) "Error"

let loc_errf m fmt = loc_err_fmt Fmt.str m fmt
let loc_error m fmt = loc_err_fmt Fmt.error m fmt

(* Scripts *)

type t =
  { file : Fpath.t;
    shebang : (string * smeta) option;
    directories : (Fpath.t * smeta) list;
    mod_uses : (Fpath.t * smeta) list;
    ocaml_unit : string * smeta; }

let file s = s.file
let shebang s = s.shebang
let directories s = s.directories
let mod_uses s = s.mod_uses
let ocaml_unit s = s.ocaml_unit

let pp_dump ppf s =
  let pp_fst pp = Fmt.using fst pp in
  let pp_paths = Fmt.(list ~sep:sp (pp_fst Fpath.pp_quoted)) in
  Fmt.record
    [ Fmt.field "file" file Fpath.pp_quoted;
      Fmt.field "shebang" shebang (Fmt.option (pp_fst Fmt.string));
      Fmt.field "directories" directories pp_paths;
      Fmt.field "mod-uses" mod_uses pp_paths;
      Fmt.field "ocaml_unit" ocaml_unit (Fmt.box @@ pp_fst Fmt.lines)]
    ppf s

let pp_locs ppf s =
  let pp_loc ppf (_, smeta) = Fmt.pf ppf "%a:" pp_loc (loc smeta) in
  let pp_list ppf = function
  | [] -> () | l -> Fmt.list pp_loc ppf l; Fmt.cut ppf ()
  in
  Fmt.pf ppf "@[<v>%a%a%a%a@]"
    (Fmt.option Fmt.(pp_loc ++ cut)) (shebang s)
    pp_list (directories s) pp_list (mod_uses s) pp_loc (ocaml_unit s)

(* Parsing *)

let directives = ["#directory"; "#mod_use"]
let pp_directive = pp_bold Fmt.string

let is_dir_letter c =
  0x61 <= c && c <= 0x7A || c = 0x5F || 0x41 <= c && c <= 0x5A

let err_eoi msg d ~sbyte ~sline =
  Tdec.err_to_here d ~sbyte ~sline "Unexpected end of input: %s" msg

let err_eoi_string = err_eoi "unclosed string"
let err_eoi_esc = err_eoi "truncated escape"
let err_exp_dir_arg d = Tdec.err_here d "Expected directive argument"
let err_illegal_uchar d b = Tdec.err_here d "Illegal character U+%04X" b

let curr_char d = (* TODO better escaping (this is for error reports) *)
  Tdec.tok_reset d; Tdec.tok_accept_uchar d; Tdec.tok_pop d

let err_esc_illegal d ~sbyte ~sline pre =
  Tdec.err_to_here d ~sbyte ~sline "%s%s: illegal escape" pre (curr_char d)

let err_unsupported_directive dir_loc dir =
  let hint = Fmt.must_be in
  let unknown = Fmt.(unknown' ~kind:(any "directives") pp_directive ~hint) in
  Tdec.err dir_loc (Fmt.str "@[%a@]" unknown ("#" ^ dir, directives))

let dec_byte d = match Tdec.byte d with
| c when 0x00 <= c && c <= 0x08 || 0x0E <= c && c <= 0x1F || c = 0x7F ->
    err_illegal_uchar d c
| c -> c
[@@ ocaml.inline]

let rec skip_ws d = match dec_byte d with
| 0x20 | 0x09 | 0x0A | 0x0B | 0x0C | 0x0D -> Tdec.accept_byte d; skip_ws d
| _ -> ()

let rec parse_directive_name d ~sbyte ~sline = match dec_byte d with
| c when is_dir_letter c ->
    Tdec.tok_accept_byte d; parse_directive_name d ~sbyte ~sline
| c ->
    let ebyte = Tdec.pos d - 1 and eline = Tdec.line d in
    let loc = Tdec.loc d ~sbyte ~ebyte ~sline ~eline in
    Tdec.tok_pop d, loc

let parse_esc d =
  let sbyte = Tdec.pos d and sline = Tdec.line d in
  match (Tdec.accept_byte d; dec_byte d) with
  | 0x22 -> Tdec.accept_byte d; Tdec.tok_add_char d '"'
  | 0x5C -> Tdec.accept_byte d; Tdec.tok_add_char d '\\'
  | 0x0A | 0x0D -> (* continuation line *) skip_ws d
  | 0xFFFF -> err_eoi_esc d ~sbyte ~sline
  | _ -> err_esc_illegal d ~sbyte ~sline "\\"

let parse_fpath_arg dir_loc d = match (skip_ws d; dec_byte d) with
| 0x22 ->
    let rec loop d ~sbyte ~sline = match dec_byte d with
    | 0x22 ->
        let loc = Tdec.loc_to_here d ~sbyte ~sline in
        let fpath = match Fpath.of_string (Tdec.tok_pop d) with
        | Ok fpath -> fpath
        | Error e -> Tdec.err loc e
        in
        Tdec.accept_byte d; (fpath, smeta ~loc)
    | 0x5C -> parse_esc d; loop d ~sbyte ~sline
    | 0xFFFF -> err_eoi_string d ~sbyte ~sline
    | _ -> Tdec.tok_accept_byte d; loop d ~sbyte ~sline
    in
    let sbyte = Tdec.pos d and sline = Tdec.line d in
    Tdec.accept_byte d; loop ~sbyte ~sline d
| c -> err_exp_dir_arg d

let parse_directives d ~sbyte ~sline =
  let rec loop directories mod_uses d ~sbyte ~sline =
    let dep_dirs, dep_srcs = match parse_directive_name d ~sbyte ~sline with
    | "directory", dir_loc ->
        (parse_fpath_arg dir_loc d :: directories, mod_uses)
    | "mod_use", dir_loc ->
        (directories, parse_fpath_arg dir_loc d :: mod_uses)
    | dir, dir_loc ->
        err_unsupported_directive dir_loc dir
    in
    match (skip_ws d; dec_byte d) with
    | 0x23 (* # *) ->
        let sbyte = Tdec.pos d and sline = Tdec.line d in
        (Tdec.accept_byte d; loop dep_dirs dep_srcs d ~sbyte ~sline)
    | _ -> List.rev dep_dirs, List.rev dep_srcs
  in
  loop [] [] d ~sbyte ~sline

let rec parse_shebang d ~sbyte ~sline = match dec_byte d with
| 0x0A | 0x0D | 0xFFFF ->
    let ebyte = Tdec.pos d - 1 and eline = Tdec.line d in
    let smeta = smeta ~loc:(Tdec.loc d ~sbyte ~ebyte ~sline ~eline) in
    Some (Tdec.tok_pop d, smeta)
| b -> Tdec.tok_accept_byte d; parse_shebang d ~sbyte ~sline

let parse_preamble d =
  let ws_parse_directives d = match (skip_ws d; dec_byte d) with
  | 0x23 (* # *) ->
      let sbyte = Tdec.pos d and sline = Tdec.line d in
      (Tdec.accept_byte d; parse_directives d ~sbyte ~sline)
  | _ -> [], []
  in
  match dec_byte d with
  | 0x23 (* # *) ->
      let sbyte = Tdec.pos d and sline = Tdec.line d in
      begin match Tdec.accept_byte d; dec_byte d with
      | 0x21 (* ! *) ->
          let () = Tdec.accept_byte d in
          let sbyte = Tdec.pos d and sline = Tdec.line d in
          let shebang = parse_shebang d ~sbyte ~sline in
          let dirs = ws_parse_directives d in
          shebang, dirs
      | c -> None, parse_directives d ~sbyte ~sline
      end
  | c -> None, ws_parse_directives d

let of_string ~file src =
  try
    let d = Tdec.create ~file:(Fpath.to_string file) src in
    let shebang, (directories, mod_uses) = parse_preamble d in
    let rest = String.subrange ~first:(Tdec.pos d) src in
    let ocaml_unit = rest, smeta ~loc:(Tdec.loc_here d) in
    Ok { file; shebang; directories; mod_uses; ocaml_unit }
  with Tdec.Err (loc, e) -> loc_error loc "%a" (Fmt.vbox Fmt.lines) e

(* #directory resolution *)

type directory_resolution = Fpath.t * smeta
type directory_resolution_error = Fpath.t * smeta * [`Error of string | `Miss ]

let directory_resolution_dir (dir, _) = Fpath.add_dir_sep dir

let resolve_directory ~ocamlpath script_root ~errs ~dirs (d, m) =
  match B0caml_ocamlpath.classify_path d with
  | `Concrete dir ->
      let dir = Fpath.(script_root // dir) in
      begin match Os.Dir.exists dir with
      | Error e -> Error ((dir, m, `Error e) :: errs)
      | Ok true -> Ok ((dir, m) :: dirs)
      | Ok false -> Error ((dir, m, `Miss) :: errs)
      end
  | `Logical dir ->
      let ocamlpath = B0caml_ocamlpath.dirs ocamlpath in
      let ds = List.map (fun r -> Fpath.append r dir) ocamlpath in
      let rec loop has_err errs has_dir dirs = function
      | [] ->
          if has_err then Error errs else
          if has_dir then Ok dirs else
          Error ((d, m, `Miss) :: errs)
      | dir :: ds ->
          begin match Os.Dir.exists dir with
          | Error e -> loop true ((dir, m, `Error e) :: errs) has_dir dirs ds
          | Ok true -> loop has_err errs true ((dir, m) :: dirs) ds
          | Ok false -> loop has_err errs has_dir dirs ds
          end
      in
      loop false errs false dirs ds

let resolve_directories ~ocamlpath s =
  let rec loop ~ocamlpath script_root errs dirs = function
  | [] -> if errs = [] then Ok (List.rev dirs) else Error (List.rev errs)
  | d :: ds ->
      match resolve_directory ~ocamlpath script_root ~errs ~dirs d with
      | Ok dirs -> loop ~ocamlpath script_root errs dirs ds
      | Error errs -> loop ~ocamlpath script_root errs dirs ds
  in
  loop ~ocamlpath (Fpath.parent (file s)) [] [] (directories s)

(* #mod_use support *)

type mod_use_resolution = Fpath.t option * Fpath.t * smeta
type mod_use_resolution_error = Fpath.t * smeta * [ `Error of string | `Miss ]

let mod_use_resolution_files (intf, impl, _) = match intf with
| None -> [impl]  | Some intf -> [intf; impl]

let resolve_mod_use script_root (mod_use, m) =
  let impl_file = Fpath.(script_root // mod_use) in
  match Os.File.exists impl_file with
  | Error e -> Error (impl_file, m, `Error e)
  | Ok false -> Error (impl_file, m, `Miss)
  | Ok true  ->
      if not (Fpath.has_ext ".ml" impl_file) then Ok (None, impl_file, m) else
      let intf_file = Fpath.set_ext ".mli" impl_file in
      match Os.File.exists intf_file with
      | Error e -> Error (intf_file, m, `Error e)
      | Ok false -> Ok (None, impl_file, m)
      | Ok true -> Ok (Some intf_file, impl_file, m)

let resolve_mod_uses s =
  let rec loop script_root errs fs = function
  | [] -> if errs = [] then Ok (List.rev fs) else Error (List.rev errs)
  | d :: ds ->
      match resolve_mod_use script_root d with
      | Ok f -> loop script_root errs (f :: fs) ds
      | Error e -> loop script_root (e :: errs) fs ds
  in
  loop (Fpath.parent (file s)) [] [] (mod_uses s)

(* Script source *)

let src ~mod_use_resolutions s =
  let read f m = match Os.File.read f with
  | Ok s -> s | Error e -> failwith (loc_errf m "%s" e)
  in
  let add_file f s l = s :: "\"\n" :: Fpath.to_string f :: "#1 \"" :: l in
  let add_mod_impl f s l = "\nend\n" :: add_file f s (" = struct\n" :: l) in
  let add_mod_intf f s l = "\nend\n" :: add_file f s (" : sig\n" :: l) in
  let add_mod_name n acc = n :: "module " :: acc in
  let add_dep_src acc (intf_file, impl_file, meta) =
    let name = Fpath.basename impl_file in
    let name = B0_ocaml.Modname.mangle_filename name in
    let impl = read impl_file meta in
    match intf_file with
    | None -> add_mod_impl impl_file impl @@ add_mod_name name acc
    | Some intf_file ->
        let intf = read intf_file meta in
        add_mod_impl impl_file impl @@
        add_mod_intf intf_file intf @@
        add_mod_name name acc
  in
  let add_ocaml_unit s l =
    let src, meta = ocaml_unit s in
    let line = string_of_int (fst (Tloc.sline (loc meta))) in
    src :: "\"\n" :: Fpath.to_string (file s) :: " \"" :: line :: "#" :: l
  in
  try
    let deps = List.fold_left add_dep_src [] mod_use_resolutions in
    let src = add_ocaml_unit s deps in
    Ok (String.concat "" (List.rev src))
  with
  | Failure e -> Error e

(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0caml programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
