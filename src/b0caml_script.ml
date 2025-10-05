(*---------------------------------------------------------------------------
   Copyright (c) 2019 The b0caml programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open B0_text

let pp_bold pp = Fmt.st' [`Bold] pp

(* Syntactic metadata *)

let pp_loc = Textloc.pp

type smeta = Textloc.t
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

exception Error of Textloc.t * string

let err loc msg = raise_notrace (Error (loc, msg))
let err_span d ~start fmt =
  Format.kasprintf (err (Textdec.textloc_span d ~start)) fmt

let err_here d fmt = Format.kasprintf (err (Textdec.textloc d)) fmt

let directives = ["#directory"; "#mod_use"]
let pp_directive = pp_bold Fmt.string

let is_dir_letter c =
  0x61 <= c && c <= 0x7A || c = 0x5F || 0x41 <= c && c <= 0x5A

let err_eoi msg d ~start =
  err_span d ~start "Unexpected end of input: %s" msg

let err_eoi_string = err_eoi "unclosed string"
let err_eoi_esc = err_eoi "truncated escape"
let err_exp_dir_arg d = err_here d "Expected directive argument"
let err_illegal_uchar d b = err_here d "Illegal character U+%04X" b

let err_esc_illegal d ~start pre u =
  err_span d ~start "%s%a: illegal escape" pre Textdec.pp_decode u

let err_unsupported_directive dir_loc dir =
  let hint = Fmt.must_be in
  let unknown = Fmt.(unknown' ~kind:(any "directives") pp_directive ~hint) in
  err dir_loc (Fmt.str "@[%a@]" unknown ("#" ^ dir, directives))

let nextc d =
  Textdec.next d;
  if Textdec.is_error d then err_here d "UTF-8 decoding error"

let uchar = Uchar.unsafe_of_int

let rec skip_ws d = match Textdec.current d with
| 0x20 | 0x09 | 0x0A | 0x0B | 0x0C | 0x0D -> nextc d; skip_ws d
| _ -> ()

let rec parse_directive_name d ~start =
  match Textdec.current d with
  | c when is_dir_letter c ->
      Textdec.lexeme_add d (uchar c); nextc d; parse_directive_name d ~start
  | c ->
      let textloc = Textdec.textloc_span_to_prev_decode d ~start in
      Textdec.lexeme_pop d,
      textloc

let parse_esc d =
  let start = Textdec.pos d in
  match (nextc d; Textdec.current d) with
  | 0x22 -> Textdec.lexeme_add d (Uchar.of_char '"'); nextc d
  | 0x5C -> Textdec.lexeme_add d (Uchar.of_char '\\'); nextc d
  | 0x0A | 0x0D -> (* continuation line *) skip_ws d
  | 0x11_0001 -> err_eoi_esc d ~start
  | u -> err_esc_illegal d ~start "\\" u

let parse_fpath_arg dir_loc d = match (skip_ws d; Textdec.current d) with
| 0x22 ->
    let rec loop d ~start = match Textdec.current d with
    | 0x22 ->
        let loc = Textdec.textloc_span d ~start in
        let fpath = match Fpath.of_string (Textdec.lexeme_pop d) with
        | Ok fpath -> fpath
        | Error e -> err loc e
        in
        nextc d; (fpath, smeta ~loc)
    | 0x5C -> parse_esc d; loop d ~start
    | 0x11_0001 -> err_eoi_string d ~start
    | u -> Textdec.lexeme_add d (uchar u); nextc d; loop d ~start
    in
    let start = Textdec.pos d in
    nextc d; loop d ~start
| c -> err_exp_dir_arg d

let parse_directives d ~start =
  let rec loop directories mod_uses d ~start =
    let dep_dirs, dep_srcs = match parse_directive_name d ~start with
    | "directory", dir_loc ->
        (parse_fpath_arg dir_loc d :: directories, mod_uses)
    | "mod_use", dir_loc ->
        (directories, parse_fpath_arg dir_loc d :: mod_uses)
    | dir, dir_loc ->
        err_unsupported_directive dir_loc dir
    in
    match (skip_ws d; Textdec.current d) with
    | 0x23 (* # *) ->
        let start = Textdec.pos d in
        (nextc d; loop dep_dirs dep_srcs d ~start)
    | _ -> List.rev dep_dirs, List.rev dep_srcs
  in
  loop [] [] d ~start

let rec parse_shebang d ~start  =
  match Textdec.current d with
  | 0x0A | 0x0D | 0x11_0001 ->
      let loc = Textdec.textloc_span_to_prev_decode d ~start in
      let smeta = smeta ~loc in
      Some (Textdec.lexeme_pop d, smeta)
  | u -> Textdec.lexeme_add d (uchar u); nextc d; parse_shebang d ~start

let parse_preamble d =
  let ws_parse_directives d = match (skip_ws d; Textdec.current d) with
  | 0x23 (* # *) ->
      let start = Textdec.pos d in
      (nextc d; parse_directives d ~start)
  | _ -> [], []
  in
  match Textdec.current d with
  | 0x23 (* # *) ->
      let start = Textdec.pos d in
      begin match nextc d; Textdec.current d with
      | 0x21 (* ! *) ->
          let () = nextc d in
          let start = Textdec.pos d in
          let shebang = parse_shebang d ~start in
          let dirs = ws_parse_directives d in
          shebang, dirs
      | c -> None, parse_directives d ~start
      end
  | c -> None, ws_parse_directives d

let of_string ~file src =
  try
    let d = Textdec.make ~file:(Fpath.to_string file) src in
    let shebang, (directories, mod_uses) = (nextc d; parse_preamble d) in
    let rest = String.subrange ~first:(Textdec.first_byte_pos d) src in
    let ocaml_unit = rest, smeta ~loc:(Textdec.textloc d) in
    Ok { file; shebang; directories; mod_uses; ocaml_unit }
  with Error (loc, e) -> loc_error loc "%a" (Fmt.vbox Fmt.lines) e

(* #directory resolution *)

type directory_resolution = Fpath.t * smeta
type directory_resolution_error = Fpath.t * smeta * [`Error of string | `Miss ]

let directory_resolution_dir (dir, _) = Fpath.ensure_trailing_dir_sep dir

let resolve_directory ~ocamlpath script_root ~errs ~dirs (d, m) =
  match B0caml_ocamlpath.classify_path d with
  | `Concrete dir ->
      let dir = Fpath.(script_root // dir) in
      begin match Os.Dir.exists dir with
      | Error e -> Result.Error ((dir, m, `Error e) :: errs)
      | Ok true -> Ok ((dir, m) :: dirs)
      | Ok false -> Error ((dir, m, `Miss) :: errs)
      end
  | `Logical dir ->
      let ocamlpath = B0caml_ocamlpath.dirs ocamlpath in
      let ds = List.map (fun r -> Fpath.append r dir) ocamlpath in
      let rec loop has_err errs has_dir dirs = function
      | [] ->
          if has_err then Result.Error errs else
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
  | Error e -> Result.Error (impl_file, m, `Error e)
  | Ok false -> Error (impl_file, m, `Miss)
  | Ok true  ->
      if not (Fpath.has_ext ".ml" impl_file) then Ok (None, impl_file, m) else
      let intf_file = Fpath.with_ext ~multi:false ".mli" impl_file in
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
    let line = string_of_int (fst (B0_text.Textloc.first_line (loc meta))) in
    src :: "\"\n" :: Fpath.to_string (file s) :: " \"" :: line :: "#" :: l
  in
  try
    let deps = List.fold_left add_dep_src [] mod_use_resolutions in
    let src = add_ocaml_unit s deps in
    Ok (String.concat "" (List.rev src))
  with
  | Failure e -> Error e
