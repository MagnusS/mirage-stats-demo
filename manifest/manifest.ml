(*
 * Copyright (c) 2015 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(* Create an OPAM manifest for a Mirage Unikernel. *)

let (/) = Filename.concat

module String = struct

  include String

  let strip str =
    let p = ref 0 in
    let l = String.length str in
    let fn = function
      | ' ' | '\t' | '\r' | '\n' -> true
      | _ -> false in
    while !p < l && fn (String.unsafe_get str !p) do
      incr p;
    done;
    let p = !p in
    let l = ref (l - 1) in
    while !l >= p && fn (String.unsafe_get str !l) do
      decr l;
    done;
    String.sub str p (!l - p + 1)

  let cut_at s ~on:sep =
    try
      let i = String.index s sep in
      let name = String.sub s 0 i in
      let version = String.sub s (i+1) (String.length s - i - 1) in
      Some (name, version)
    with _ ->
      None

  let split s ~on:sep =
    let rec aux acc r =
      match cut_at r sep with
      | None       -> List.rev (r :: acc)
      | Some (h,t) -> aux (strip h :: acc) t in
    aux [] s

end

module File = struct

  let read file =
    if Sys.file_exists file then
      let ic = open_in_bin file in
      Some (input_line ic)
    else
      None

  let write ~file contents =
    let write () =
      let oc = open_out file in
      output_string oc contents;
      output_char oc '\n';
      close_out oc in
    match read file with
    | None        -> write ()
    | Some actual -> if actual <> contents then write ()

end

module Shell = struct

  let yellow fmt = Printf.sprintf ("\027[33m"^^fmt^^"\027[m")
  let yellow_s = yellow "%s"
  let red fmt = Printf.sprintf ("\027[31m"^^fmt^^"\027[m")
  let red_s = red "%s"

  let info fmt = Printf.printf (fmt ^^ "%!\n")
  let error fmt =
    Printf.kprintf (fun str ->
        Printf.eprintf "%s %s\n%!" (red_s "[ERROR] ") str;
        exit 1
      ) fmt

  let read_command fmt =
    let open Unix in
    Printf.ksprintf (fun cmd ->
        let () = info "%s %s" (yellow_s "=>") cmd in
        let ic, oc, ec = open_process_full cmd (environment ()) in
        let buf1 = Buffer.create 64
        and buf2 = Buffer.create 64 in
        (try while true do Buffer.add_channel buf1 ic 1 done with End_of_file -> ());
        (try while true do Buffer.add_channel buf2 ec 1 done with End_of_file -> ());
        match close_process_full (ic,oc,ec) with
        | WEXITED 0   -> Buffer.contents buf1
        | WSIGNALED n -> error "process killed by signal %d" n
        | WSTOPPED n  -> error "process stopped by signal %d" n
        | WEXITED r   -> error "command terminated with exit code %d\n\
                                stderr: %s" r (Buffer.contents buf2)
      ) fmt

end

module Git = struct

  let revision =
    Shell.read_command "git rev-parse HEAD"
    |> String.strip

  let origin =
    Shell.read_command "git config --get remote.origin.url"
    |> String.strip

end

module Opam = struct

  let last = function
    | [] -> ""
    | l  -> List.hd (List.rev l)

  let info pkg fmt =
    Printf.ksprintf (fun str ->
        Shell.read_command "opam info %s %s" pkg str
        |> String.split ~on:'\n'
        |> List.map String.strip
        |> List.filter ((<>)"")
        |> last (* Can be needed with pinned packages ...*)
      ) fmt

  let list pkg =
    Shell.read_command
      "opam list -s --required-by %s --rec --depopts --installed"
      pkg
    |> String.split ~on:' '
    |> List.map String.strip
    |> List.filter ((<>)"")

end

module Package = struct

  type t = {
    name: string;
    version: string;
    hash: string;
    archive: string;
  }

  let tt = "{ name: string; version: string; hash: string; archive: string }"

  let name pkg = Opam.info pkg "-f package"
  let version pkg = Opam.info pkg "-f version"
  let hash pkg = Opam.info pkg "-f upstream-checksum"
  let archive pkg = Opam.info pkg "-f upstream-url"

  let pp fmt t =
    Format.fprintf fmt "{ name=%S; version=%S; hash=%S; archive=%S }"
      t.name t.version t.hash t.archive

  let create pkg =
    let name = name pkg in
    let version = version pkg in
    let hash = hash pkg in
    let archive = archive pkg in
    { name; version; hash; archive } 

  let current () =
    { name = "unikernel"; (* FIXME: expose the unikernel name in the Mirage API *)
      version = "git";  (* FIXME: support non-git repo? *)
      hash = Git.revision;
      archive = Git.origin;
    }

  let of_config file =
    let t = Mirage.load file in
    let pkgs = Mirage.packages t in
    List.map create pkgs

  let pp_name fmt t =
    let s = Bytes.copy t.name in
    String.iteri (fun i -> function
        | '-' -> Bytes.set s i '_'
        | _   -> ()
      ) s;
    Format.pp_print_string fmt s

  module Set = struct
    include Set.Make(struct
        type s = t
        type t = s
        let compare x y = Pervasives.compare x.name y.name
      end)
    let of_list t = List.fold_left (fun t h -> add h t) empty t
  end

end

let () =
  let file =
    if Array.length Sys.argv = 1 then None
    else if Array.length Sys.argv = 2 then Some Sys.argv.(1)
    else Shell.error "usage: %s [config.ml]" Sys.argv.(0)
  in
  let current = Package.current () in
  let root_pkgs = Package.of_config file in
  let pkgs = 
    root_pkgs
    |> List.map (fun pkg -> pkg.Package.name) 
    |> String.concat ","
    |> Opam.list
    |> List.fold_left (fun acc name ->
        if Package.Set.exists (fun pkg -> pkg.Package.name = name) acc then
          acc
        else
          let pkg = Package.create name in
          Package.Set.add pkg acc
      ) (Package.Set.of_list root_pkgs)
    |> Package.Set.elements
  in
  let buf = Buffer.create 1024 in
  let fmt = Format.formatter_of_buffer buf in
  Format.fprintf fmt "type t = %s\n\n" Package.tt;
  Format.fprintf fmt "let current = %a\n\n" Package.pp current;
  List.iter (fun pkg ->
      Format.fprintf fmt "let %a = %a\n\n" Package.pp_name pkg Package.pp pkg
    ) pkgs;
  Format.fprintf fmt "let all = [\n\
                     \  current;\n";
  List.iter (fun pkg -> Format.fprintf fmt "  %a;\n" Package.pp_name pkg) pkgs;
  Format.fprintf fmt "]";
  File.write ~file:"opam_manifest.ml" (Buffer.contents buf)
