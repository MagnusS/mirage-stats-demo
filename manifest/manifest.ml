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
  let revision = Shell.read_command "git rev-parse HEAD"
  let origin = Shell.read_command "git config --get remote.origin.url"
end

module Opam = struct

  let info pkg fmt =
    Printf.ksprintf (fun str ->
        Shell.read_command "opam info %s %s" pkg str
      ) fmt

  let list pkg =
    Shell.read_command
      "opam list -s --required-by %s --rec --depopts --installed"
      pkg
    |> Stringext.split ~on:'\n'
    |> List.map String.trim
    |> List.filter ((<>)"")

end

module Package = struct

  type t = {
    name: string;
    version: string;
    hash: string;
    archive: string;
  }

  let name pkg = Opam.info pkg "-f name"
  let version pkg = Opam.info pkg "-f version"
  let hash pkg = Opam.info pkg "-f upstream-checksum"
  let archive pkg = Opam.info pkg "-f upstream-url"

  let create pkg =
    { name = name pkg; version = version pkg;
      hash = hash pkg; archive = archive pkg; }

  let pp fmt t =
    Format.fprintf fmt "{ name=%S; version=%S; hash=%S; archive=%S }"
      t.name t.version t.hash t.archive

  let current () =
    { name = "current"; (* FIXME: expose the unikernel name in the Mirage API *)
      version = "git";  (* FIXME: support non-git repo? *)
      hash = Git.revision;
      archive = Git.origin;
    }

  let of_config file =
    let t = Mirage.load file in
    let pkgs = Mirage.packages t in
    List.map create pkgs

end

let () =
  let file =
    if Array.length Sys.argv = 1 then None
    else if Array.length Sys.argv = 2 then Some Sys.argv.(1)
    else Shell.error "usage: %s [config.ml]" Sys.argv.(0)
  in
  let current = Package.current () in
  let pkgs = Package.of_config file in
  let buf = Buffer.create 1024 in
  let fmt = Format.formatter_of_buffer buf in
  Format.fprintf fmt "let current = %a\n\n" Package.pp current;
  List.iter (fun pkg ->
      Format.fprintf fmt "let %s = %a\n\n" pkg.Package.name Package.pp pkg
    ) pkgs;
  Format.fprintf fmt "let t = [\n\
                     \  current;\n";
  List.iter (fun pkg -> Format.fprintf fmt "  %s;\n" pkg.Package.name) pkgs;
  Format.fprintf fmt "]\n";
  File.write ~file:"opam_manigest.ml" (Buffer.contents buf)
