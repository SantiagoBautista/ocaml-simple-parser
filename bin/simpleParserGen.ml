let authors = [
  "Timothée Haudebourg <timothee.haudebourg@irisa.fr>"
]

let version = "0.1"

let summary =
  "Parser generator for BNF-based LL1 grammars."

module Conf = struct
  type t = {
    verbosity: int;
    filename: string option;
  }

  let default = {
    verbosity = 0;
    filename = None;
  }

  let verbose t =
    { t with verbosity = t.verbosity+1 }

  let quiet t =
    { t with verbosity = t.verbosity-1 }

  let set_file filename t =
    { t with filename = Some filename }
end

let spec =
  let open Clap in
  app "Simple parser generator" "0.0" authors summary
  +> arg (short 'v') (Flag Conf.verbose) "Increase the level of verbosity."
  +> arg (short 'q') (Flag Conf.quiet) "Decrease the level of verbosity."
  +> anon "FILE" (String Conf.set_file) "Sets the input file(s) to process."

(** Make a sequence of char out of an input channel. *)
let seq_of_channel input =
  let rec next mem () =
    match !mem with
    | Some res -> res
    | None ->
      let res =
        try
          let c = input_char input in
          Seq.Cons (c, next (ref None))
        with
        | End_of_file -> Seq.Nil
      in
      mem := Some res;
      res
  in
  next (ref None)

let process_file filename =
  try
    let file = open_in filename in
    let input = seq_of_channel file in
    let utf8_input = Unicode.Encoding.utf8_decode input in
    let lexer = SimpleParser.Lexer.create utf8_input in
    let ast = SimpleParser.Parser.parse lexer in
    let grammar = SimpleParser.Grammar.of_ast ast in
    (* Seq.iter (function (token, _) -> Format.printf "%t@." (SimpleParser.Lexer.Token.print token)) lexer *)
    (* Format.printf "%t@." (SimpleParser.Ast.print ast) *)
    (* Format.printf "%t@." (SimpleParser.Grammar.print grammar); *)
    Format.printf "%t@." (SimpleParser.Generator.generate_parser grammar)
  with
  | SimpleParser.Generator.Error (e, _) ->
    Format.eprintf "generation error: %t@." (SimpleParser.Generator.print_error e)

let _ =
  (* let opt = Format.get_formatter_out_functions () in
  Format.set_formatter_out_functions { opt with out_indent = function i -> }; *)
  (* read options. *)
  let conf = Clap.parse spec Conf.default in
  begin match conf.filename with
    | Some filename ->
      process_file filename
    | None -> failwith "missing file"
  end
