(* answer type : 最終結果の型 *)
type a = Ok of a | Error of a

(* 初期継続 *)
(* id : 'a -> 'a *)
let id x = x

(* メイン関数 *)
let go () =
  let program = Parser.start Lexer.token (Lexing.from_channel stdin) in
  (* これで標準入力を字句解析して、構文解析した結果を program に入れ *)
  if Array.length Sys.argv = 1 then begin
    (* 引数がないなら（-no-message が指定されていないなら）*)
    print_string "Parsed : ";
    Syntax.print program;               (* 入力を表示する *)
    print_newline ();
    print_string "Result : "
  end;
  print_string (Value.to_string(Eval.f program Env.empty id) Env.empty);      (* 結果を表示する *)
  print_newline ()

(* スタートアップ *)
let _ = go ()
