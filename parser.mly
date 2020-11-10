%{
(* 補助的な変数、関数、型などの定義 *)
open Syntax

(* 目的：変数列と本体の式から、入れ子になった１引数関数を作る *)
(* create_fun : string list -> Syntax.t -> Syntax.t *)
(*let create_fun variables expr =
  List.fold_right (fun var expr -> Fun (var, expr)) variables expr*)

%}

/* 以降、どういうわけかコメントが C 式になることに注意 */
/* トークンの定義 */
%token <int> NUMBER
/* これは、数字には int 型の値が伴うことを示している */
%token PLUS MINUS TIMES
%token TRUE FALSE
%token EQUAL LESS MORE
%token LPAREN RPAREN
%token IF THEN ELSE
%token <string> VAR
/* これは、変数には string 型の値が伴うことを示している */
%token LET IN FUN ARROW
%token EOF
/* End of File: 入力の終わりを示す */

/* 非終端記号の型をここで宣言する */
%type <Syntax.t> start

/* 開始記号の定義 */
%start start

/* 演算子の優先順位を指定する */
/* 下に行くほど強く結合する */
%nonassoc IN
%nonassoc THEN
%nonassoc ELSE IF
%right EQUAL LESS MORE
%left PLUS MINUS
%left TIMES
%nonassoc UNARY
/* nonassoc は結合なし（毎回、かっこを書かなくてはならない）、
   left は左結合、right は右結合 */

/* 以下の %% は省略不可。それ以降に文法規則を書く */
%%

start:
| expr EOF
        { $1 }

simple_expr:
| NUMBER
        { Number ($1) }
| TRUE
        { Bool (true) }
| FALSE
        { Bool (false) }
| VAR
        { Var ($1) }
/* | VAR variables
        { $1 :: $2 }
| FUN variables ARROW expr
        { Fun ($2, $4) } */
| LPAREN expr RPAREN
        { $2 }

expr:
| simple_expr
        { $1 }
| expr PLUS expr
        { Op ($1, Plus, $3) }
| expr MINUS expr
        { Op ($1, Minus, $3) }
| expr TIMES expr
        { Op ($1, Times, $3) }
| expr EQUAL expr
        { Op ($1, Equal, $3) }
| expr LESS expr
        { Op ($1, Less, $3) }
| expr MORE expr
        { Op ($3, More, $1) }
| IF expr THEN expr ELSE expr
        { OpIf ($2, $4, $6) }
| LET VAR EQUAL expr IN expr
        { Let ($2, $4, $6) }
/* | LET VAR variables EQUAL expr IN expr
        { Let ($2, create_fun $3 $5, $7) } */
| MINUS expr %prec UNARY
        { Op (Number (0), Minus, $2) }
