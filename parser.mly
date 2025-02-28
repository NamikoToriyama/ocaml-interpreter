%{
(* 補助的な変数、関数、型などの定義 *)
open Syntax

(* 目的：変数列と本体の式から、入れ子になった１引数関数を作る *)
(* create_fun : string list -> Syntax.t -> Syntax.t *)
let create_fun variables expr =
  List.fold_right (fun var expr -> Fun (var, expr)) variables expr

(* 目的：式の列から、Cons を使ったリストを作る *)
(* create_list : Syntax.t list -> Syntax.t *)
let create_list exprs =
  List.fold_right (fun expr lst -> Cons (expr, lst)) exprs Nil
%}

/* 以降、どういうわけかコメントが C 式になることに注意 */
/* トークンの定義 */
%token <int> NUMBER
/* これは、数字には int 型の値が伴うことを示している */
%token PLUS MINUS TIMES DIVIDE
%token TRUE FALSE
%token EQUAL LESS MORE
%token LPAREN RPAREN
%token IF THEN ELSE
%token <string> VAR
/* これは、変数には string 型の値が伴うことを示している */
%token LET REC IN FUN ARROW ERROR
%token LBRACKET RBRACKET MATCH WITH CONS BAR RAISE TRY
%token LPAREN RPAREN COMMA
%token SEMI
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
%nonassoc ELSE
%nonassoc ARROW
%nonassoc EQUAL LESS MORE
%right CONS
%left PLUS MINUS
%left TIMES DIVIDE
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
| LPAREN expr RPAREN
        { $2 }
| LBRACKET RBRACKET
        { Nil }
| LBRACKET expr_semi_list RBRACKET
        { create_list $2 }

expr:
| simple_expr
        { $1 }
| expr PLUS expr
        { Op ($1, Plus, $3) }
| expr MINUS expr
        { Op ($1, Minus, $3) }
| expr TIMES expr
        { Op ($1, Times, $3) }
| expr DIVIDE expr
        { Op ($1, Divide, $3) }
| expr EQUAL expr
        { Op ($1, Equal, $3) }
| expr LESS expr
        { Op ($1, Less, $3) }
| expr MORE expr
        { Op ($3, More, $1) }
| expr CONS expr
        { Cons ($1, $3) }
| IF expr THEN expr ELSE expr
        { OpIf ($2, $4, $6) }
| LET VAR EQUAL expr IN expr
        { Let ($2, $4, $6) }
| LET VAR variables EQUAL expr IN expr
        { Let ($2, create_fun $3 $5, $7) }
| LET REC VAR variables EQUAL expr IN expr
        { Letrec ($3, List.hd $4, create_fun (List.tl $4) $6, $8) }
| FUN variables ARROW expr
        { create_fun $2 $4 }
| app
        { $1 }
| MINUS expr %prec UNARY
        { Op (Number (0), Minus, $2) }
| MATCH expr WITH LBRACKET RBRACKET ARROW expr BAR VAR CONS VAR ARROW expr
        { Match ($2, $7, $9, $11, $13) }
| RAISE LPAREN ERROR expr RPAREN
        { Raise ($4)}
| TRY expr WITH ERROR VAR ARROW expr
        { Try ($2, $5, $7)}

variables:
| VAR
        { [$1] }
| VAR variables
        { $1 :: $2 }

app:
| simple_expr simple_expr
        { App ($1, $2) }
| app simple_expr
        { App ($1, $2) }

expr_semi_list:
| expr opt_semi
        { [$1] }
| expr SEMI expr_semi_list
        { $1 :: $3 }

opt_semi:
|
    { () }
| SEMI
    { () }
