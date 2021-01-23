# 授業めも
## リンク
[計算モデル論教科書](http://pllab.is.ocha.ac.jp/~asai/class/model20/book.pdf)
[最低限のlex/yacc](http://pllab.is.ocha.ac.jp/~asai/class/automaton20/lex-yacc.html)

# 計算モデル論
BNF ... Backus-Naur Form

# 最低限のlex/ycc
## 字句解析と構文解析
入力文字列の解析 
- 字句解析
  - 入力の文字列を意味のある塊ごとに分解する
  - 「北に行く」を「北」「に」「行く」に分ける
- 構文解析
  - 得られた単語の列が指定された文法に従っているかをチェックする
  - ○「北に行く」×「行くに北」
  - 文法に従っていた時には**構文木**を返す
    - 構文木 ... 文法構造を木で示したもの

## lexとyacc
- lex
  - 字句解析を行うツール
  - lexical analysis
  - 字句の定義を受け取ったら字句解析を行うプログラムを返す
- yacc
  - 構文解析を行うツール
  - yet another compiler compiler
  - 文法規則を受け取ったら構文解析を行うプログラムを返す

## インタープリターの全体像
- lexer.mll 字句の定義
- parser.mly 文法の定義
- syntax.ml （抽象）構文木の定義
- eval.ml 入力された式の計算
- main.ml メインファイル

![全体像](./全体像.png)

### Syntax: 構文木の定義
- 糖衣構文（シンタクティックシュガー）
  - ユーザーの入力には許されているけれども、内部的には他の構文に変換されるような構文のこと
- 抽象構文木（Abstract Syntax Tree, AST）

```
(* ２項演算子の型 *)
type op_t = Minus | Times

(* Syntax.t: parser が出力する抽象構文木の型 *)
type t  = Num of int            (* 整数 *)
        | Op of t * op_t * t    (* ２項演算 *)
```

- 具象構文木
  - 入力文字列を構文解析するときに使う構文木

### Parser: 構文解析ファイル
大きく分けて三つの役割がある
- 補助的な変数や関数、型の定義
- 各種の定義と宣言
- 構文解析の規則（文法）

### 授業ノート
インタープリターメモ，締め切り2月頭くらい


../test-suite/check-model ./interpreter 1

———————————

型チェック

Option … ocamlの元々ある型、Noneかa型
gen_type() 何型でも大丈夫，新しい型変数を作る，自分の正体はまだわかっていない

Typing.ml

Γ … 環境
T … 式
Τ … 型

Ty1, ty2 … type1, type2
Unify ty1 Type.TInt … ty1はintじゃないとダメだよって言う制約をしている
Type.Tint で最終結果がintにあればいいよねって感じになってる

Fun文
xの型はなんでもいい，適当なτ（好きな型)
|- でこの環境下で新しい環境を設定する

unify関数
Ty1 = ty2となるように代入する
最後3つのところでエラーが出る

== ocamlではポインタが一致しているときにtrueが返る，ダメならパターンマッチの続きをする
Contents 書き換え可能なレコード，SomeはNoneじゃない何かの中身, Some(ty’)で多分中身を出している

occur関数
R1と言う型がty2の中に出てきてるか調べる，出てきている場合は同じにならない
主に変数用，ポインタが一緒ならエラーが出るのかな
※ Fun f -> f f とかでエラーを出すよう


仮想の型システムは使えない，授業のやつではint -> intで出てくる

—————————————

継続 … システムのstack
エラーをstackに積んでいく、CTSだとできる
PWL … papers we love
限定継続 … 授業ではshift/resetを使う

継続の型
Int -> int
(If 2=3 then “hello”  else “hi”) ^ “world”
Bool -> string

限定継続 reset関数で囲う
Shift 継続をとってくるfun k を定義するとkが変数
	まず継続をclearする
	kを変数とする
	executesでMを実行する

Resetとshiftを使うことでシステムのstackをなくすことができる
http://pllab.is.ocha.ac.jp/~asai/OchaCaml/
—————————————

ocamlc -i eval.ml 
型を返してくれる
Option + g + g

—————————————

継続渡し形式で書く -> 実行順序を規定する
型がaという形式で返せるので、stringとかintで返しても良い

足し算書き直しのイメージ
Func a -> (func b -> cons ( a + b))

ガンマ -> 環境
T >> C -> contのこと
最後まで得られた結果 a

—————————————

List.itrを使うとlistの実装は楽


例外、failwithはずるいらしい、
Errorを出す、返し続けるのではなくて、ぶった切る
Goのpanicみたいなやつか

(* 目的：リストの要素をすべて掛け合わせる *)
(* times : int list -> int *)
let rec times lst = match lst with
    [] -> 1
  | 0 :: rest -> 0
  | first :: rest -> first * times rest

(* テスト *)
let test1 = times [1; 2; 3; 4; 5] = 120 -> 普通に実行される
let test2 = times [1; 2; 0; 4; 5] = 0 -> 途中で0かけると0になる
→ 無駄に残りの計算を行うことになる

(* CPS 版 *)

(* times2 : int list -> (int -> int) -> int *)
let rec times2 lst cont = match lst with
    [] -> cont 1
  | 0 :: rest -> 0
  | first :: rest -> times2 rest (fun x -> cont (first * x))

(* 初期継続 *)
(* id : 'a -> 'a *)
let id x = x

(* テスト *)
let test1 = times2 [1; 2; 3; 4; 5] id = 120
let test2 = times2 [1; 2; 0; 4; 5] id = 0

int list -> (int -> α) -> int 
になれば答えがあってる

CPSのプログラムでは、
実行順序が一意に定まる必要がある


最低限のlex, yaccメモ
5.6.11 match 文 のところのmatchの式で
match <式> with [] -> <式> | 変数 :: 変数 -> <式> . + <式>
このようになっているのですが、3つ目の<式>の後にある「.」は意図的なものなのでしょうか？

5.6.13 reduce/reduce conflict 
上のふたつの conflict の状況を再現すると以下のようになります。
* ( <式> ) .
このような状態で conflict のところに書いてある 30 個のトークンのうちのひとつが次に来ると

この部分の、「.」も気になったのですが、( <式> ) だけならこの( <式> ) という部分を箇条書きにしなくてもいいような気がしました。

5.6.14 逐次実行文 
のところはたくさん「.」が残ってて気になりました！


ーーーーーーーーーーーーーーーーーーー

18:00 関数定義の授業開始

Let f x = func x -> … in

複数引数の場合
Let f x y = x + y in
Let f = fun x -> fun y -> x + y in

F 1 2
((Fun x -> fun y -> x + y) 1) 2

クロージャ
関数を返す、関数定義を閉じたもの
Let f x = x in f;;
‘A -> ‘a = <fun>

Clo<x, t, 
x … 変数
T … body
Γ … 環境　
