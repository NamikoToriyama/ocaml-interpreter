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
![全体像](./全体像.png)
  語句の定義だけまとめておくと良さそう！