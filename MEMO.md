# MEMO

* α変換: 名前の付け替え
* β簡約: 代入
* η簡約: \x. P x => P
* δ簡約: primitive 演算の実行

* redex : 簡約可能
* Constant Applicative Form (CAF) : 定義の形式であり、左辺の定義に引数がなく右辺にもラムダがきてないもの
* Normal Form (NF) : それ以上簡約できない形
* Weak Head Normal Form (WHNF) : ラムダのボディ部が NF でないやつ
