# MEMO

* α変換: 名前の付け替え
* β簡約: 代入
* η簡約: \x. P x => P
* δ簡約: primitive 演算の実行

* redex : 簡約可能
* Constant Applicative Form (CAF) : 定義の形式であり、左辺の定義に引数がなく右辺にもラムダがきてないもの
* Normal Form (NF) : それ以上簡約できない形
* Weak Head Normal Form (WHNF) : ラムダのボディ部が NF でないやつ


----
G-machine
                   ,--------
                  |     `,  `,
                  v      |   |
   SC ---> R ---> E ---> C   |
                  |          |
                  `-> D ---> A

* Supercombinator (as compileSC)
SC[d] : compileSC is the G-Machine code for the supercombinator definition d.

* Reduction? (as compileR)
R[e] p d : generate code which instanticates the expression e in environment p,
           for a supercombinator of arity d.

* to Code (as compileC)
C[e] p : generate code which constructs the graph of e in environment p.

* for strict (as compileE)
E[e] p : compile code that evaluates an expression e to WHNF in environment p.

* alternatives (as compileD)
D[alts] p : compile the code for the alternatives in a `case` expression.

* alternative (as compileA which is actually argument of above compileAlts)
A[alt] p : compile the code for an alternative in a `case` expression.
