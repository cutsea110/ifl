Tasks

- [ ] gc がバグっている (*)
- [x] gc のログが verbose off でも出てしまうのを直したい
- [x] Mark6 の単体テストをもっとしっかり書く
- [x] examples/testProg28 しか動かなくなったので何とかする(テストが一気に無くなったのでテスト追加でも良い)
- [x] TiState を record にする
- [ ] どの Rule が適用されたか分かるようにしたい
- [ ] dump を使って退避しつつヒープを emptyStack 使って初期化している箇所は discard 後の pop 後の stack を使うようにする
- [ ] dump から heap に restore する際に部分式評価時の統計データも含めてマージ処理が必要
- [ ] step 実行の結果を見やすくする
- [x] δ簡約が余計にカウントされてしまうので doAdminPrim は上ではなく個別に必要なところで呼ぶように修正
- [x] 引数順序を TiState -> TiState になるように変更する

(*): 以下で確認できる

```
tak x y z = if (x <= y)
            y
            (tak (tak (x-1) y z) (tak (y-1) z x) (tak (z-1) x y)) ;
main = tak 4 2 0
```
1. cabal run ifl -- -v -t 100 examples/testProg33.ifl    # これは問題ない 533 ステップで正常終了
2. cabal run ifl -- -v -t 10  examples/testProg33.ifl    # こっちは 193 ステップで終了してしまう

どうやら GC により 163 ステップ目で Frame Ptr の先の Arg 3 が回収されるがこれがのちに Enter tak で呼び戻され Arg 3 が null のまま来るのがまずそう
