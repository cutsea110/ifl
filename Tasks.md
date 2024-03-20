Tasks

- [ ] gc がまだバグっている(TIM Mark3) (*)
- [x] gc がバグっている(TIM Mark3)
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

(*)
以下で不具合再発。つまり tak 12 6 0 で GC が走った時におかしい。
step 254 で FramePtr #1 が #11 に移動したときに 3 スロットとも空になっている。

```
cabal run ifl -- -t 10 -c timark3 examples/testProg33.ifl
```
