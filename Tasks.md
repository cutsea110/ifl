Tasks

- [x] Mark6 の単体テストをもっとしっかり書く
- [x] examples/testProg28 しか動かなくなったので何とかする(テストが一気に無くなったのでテスト追加でも良い)
- [x] TiState を record にする
- [ ] どの Rule が適用されたか分かるようにしたい
- [ ] dump を使って退避しつつヒープを emptyStack 使って初期化している箇所は discard 後の pop 後の stack を使うようにする
- [ ] dump から heap に restore する際に部分式評価時の統計データも含めてマージ処理が必要
- [ ] step 実行の結果を見やすくする
- [x] δ簡約が余計にカウントされてしまうので doAdminPrim は上ではなく個別に必要なところで呼ぶように修正
- [x] 引数順序を TiState -> TiState になるように変更する
