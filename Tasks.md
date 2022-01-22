Tasks

- TiState を record にする
- どの Rule が適用されたか分かるようにしたい
- dump を使って退避しつつヒープを emptyStack 使って初期化している箇所は discard 後の pop 後の stack を使うようにする
- dump から heap に restore する際に部分式評価時の統計データも含めてマージ処理が必要
- δ簡約が余計にカウントされてしまうので doAdminPrim は上ではなく個別に必要なところで呼ぶように修正
- 引数順序を TiState -> TiState になるように変更する
