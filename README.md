## picross(nonogram) solver

ピクロスを単純に解けるところまで解くプログラムです。 

## How to run

#### scalacが入っている場合

```
$ scalac src/main/scala/com/github/nyango/solvepicross/picross.scala # この行は一度実行したらその後実行しなくて良いです。
$ scala com.github.nyango.solvepicross.SolvePicross
```

#### sbtが入っている場合

```
$ sbt run
```

## How it works

非確定マスを埋める・埋めないの2パターンでそれぞれ決め打ちし、
その中で条件を満たすものを抽出し、
どのような埋め方をしても埋め方が一意になるマスを確定させていきます。
それの繰り返しで埋められるところまで埋めます。
大概の問題には対応できると思います。


## Example / 入力・結果例

```
Input table height / 盤面の縦幅(nxmのn)を入力してください
11
Input table width / 盤面の横幅(nxmのm)を入力してください
8
Input numbers on the left (separated by space) / 左側に付与される数字をスペース区切りで各行入力してください
0
4
6
2 2
2 2
6
4
2
2
2
0
Input numbers on the top (separated by space) /上側に付与される数字をスペース区切りで各行入力してください
0
9
9
2 2
2 2
4
4
0
Executing... / 計算します

試行回数1回目	未確定マス数:17
□□□□□□□□
□■■◆◆□◆□
□■■■■■◆□
□■■◆◆■◆□
□■■◆◆■◆□
□■■■■■◆□
□■■◆◆□◆□
□■■□□□◆□
□■■□□□◆□
□■■□□□◆□
□□□□□□□□


試行回数2回目	未確定マス数:0
□□□□□□□□
□■■■■□□□
□■■■■■■□
□■■□□■■□
□■■□□■■□
□■■■■■■□
□■■■■□□□
□■■□□□□□
□■■□□□□□
□■■□□□□□
□□□□□□□□

```
