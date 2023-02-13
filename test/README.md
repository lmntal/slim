# テストスクリプトについて

テストは TAP（Test Anything Protocol）[^tap-docs] を用いて行います．

## How to Run

Run `make check` at the root directory of the project.

## 概要

Automake の TAP を用いて，自動的にテストを行います．

- `<testname>.lmntest` ファイルは，LMNtal のプログラムだけでなく，
  実行結果の予想などを含みます．
  - そのままではコンパイルできません．
- `<testname>.lmntest` ファイルは，
  [create_testdata.awk](system_check/create_testdata.awk)
  スクリプトによって，
  テストが成功した場合に最終状態のプロセスに `ok` を含むような，
  LMNtal プログラムへと作り替えられます．
- プログラムに変更があった場合は，
  コンパイラによってこの LMNtal プログラムは il ファイルにコンパイルされます．
- [check.pl](system_check/check.pl) は，
  作成された il ファイルを slim に与えて実行し，
  結果に `ok` があるかどうかを検査します．

## Advanced Usage

それぞれのディレクトリの中のテストのみ実行する．

- `library_check` などのディレクトリに入って，`make check-TESTS` を実行する．

## 新たなテストを作成する手順

[system_check](system_check) にテストを追加する場合について，解説します．

- 他のテストも同様に出来るはず．

新たなテストが必要な場合は，

1. [system_check](system_check) ディレクトリ以下に，
   新しくディレクトリ `<dirname>` を作って，または既存のディレクトリの中に，
   1. `<testname>.lmntest` ファイル（テストスクリプト）を新たに作成してください．
      - E.g., [testsuite/basic/append1.lmntest](testsuite/basic/append1.lmntest)
        ```prolog
        append(c(1,c(2,c(3,n))),c(4,c(5,n)),result), ( append(X,Y,Z), n(X) :- Y=Z ), ( append(X,Y,Z), c(A,X1,X) :- c(A,Z1,Z), append(X1,Y,Z1) ).
        result(c(1,c(2,c(3,c(4,c(5,n))))))
        ok
        ```
      - 構成
        1. 1 行目にテストしたい LMNtal プログラム，
        2. 2 行目に LMNtal プログラムの予想される出力，
        3. 3 行目に `ok` または `ng` を記述してください．
           - `ok` ならば， 1 行目の結果が 2 行目と等しいとき，
           - `ng` ならば， 1 行目の結果が 2 行目と異なるときにテストに成功します．
        4. 4 行目以降には，LMNtal コメント以外は書かないでください．
   2. `check.sh` （テストを実行するプログラム）を新たに生成，
      または既存のファイルに変更を加えてください．
      - TAP に従って結果を出力するシェルスクリプトです．
      - E.g., [testsuite/basic/check.sh](testsuite/basic/check.sh)
        ```bash
        #!/bin/sh
        ./check.pl \
            /testsuite/basic/append1 \
            /testsuite/basic/append2 \
            /testsuite/basic/append3 \
            /testsuite/basic/append4
        ```
      - `check.sh` ファイルでは，
        [`./check.pl`](system_check/check.pl) を呼び出し，
        その引数として，
        テストしたい `<testname>.lmntest` ファイルのパスから `.lmntest` を除いたもの
        (e.g., `/testsuite/basic/append1`)
        を全て与えてください．
2. [system_check/Makefile.am](system_check/Makefile.am) の
   1. `TESTS` 変数に，
      追加した `check.sh` スクリプトへのパス
      を追加してください．
      - E.g., `TESTS = testsuite/<dirname>/check.sh`
   2. `check_DATA` 変数に，
      追加した `<testname>.lmntest` ファイルへのパス
      を追加してください．
      - E.g., `check_DATA = testsuite/<dirname>/<testname>.lmntest`

## Directory Structure

system_check

2. atom
   - 簡単な LMNtal プログラム
3. basic
   - Appending a list to a list.
4. count
5. guard_float
6. guard_ground
7. guard_ground_multi
8. guard_int
9. guard_string
10. guard_unary
11. hyperlink
12. mem_name
13. memo.md
14. miscellaneous
15. proccxt
16. proccxt_free
17. proxyatom
18. rulecxt
19. simpagation
20. unification
21. uniq

library_check

1. integer
2. set
3. statespace

statespace

1. advanced
2. basic
3. hyperlink

## 歴史的経緯

元々のテストスクリプトは LMNtal 処理系用のテストスクリプトを流用したもので，
以下のような欠点がありました．

- テストが走るたびに lmn ファイルをコンパイルする
- テストの結果如何にかかわらず PASS: 1 のみを表示する
- ~.log にテスト結果が出力される

１回のテストに５分程度かかるため，
自動テストの利点があまり発揮できていませんでした．

そこで，
Automake の TAP を用いてよりテストの利便性を高めました．

- コンパイルを毎回しなくとも良いように，
  テストケースごとにファイルに分け，
  make によって監視しています．

[^tap-docs]: https://www.gnu.org/software/automake/manual/html_node/Using-the-TAP-test-protocol.html
