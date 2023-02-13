# テストスクリプトについて

テストは TAP（Test Anything Protocol）[^tap-docs] を用いて行います．

## How to Run

Run `make check` at the root directory of the project.

## 概要

Automake の TAP を用いて，自動的にテストを行います．

[system_check](system_check) では，

- `<testname>.lmntest` ファイルは，LMNtal のプログラムだけでなく，
  実行結果の予想などを含みます．
  - そのままではコンパイルできません．
- `<testname>.lmntest` ファイルは，
  [system_check/Makefile.am](system_check/Makefile.am)

  ```make
  %.il: %.lmntest
    awk -f create_testdata.awk | \        ## Create a test program.
    $(LMNC) --stdin-lmn $(LMNCFLAGS) >$@  ## Compile with a compiler.
  ```

  によって，

  1. [create_testdata.awk](system_check/create_testdata.awk)
     スクリプトによって，
     テストが成功した場合に最終状態のプロセスに `ok` を含むような，
     LMNtal プログラムへと作り替えられます．

  2. プログラムに変更があった場合は，
     コンパイラによってこの LMNtal プログラムは il ファイルにコンパイルされます．

- [check.pl](system_check/check.pl) は，
  作成された il ファイルを slim に与えて実行し，
  結果に `ok` があるかどうかを検査します．

[statespace](statespace) では，

- 非決定実行したい LMNtal プログラムをおいておき，
  そのまま il ファイルにコンパイルします．
- [check.pl](statespace/check.pl) が，
  コンパイルされた il ファイルと，全状態数と，最終状態数を受け取り，状態空間をチェックします．

[library_check](library_check) では，

- `--use-builtin-rule` をつけて実行したい LMNtal プログラムをおいておき，
  単にそれがそのまま実行されます．
- 実行結果の検査などは現在行っていません．
  - TODO: `system_check` を参考に，実行結果の確認まで行うようにする．

## Advanced Usage

それぞれのディレクトリの中のテストのみ実行する．

- `library_check` などのディレクトリに入って，`make check-TESTS` を実行する．

## 新たなテストを作成する手順

[system_check](system_check) にテストを追加する場合について，解説します．

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

[system_check](system_check)

1. [append](system_check/testsuite/append)
   - Appending a list to a list.
2. [basic](system_check/testsuite/basic)
   - 簡単な LMNtal プログラム
3. [count](system_check/testsuite/count)
   - ???
4. [guard_float](system_check/testsuite/guard_float)
   - ???
5. [guard_ground](system_check/testsuite/guard_ground)
   - ???
6. [guard_ground_multi](system_check/testsuite/guard_ground_multi)
   - ???
7. [guard_int](system_check/testsuite/guard_int)
   - ???
8. [guard_string](system_check/testsuite/guard_string)
   - ???
9. [guard_unary](system_check/testsuite/guard_unary)
   - ???
10. [hyperlink](system_check/testsuite/hyperlink)
    - ???
11. [mem_name](system_check/testsuite/mem_name)
    - ???
12. [miscellaneous](system_check/testsuite/miscellaneous)
    - ???
13. [proccxt](system_check/testsuite/proccxt)
    - ???
14. [proccxt_free](system_check/testsuite/proccxt_free)
    - ???
15. [proxyatom](system_check/testsuite/proxyatom)
    - ???
16. [rulecxt](system_check/testsuite/rulecxt)
    - ???
17. [simpagation](system_check/testsuite/simpagation)
    - ???
18. [unification](system_check/testsuite/unification)
    - ???
19. [uniq](system_check/testsuite/uniq)
    - ???

[library_check](library_check)

1. [integer](library_check/testsuite/integer)
   - ???
2. [set](library_check/testsuite/set)
   - Test set module.
3. [statespace](library_check/testsuite/statespace)
   - ???

[statespace](statespace)

1. [advanced](statespace/testsuite/advanced)
   - ???
2. [basic](statespace/testsuite/basic)
   - ???
3. [hyperlink](statespace/testsuite/hyperlink)
   - Test hyperlinks

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
