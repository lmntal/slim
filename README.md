SLIM - slim LMNtal imprementation {#mainpage}
===============================

バグ報告は lmntal@ueda.info.waseda.ac.jp までお願いします。

最新版における新しい機能やその他の変更点については NEWS を参照してください。

インストール方法は INSTALL に記述されています。

---


### Getting started

The way to compile this package is:

```
export LMNTAL_HOME=/path/to/devel # set path of compiler
cd slim
./autogen.sh
./configure
make
```
or

```
tar xvzf slim-x.y.z.tar.gz
cd slim-x.y.z
./configure
make
```

Among generated files, src/slim is the LMNtal interpreter.
So you can run SLIM as follows:

```
lmntal --slimcode source.lmn > source.il
./slim source.il
```

You can see what options are available with SLIM as follows:

```
./slim --help
```

### Model Checking

```
lmntal --slimcode source.lmn > source.il
./slim --nd source.il # single core
./slim --nd --use-Ncore=12 source.il # multi-core
./slim --nd --use-Ncore=12 --delta-mem source.il # multi-core optimization
```

### Requirements
- automake 1.14.1
- autoconf 2.69
- g++
- flex 2.5.35
- re2c 1.0.3
- bison 3.0
- ruby 1.9.3p547
- libtool 2.2.6b
- cunit


///Tree Database描画ツール(by adachi-r)///

このブランチ(treecomp_calculation)はメモリ使用量を自作カウンタで出力し、また、Graphviz、DOT言語を用いてTree Databaseの論理構造を描画するツールを実装したブランチです。

使用方法：
このブランチを用いてプログラムを実行する際は、以下の手順で行ってください。

1. Tree Compressionを有効化し、非決定実行で行う(実行時コマンドに必ず--tree-compress=N(Nは自然数)、--ndを用いる)
2. このプログラムを実行するとtreedatabase.dotファイルがsrcディレクトリ内に生成されるため、

dot -T png treedatabase.dot -o (任意の名前).png

と打つとグラフ構造が描画された画像(上記の場合はpngファイル。png以外の画像ファイルでも可)が生成される。

なお、画像ファイルを生成するには、SLIMを実行するのに必要なソフトウェアに加え、Graphvizが必要となります。