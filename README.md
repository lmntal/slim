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


///Tree Database軽量化ブランチ(by adachi-r)///
このブランチ(treecomp_32bit)は、TreeNodeElementを64ビットから32ビットに軽量化したブランチです。
