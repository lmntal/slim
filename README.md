SLIM - slim LMNtal imprementation {#mainpage}
===============================

バグ報告は lmntal@ueda.info.waseda.ac.jp までお願いします。

最新版における新しい機能やその他の変更点については NEWS を参照してください。

インストール方法は INSTALL に記述されています。

---


### Note

This slim version has extended ground, which is used to encode formal systems involving name binding.
The extended ground has attributes just like hlground.
The modification to the slim is as follows:
```
-task.c:    Ground related functions receive attributes.
-memebrane.h and memebrane.c: Ground related functions are defined and implemented. 
-visitlog.h: One processTbl function is added.
-lmntal.h: A global variable is defined to store atoms local to ground graph type. 
Therefore, ground is computed only once in "lmn_mm_is_ground".
-others: To load intermediate LMNtal code, in which ground has attributes, an argument is added to ground instructors.
```

### How to use

Currently, the LMNtal complier does not support ground with attributes.
Therefore, take the following steps to run LMNtal code in which ground has attributes.
```
-In LMNtal code, use hlground with attributes in the places where you want to use ground with attributes.
-Generate intermediate code.
-Replace "hlground"  with "ground" in the intermediate code. 
For exampel, replace "ishlground" with "isground".
-Execute the modified intermediate code.
```
Once the complier is modified, these steps are no longer needed.

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
- bison 2.3
- ruby 1.9.3p547
- libtool 2.2.6b
- cunit
