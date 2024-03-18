# SLIM - Slim LMNtal IMprementation

_Every dinosaur has a time when he or she is a small, tiny child..._

This is a runtime and a model checker for the
[LMNtal (pronounced "elemental")](https://www.ueda.info.waseda.ac.jp/lmntal) language.

## Getting started

### Requirements

1. automake 1.14.1
2. autoconf 2.69
3. g++
4. flex 2.5.35
5. re2c 1.0.3
6. bison 3.0
7. ruby 1.9.3p547
8. libtool 2.2.6b
9. cunit

### Installation

Build the package as follows:

```bash
export LMNTAL_HOME=/path/to/devel # set the path to the compiler
cd slim
cmake -DCMAKE_INSTALL_PREFIX=$(pwd) -DCMAKE_BUILD_TYPE=Release -B build -S .
make -j # `-j` for a parallel build
make -j install # set up libraries
```

or

```bash
tar xvzf slim-x.y.z.tar.gz
cd slim-x.y.z
cmake -DCMAKE_INSTALL_PREFIX=$(pwd) -DCMAKE_BUILD_TYPE=Release -B build -S .
make -j
make -j install
```

Among generated files, `bin/slim` is the LMNtal interpreter.
So you can run SLIM as follows:

```sh
lmntal --slimcode source.lmn > source.il # Compile the source.lmn file and generate a source.il
./bin/slim source.il # Execute the runtime with the source.il
```

You can see what options are available with SLIM as follows:

```
./bin/slim --help
```

### Model Checking

```sh
lmntal --slimcode source.lmn > source.il
./bin/slim --nd source.il # single core
./bin/slim --nd --use-Ncore=12 source.il # multi-core
./bin/slim --nd --use-Ncore=12 --delta-mem source.il # multi-core optimization
```

## Development

### Testing

To test the project, run the following:

```bash
make check
```

To add or modify a test, see [test](test).

### Formatting

We are currently **NOT** using any formatter
but we **strongly recommend** you to use [ClangFormat](https://clang.llvm.org/docs/ClangFormat.html)
if you are to add any additional lines.
Install ClangFormat and run the following:

```bash
clang-format -i filename.cpp
```

## For the further information ...

バグ報告は lmntal@ueda.info.waseda.ac.jp までお願いします。

最新版における新しい機能やその他の変更点については NEWS を参照してください。

インストール方法は INSTALL に記述されています。
