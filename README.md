SLIM - slim LMNtal imprementation
===============================

バグ報告は lmntal@ueda.info.waseda.ac.jp までお願いします。

最新版における新しい機能やその他の変更点については NEWS を参照してください。

インストール方法は INSTALL に記述されています。

---

The way to compile this package is:

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