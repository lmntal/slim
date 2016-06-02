このファイルはSLIMの他言語インタフェース機能の利用方法について書かれて
いる。この機能を使うことで、LMNtalプログラムからCで実装した関数を呼び出
すことができ、LMNtalだけでは実現できない拡張機能をSLIMに追加することが
できる。

SLIMの多言語インタフェースを利用するには、Cのソースコードを決められた形
式で記述し、それをコンパイルして共有ライブラリ(Windowsでは動的ライブラ
リ)を作成する。そして、そのライブラリをSLIMが認識するライブラリを特定の
場所に置くことで、SLIMが実行時にライブラリを読み込み、Cで書かれた関数
を呼び出すことができる。

以下では、ライブラリの作成方法と、LMNtalからの利用方法について説明する。

** ライブラリの作成 

1. コールバック関数を用意する~
2. 初期化関数の作成~
3. コンパイルと配置~

SLIMの機能を利用するために、まずはSLIMの他言語インタフェース用のヘッダ
ファイル（lmntal_ext.h）をインクルードしておく。
 #include <lmntal_ext.h>

*** 1. コールバック関数を用意する

LMNtalプログラムの実行中に、SLIMから呼び出されるコールバック関数を記述
する。コールバック関数は必ずしも一つである必要はなく、一つのファイルで
いくつも作成することができる。

関数は以下の形式で記述する。
 void 関数名(ReactCxt rc,
             LmnMembrane mem,
             LmnAtom atom1, LmnLinkAttr attr1,
             LmnAtom atom2, LmnLinkAttr attr2,
                      ・・・
             LmnAtom atomN, LmnLinkAttr attrN)
 {
   処理内容
 }

関数名はどんなものでもよい。コールバック関数はルールが適用された際に呼
び出され、関数の第一引数にはルールが適用された膜が渡される。第二引数以
降は、呼び出された際に渡されたリンクを受け取る。

| 引数番号 | 型          | 意味               |h
| 1        | ReactCxt    | ルール適用時の実行コンテキスト |
| 2        | LmnMembrane | 関数を呼び出した際の所属膜 |
| 2N+1     | LmnAtom     | 第N引数のリンク先のアトム(or整数などのデータ)  |
| 2N+2     | LmnAttr     | 第N引数のリンク先の属性  |

引数で渡されたリンクは、関数内でリンク先のプロセスの解放や、リンクの接
続を行わなければ、リンクの一方がどこにも接続されていない状態になってし
まう。そのため、関数内で適切に処理を行わなければならない。

例:

 /* 第一引数のアトムの名前を出力する */
 void print_any(ReactCxt rc, LmnMembrane *mem, LmnWord a0, LmnLinkAttr t0)
 {
   if (LMN_ATTR_IS_DATA(t0)) {
     switch (t0) {
     case LMN_INT_ATTR:
       printf("%d\n", (int)a0);
       break;
     case LMN_DBL_ATTR:
       printf("%f\n", lmn_get_double(a0));
       break;
     default:
       fprintf(stderr, "PRINT.C: uhknown data type\n");
       break;
     }
   } else { /* symbol atom */
     printf("%s\n", LMN_ATOM_STR(a0));
   }
 
   lmn_mem_remove_atom(mem, a0, t0);
   lmn_free_atom(a0, t0);
 }

*** 2. 初期化関数を作る
SLIMは共有ライブラリを見つけると初期化処理を行う関数を一度だけ呼び出す。
この初期化関数の中で、コールバック関数の登録を行う。

初期化関数は、次のような形式で書く。

 void init_ファイル名(void);

ファイル名は、Cのソースファイル名の拡張子を除いた部分で、例えばSILMは
print.c というファイルならば init_aaa という関数を記述する。

コールバック関数の登録には lmn_register_c_fun を使う。

 /* name: 登録名
    f: 登録する関数へのポインタ
    arity: 登録する関数が受け取るリンクの数
 */ 
 void lmn_register_c_fun(const char *name, void *f, int arity);

例えば、print.cのprint_any というリンクを一つ受け取るコールバック関数を
printという名前で登録する場合、
 int init_print() {
   lmn_register_c_fun("print", print_any, 1);
 }

もちろんinit関数ではその他にプログラマが好きな初期化処理を書いてもいい。

*** 3. コンパイルと配置

1.,2.で作成したCのファイルを好きなCコンパイラでコンパイルし、共有ライブ
ラリ（Windowsなら動的ライブラリ）を作成する。この時のライブラリのファイ
ル名とinit関数の名前は対応していなければならない。できた共有ライブラリ
をSLIMが認識するパス内に配置する。

**  LMNtalプログラムからの関数の呼び出し
登録されたCの関数を呼び出すためには '$callback'アトムを使う。アトムの
引数は以下の意味をもつ。

- 第1引数: 呼び出したいコールバック関数の登録名をunaryアトムで指定する。
- 第2〜N引数: 関数に渡すリンク。ここでの順番のままコールバック関数に渡される。登録時のアリティとリンク数は一致していなければならない。

例:
 '$callback('print', hoge).
