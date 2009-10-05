# 
#  translate_generator.rb - 
# 
#    Copyright (c) 2008, Ueda Laboratory LMNtal Group <lmntal@ueda.info.waseda.ac.jp>
#    All rights reserved.
# 
#    Redistribution and use in source and binary forms, with or without
#    modification, are permitted provided that the following conditions are
#    met:
# 
#     1. Redistributions of source code must retain the above copyright
#        notice, this list of conditions and the following disclaimer.
# 
#     2. Redistributions in binary form must reproduce the above copyright
#        notice, this list of conditions and the following disclaimer in
#        the documentation and/or other materials provided with the
#        distribution.
# 
#     3. Neither the name of the Ueda Laboratory LMNtal Group nor the
#        names of its contributors may be used to endorse or promote
#        products derived from this software without specific prior
#        written permission.
# 
#    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
#    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
#    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
#    A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
#    OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
#    SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
#    LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
#    DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
#    THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
#    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
#    OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
# 
#  $Id: translate_generator.rb,v 1.5 2008/09/19 05:18:17 riki Exp $


# わざわざ既にあるインタプリタを消すのも怖いので,当面はトランスレータ生成にのみ使用する
# その場合, translate_generated.cを出力し, translate_instruction_generated関数を定義する

# mode
#  行頭で#__echoのように指定すると,次のモード指定まではその出力方式になる
#  0: __ignore 無視する, 最初にモードが指定されるまでは__ignoreになっている
#  1: __echo 書かれていることをそのままトランスレータ/インタプリタのコードに出力
#  2: __echo_i インタプリタ生成時はそのまま出力,トランスレータ生成時は無視する
#  3: __echo_t トランスレータ生成時は～
#  4: __format インタプリタ生成時は書かれていることがインタプリタのコードに出力され, $$1などのマクロ部分はlmn実行時に置き換えられる
#              トランスレータ生成時は書かれていることがトランスレート結果のCコードに出力され, マクロの置換はトランスレート実行時に行われる
#  5: __format_i インタプリタ生成時は～
#  6: __format_t トランスレータ生成時は～

# case
#  行頭で#hogeのように上記のモードにないものを指定すると,中間命令INSTR_HOGE(すべて大文字に置換される)に対するコード生成を行う
#  #hoge X Y Zのように引数を指定する
#  case開始から次のcaseまでがその中間命令を実行するためのコードである

# argument
#  引数が指定された場合,その引数を格納する変数と読み込むためのコードが生成される
#  書かれた通りの型で変数宣言が行われる #hoge fuga と書かれていれば, fugaという型を持つ変数が用意され,fugaサイズ分だけ中間命令列から読込みを行う
#  分割はスペースで行う
#  __formatモードでは,$0,$1,$2の形で変数にアクセスできる(簡単のため$9まで)
#  インタプリタ実行中,トランスレータ変換中において, 変数はtarg0,targ1,...の名前で確保されるため,__echoからはそのようにアクセスできる
#  トランスレータ変換結果では,$0,$1の部分に即値が埋め込まれる

# macro
#  上の引数アクセス以外に,__formatモードで使用できるマクロがある
#  $s successcode.成功したとき実行すべきコード.proceedでしか使わないかも
#  $f failcode.失敗したとき実行すべきコード.次のマッチングに行ったり,ルール自体が失敗したりする
#( $r recursive.再帰呼び出し? まだ考えていない )
#  また,マクロではなく単に$をコードに使う場合は$$と書く ab$1cdは"abtarg1cd"となり, ab$$1cdは "ab$1cd"となる

# list argument
#  [1,2,3,4]のような可変長整数リストを引数とする命令が多いため,これを読み込む場合
#  #hogehoge $listと書くことで適当な読込みを行う("$list"全体で予約名)
#  $0[0]のように要素にアクセスする $0_numで要素数にアクセスできる (これらの動作は通常の変数とは異なる)
#  この場合インタプリタに対しては,  int arg0_num=読み込み;int *arg0=malloc(arg0_num);for(...(全部読み込み))  を生成する.
#  トランスレータに対しては,  int arg0_num=読み込み;int *arg0=malloc(arg0_num);for(...(全部読み込み))  を生成し,
#  更にトランスレータ出力結果に  int arg0_num=定数;int arg0[] = {...};  が出力されるようなコード生成を行う.
#  例外的にトランスレート出力結果においてもarg0の名前が残っているので注意

# global id
#  トランスレート結果としてsoにコンパイルされる場合,ローカルなidからグローバルなidへの変換が必要になる
#  トランスレータの出力コードにおいてのみ意味を持ち,それ以外では何もしないマクロを用意する
#  TR_GSID() trans global symbol id
#  TR_GFID() trans global functor id
#  TR_GRID() trans global ruleset id

# 書くのが難しい場合は,translate.c内translate_instruction関数内に直接書く

# true:  generate translator
# false: generate interpreter
$translateor_generate = true

def case_open(op, arg)
  # case文先頭のコード生成
  # 変数の宣言と中間命令からの読み込み
  # load.c: load_argに相当する処理で読み込む
  print "case INSTR_", op.upcase, ":{\n"
  for i in 0..arg.size-1
    if arg[i] != "$list"
      print "  ", arg[i], " targ", i, ";\n"
    else
      print "  int *targ", i, ";\n"
      print "  int targ", i, "_num;\n"
    end
  end
  for i in 0..arg.size-1
    if arg[i] != "$list"
      print "  READ_VAL(", arg[i], ", instr, targ", i, ");\n"
    else
      print "  READ_VAL(LmnJumpOffset, instr, targ", i, "_num);\n"
      print "  targ", i, " = malloc(sizeof(int)*targ", i, "_num);\n"
      print "  { int i; for(i=0; i<targ", i, "_num; ++i){ READ_VAL(int, instr, targ", i, "[i]); } }\n"
    end
  end
end

def format_print(line)
  # \ -> \\
  # " -> \"
  # % -> %%
  # $$ : $
  # $X : X番の引数(Xは一文字ということにしておく)
  line.gsub!("\\", "\\\\")
  line.gsub!("\"", "\\\"")
  line.gsub!("%", "%%")
  format_arg = []
  pos = 0
  while((pos=line.index("$", pos+1)) != nil)
    if line[pos+1] == "$"[0]
      line[pos,1] = ""
    elsif line[pos+1]>="0"[0] && line[pos+1]<="9"[0]
      format_arg << line[pos+1] - "0"[0]
      line[pos,2] = "%d"
    end
  end
  print "\tindent();\n"
  print "\tprintf(\"", line, "\\n\""
  format_arg.each{|x| print ", arg", x}
  print ");\n"
end

# -iオプションがついていたらインタプリタ生成, ついていなければトランスレータ生成
if ARGV == 1 && ARG[0] == "-i" then $translator_generate = false end
first_of_case = true
mode = 0
argument = []
line = ""
while(line=gets())
  line.chop!
  if line == ""
    # skip 
  elsif line == "#__end"
    if not first_of_case
      first_of_case = true
      print "}\n"
    end
    mode = 1
  elsif line[0] == "#"[0]
    t = line[1,line.size-1].split(" ")
    a = ["__ignore", "__echo", "__echo_i", "__echo_t", "__format", "__format_i", "__format_t"].index(t[0])
    if a == nil
      if first_of_case
        first_of_case = false
      else
        print "}\n"
      end
      case_open(t[0], t[1,t.size-1])
      mode = 4
    else
      mode = a
    end
  else
    case mode
    when 0 #ignore
    when 1 #echo
      print line, "\n"
    when 2 #echo_i
    when 3 #echo_t
    when 4 #format
      format_print(line)
    when 5 #format_i
    when 6 #format_t
    end
  end
end

if not first_of_case
  print "}\n"
end
