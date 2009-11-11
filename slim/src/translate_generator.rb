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
#  行頭に# と#の後に空白を入れると,その行はコメントとみなされる
$modes = [  # 実装注意: シンボル定義だと略記の処理であまり気持ちよくない  後の処理で使うのでechoの後にformatを順番どおり並べること
"__ignore",   #  __ignore 無視する, 最初にモードが指定されるまでは__ignoreになっている
"__echo",     #  __echo 書かれていることをそのままトランスレータ/インタプリタのコードに出力
"__echo_i",   #  __echo_i インタプリタ生成時はそのまま出力,トランスレータ生成時は無視する
"__echo_t",   #  __echo_t トランスレータ生成時は〜
"__format",   #  __format インタプリタ生成時は書かれていることがインタプリタのコードに出力され, $$1などのマクロ部分はlmn実行時に置き換えられる
              #              トランスレータ生成時は書かれていることがトランスレート結果のCコードに出力され, マクロの置換はトランスレート実行時に行われる
"__format_i", #  __format_i インタプリタ生成時は〜
"__format_t"] #  __format_t トランスレータ生成時は〜
$format_first_index = $modes.index("__format") # format系modeの最初のindex
$format_end_index = $modes.index("__format_t") # format系modeの最後のindex

#  略記として,モードが__format_*の時,行の先頭が%である行は, __echo_*になる. (__formatの時は__echo, __format_iの時は__echo_i)

# case
#  行頭で#hogeのように上記のモードにないものを指定すると,中間命令INSTR_HOGE(すべて大文字に置換される)に対するコード生成を行う
#  #と命令名の間に空白を入れないようにすること
#  #hoge X Y Zのように引数を指定する
#  caseを開くと自動的に__formatモードになる
#  case開始から次のcaseまでがその中間命令を実行するためのコードである
#  #__endによって強制的にcaseを閉じることもできる 特にテンプレートの最後には必ず書く __end後は自動的に__echoモードになる

# argument
#  引数が指定された場合,その引数を格納する変数と読み込むためのコードが生成される
#  書かれた通りの型で変数宣言が行われる #hoge fuga と書かれていれば, fugaという型を持つ変数が用意され,fugaサイズ分だけ中間命令列から読込みを行う
#  引数のセパレータはスペース
#  __formatモードでは,$0,$1,$2の形で変数にアクセスできる(簡単のため$9まで)
#  インタプリタ実行中,トランスレータ変換中において, 変数はtarg0,targ1,...の名前で確保されるため,__echoからはそのようにアクセスできる
#  トランスレータ変換結果では,$0,$1の部分に即値が埋め込まれる

# macro
#  上の引数アクセス以外に,__formatモードで使用できるマクロがある
#  $s successcode.成功したとき実行すべきコード.proceedでしか使わないかも 変数名はsuccesscode
#  $f failcode.失敗したとき実行すべきコード.次のマッチングに行ったり,ルール自体が失敗したりする 変数名はfailcode
#( $r recursive.再帰呼び出し? まだ考えていない )
#  $a address.今処理中の命令語があるアドレスの表現.トランスレータで一意なサフィックスが欲しいときにだけ使う 変数名はop_address
#  また,マクロではなく単に$をコードに使う場合は$$と書く targ1=55のとき, ab$1cdは"ab55cd"となり, ab$$1cdは "ab$1cd"となる

# list argument
#  [1,2,3,4]のような可変長整数リストを引数とする命令が多いため,これを読み込む場合
#  #hogehoge $listと書くことで適当な読込みを行う("$list"全体で予約名)
#  $0[0]のように要素にアクセスする $0_numで要素数にアクセスできる (これらの動作は通常の変数とは異なる)
#  この場合インタプリタに対しては,  int targ0_num=読み込み;int *targ0=malloc(targ0_num);for(...(全部読み込み))  を生成する.
#  トランスレータに対しては,  int targ0_num=読み込み;int *targ0=malloc(targ0_num);for(...(全部読み込み))  を生成し,
#  更にトランスレータ出力結果に  int targ0_num=定数;int targ0[] = {...};  が出力されるようなコード生成を行う.
#  例外的にトランスレート出力結果においてもtarg0の名前が残っているので注意
#  中間バイト列からの読込みはLmnInstrVar単位で行われ, LmnWordの配列に格納される(インタープリット時にvectorを使うため)

# functor argument
#  ファンクタは種類と各種に応じた即値のペアであるため#hogehoge $functorと書くことで特別扱いをする
#   -ファンクタの種類:   $0_attr
#   -整数ファンクタ:     $0_long_data
#   -小数ファンクタ:     $0_double_data
#   -文字列ファンクタ:   $0_string_data
#   -シンボルファンクタ: $0_functor_data
#  と種類に応じて使うデータを変える必要がある (実際扱うコードもバラバラなため,冗長な訳ではない)
#  インタプリタに対しては,意味上では int targ0_attr=読み込み;struct union LmnFunctoLiteral targ0=読み込み;  を生成する.
#  トランスレータに対しては, int targ0_attr=読み込み;struct union LmnFunctoLiteral targ0=読み込み;  を生成し,(targ0.long_data等でアクセスする)
#  更にトランスレータ出力結果に  int targ0_attr=定数;何か targ0_何か=定数; が出力されるようなコード生成を行う.(もちろんattrに応じた種類のみを利用すること)
#  実際には即値が埋め込まれる

# global id
#  トランスレート結果としてsoにコンパイルされる場合,ローカルなidからグローバルなidへの変換が必要になる
#  トランスレータの出力コードにおいてのみ意味を持ち,それ以外では何もしないマクロを用意する
#  TR_GSID(x) trans global symbol id
#  TR_GFID(x) trans global functor id
#  TR_GRID(x) trans global ruleset id

# 書くのが難しい場合は,translate.c内translate_instruction関数内に直接書く

# true:  generate translator
# false: generate interpreter
$translator_generate = true

def case_open(op, arg)
  # case文先頭のコード生成
  # 変数の宣言と中間命令からの読み込み
  print "case INSTR_", op.upcase, ":{\n"
  for i in 0..arg.size-1
    if arg[i] == "$list"
      print "  LmnWord *targ", i, ";\n"
      print "  int targ", i, "_num;\n"
    elsif arg[i] == "$functor"
      print "  LmnLinkAttr targ", i, "_attr;\n"
      print "  union LmnFunctorLiteral targ", i, ";\n"
    else
      print "  ", arg[i], " targ", i, ";\n"
    end
  end

  for i in 0..arg.size-1
    if arg[i] == "$list"
      # 読込みを行うマクロを出力
      print "  READ_VAL_LIST(instr, targ", i, ");\n"
    elsif arg[i] == "$functor"
      # 読込みを行うマクロを出力
      print "  READ_VAL_FUNC(instr, targ", i, ");\n"
      # トランスレート後は全部即値で埋め込む
    else
      # その他の引数は単に読み出すだけでよい
      print "  READ_VAL(", arg[i], ", instr, targ", i, ");\n"
    end
  end

  if $translator_generate and arg.include?("$list")
    # トランスレータの場合は出力にint targ1_num=5; int targ1[]={1,2,3,4,5}; を含める必要がある
    # この出力は関数の途中でも出てくる+名前がかぶるので{}で囲う必要がある 幸い他の中間命令でこのデータが必要になることはないはず
    print "  print_indent(indent++); printf(\"{\\n\");\n"

    for i in 0..arg.size-1
      if arg[i] == "$list"
        print "  tr_print_list(indent, ", i, ", targ", i, "_num, targ", i, ");\n"
      end
    end
  end
  
  # debug: 各中間命令名を実行時に出力
  #if $translator_generate
  #  print '  printf(" printf(\"start instr : ', op, '\\\\n\");\n");', "\n"
  #end
end

def case_close(arg)
  # リストを読み込んでいたら開放する必要がある
  for i in 0..arg.size-1
    if arg[i] == "$list"
      print "  free(targ", i, ");\n"
    end
  end
  
  if $translator_generate
    # case文が変数を含む場合だけ($listが引数にある場合だけ)閉じる
    if arg.include?("$list")
      print "  print_indent(--indent); printf(\"}\\n\");\n"
    end
    
    # トランスレート時は次の読み込み位置をリターン
    print "  return instr;\n"
  else
    # インタプリタはデフォルトではswitchを抜けるだけ
    print "  break;\n"
  end

  print "}\n"
end


# トランスレータ生成時に処理文をprintfで出力するためのプログラムを出力する
# 上の引数のところで書いたように一部の種の変数は置換を行わないため, n番引数がそれか否かを知るためargを引数に取る

def print_interp_format(line, arg)
  # $$ : $
  # $X : targX X番目の引数
  # $f : 失敗確定なので return FALSE
  # $s : 成功確定なので return TRUE
  # $a : op_address 関数の最初で保存しておく ややこしいから使えなくする？
  # 引数がファンクタの場合のみ, $1_long_data を targ1_long_data ではなく targ1.long_dataに変換してやる
  
  pos = 0
  while (pos=line.index("$", pos+1)) != nil
    if line[pos+1] == "$"[0]
      # "$$"なら1つ消して"$"にしてやる
      line[pos,1] = ""
    elsif line[pos+1]>="0"[0] && line[pos+1]<="9"[0]
      # 引数の場合
      x = line[pos+1] - "0"[0]
      if arg[x] == "$functor"
        # ファンクタなら上記の変換
        long_data_name = "_long_data"
        double_data_name = "_double_data"
        string_data_name = "_string_data"
        functor_data_name = "_functor_data"
        attr_name = "_attr"
        if line[pos+2,long_data_name.size] == long_data_name
          line[pos,long_data_name.size+2] = "targ"+x.to_s+".long_data"
        elsif line[pos+2,double_data_name.size] == double_data_name
          line[pos,double_data_name.size+2] = "targ"+x.to_s+".double_data"
        elsif line[pos+2,string_data_name.size] == string_data_name
          line[pos,string_data_name.size+2] = "targ"+x.to_s+".string_data"
        elsif line[pos+2,functor_data_name.size] == functor_data_name
          line[pos,functor_data_name.size+2] = "targ"+x.to_s+".functor_data"
        elsif line[pos+2,attr_name.size] == attr_name
          line[pos,attr_name.size+2] = "targ"+x.to_s+"_attr"
        else
          warn "unexpected functor type at " + $linenum.to_s + ".\n"
          warn line[pos+2,long_data_name.size]
        end
      else
        # ファンクタ以外なら単に置き換えるだけ
        line[pos,2] = "targ" + x.to_s
      end
    elsif line[pos+1] == "f"[0]
      # $fは失敗
      line[pos,2] = "return FALSE"
    elsif line[pos+1] == "s"[0]
      # $sは成功
      line[pos,2] = "return TRUE"
    else
      warn "unexpected macro at " + $linenum.to_s + ".\n"
      warn line
    end
  end

  puts line
end

def print_trans_format(line, arg)
  # \ -> \\ 文字列リテラルにおいて\を表したければ\\と書く
  # " -> \" 文字列リテラルにおいて"を表したければ\"と書く
  # % -> %% printfの文字列において%を表したければ%%と書く
  # $$ : $  このツール入力において$$はただの$を意味し,$1は1番目の引数を表す
  # $X : X番の引数(Xは一文字ということにしておく)
  
  # rubyの字句解析で\\が\になり,gsubのフォーマット解析で更に\\が\になるらしいので\\と出力したければ\8個
  line.gsub!("\\", "\\\\\\\\")
  line.gsub!("\"", "\\\"")
  line.gsub!("%", "%%")
  format_arg = []
  pos = 0
  while((pos=line.index("$", pos+1)) != nil)
    if line[pos+1] == "$"[0]
      # "$$"なら1つ消して"$"にしてやる
      line[pos,1] = ""
    elsif line[pos+1]>="0"[0] && line[pos+1]<="9"[0]
      x = line[pos+1] - "0"[0]
      if arg[x] == "$list"
        # リスト引数なら, 変換後出力に含まれるtarg1をそのまま参照
        line[pos,2] = "targ" + x.to_s
      elsif arg[x] == "$functor"
        # ファンクタの場合, formatの中で $1_long_data がある場合, これは即値_long_dataを意味するのではなく,
        # $1_long_data全体で1つの即値になって欲しい ということで特別な扱いが必要
        long_data_name = "_long_data"
        double_data_name = "_double_data"
        string_data_name = "_string_data"
        functor_data_name = "_functor_data"
        attr_name = "_attr"
        if line[pos+2,long_data_name.size] == long_data_name
          format_arg << "targ"+x.to_s+".long_data"
          line[pos,long_data_name.size+2] = "%ld"
        elsif line[pos+2,double_data_name.size] == double_data_name
          format_arg << "targ"+x.to_s+".double_data"
          line[pos,double_data_name.size+2] = "%lf"
        elsif line[pos+2,string_data_name.size] == string_data_name
          format_arg << "targ"+x.to_s+".string_data"
          line[pos,string_data_name.size+2] = "%d"
        elsif line[pos+2,functor_data_name.size] == functor_data_name
          format_arg << "targ"+x.to_s+".functor_data"
          line[pos,functor_data_name.size+2] = "%d"
        elsif line[pos+2,attr_name.size] == attr_name
          format_arg << "targ"+x.to_s+"_attr"
          line[pos,attr_name.size+2] = "%d"
        else
          warn "unexpected functor type at " + $linenum.to_s + ".\n"
          warn line[pos+2,long_data_name.size]
        end
      else
        # 普通の引数なら, 変換時に読み込んだ値を突っ込むために%ldに置換
        format_arg << "(int)targ"+x.to_s
        line[pos,2] = "%d"
      end
    elsif line[pos+1] == "s"[0]
      # "$s"ならsuccesscode
      format_arg << "successcode"
      line[pos,2] = "%s"
    elsif line[pos+1] == "f"[0]
      # "$f"ならfailcode
      format_arg << "failcode"
      line[pos,2] = "%s"
    elsif line[pos+1] == "a"[0]
      # "$a"ならその命令語インスタンスのあるアドレス
      format_arg << "op_address"
      line[pos,2] = "%p"
    else
      warn "unexpected macro at " + $linenum.to_s + ".\n"
      warn line
    end
  end
  # 最終的に表示するのはここ
  print "  print_indent(indent); printf(\"", line, "\\n\""
  format_arg.each{|x| print ", ", x}
  print ");\n"
end

# -iオプションがついていたらインタプリタ生成, ついていなければトランスレータ生成
if ARGV.include?("-i") then $translator_generate = false end
is_case_opened = false # 今case文が開いているかどうか
mode = "__ignore" # 今の出力モード
line = ""
arg = [] # 今開いているcaseの引数
$linenum = 0 #今の行数

while(line=STDIN.gets)
  $linenum = $linenum + 1
  line.chop!

  if line[0]=="#"[0] and (line.length==1 or line[1]==" "[0] or line[1]=="\n"[0] or line[1]=="\t"[0])
    # #だけの行 or #の直後に空白があればその行はコメント
    next
  elsif line == "#__end" # __end命令
    # 今ケースが開いていたら閉じる, その後__echoモードにする
    if is_case_opened
      is_case_opened = false
      case_close(arg)
    end
    mode = "__echo"
  elsif line[0] == "#"[0] # 何かのコマンド?
    t = line[1,line.size-1].split(" ")
    a = $modes.index(t[0]) # モード名をindexに変換
    
    if a == nil # モード指定ではない場合=中間命令対応のcase文の開始
      # 既にcase文が開いていたら閉じる
      if is_case_opened
        case_close(arg)
      end
      is_case_opened = true
      # ケースを開く, モードは__formatに
      arg = t[1,t.size-1]
      case_open(t[0], arg)
      mode = "__format"
    else # モード指定だった場合
      mode = t[0]
    end
  else # コマンドでもない場合
    if line == ""
      next
    end

    temp_mode = mode # 一時的なモード変数
    temp_mode_index = $modes.index(mode) #これはnilにはならない
    if line[0]=="%"[0] and temp_mode_index>=$format_first_index and temp_mode_index<=$format_end_index
      # 行頭が%でmodeがformat_*なら略記なのでecho_*にモードをずらす
      line[0,1] = " "
      temp_mode = $modes[temp_mode_index-($format_end_index-$format_first_index+1)]
    end

    case temp_mode
    when "__ignore"
    when "__echo"
      print line, "\n"
    when "__echo_i"
      if not $translator_generate
        print line, "\n"
      end
    when "__echo_t"
      if $translator_generate
        print line, "\n"
      end
    when "__format"
      if $translator_generate
        # トランスレータ用には$1や$f等を%dにしてprintfで包んで出力するイメージ
        print_trans_format(line, arg)
      else
        # インタプリタ用には$1や$f等をtarg1やreturnに変換して出力する
        print_interp_format(line, arg)
      end
    when "__format_i"
      if not $translator_generate
        print_interp_format(line, arg)
      end
    when "__format_t"
      if $translator_generate
        print_trans_format(line, arg)
      end
    end
  end
end

