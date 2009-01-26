# mode
#  0: __ignore ignore
#  1: __echo echo
#  2: __echo_i echo for interpreter
#  3: __echo_t echo for translator
#  4: __format format
#  5: __format_i format for interpreter
#  6: __format_t format for translator

# argument
#  多分大文字ならC言語由来の型,小文字ならLMNtal由来の型とかにする(I:int,F:float,f:functorみたいな)
#  v
#  i
#  f

# format
#  interpret時はformatでの出力はそのままコード
#  translate時はformatでの出力はprintf文
# echo
#  interpret時はechoでの出力はそのままコード
#  translate時はechoでの出力はそのままコード

# true:  generate translator
# false: generate interpreter
$translateor_generate = true

def case_open(op, arg)
  # case文先頭のコード生成
  # 変数の宣言と中間命令からの読み込み
  # load.c: load_argに相当する処理で読み込む
  print "case INSTR_", op.upcase, ":{\n"
  for i in 0..arg.size-1
    case arg[i]
    when "a"
      print "\tLmnLinkAttr arg", i, ";\n"
    when "v"
      print "\tLmnInstrVar arg", i, ";\n"
    when "I"
      print "\tint arg", i, ";\n"
    when "D"
      print "\tdouble arg", i, ";\n"
    else
      print "\tsomething?? arg", i, ";\n"
    end
  end
  for i in 0..arg.size-1
    case arg[i]
    when "a"
      print "\tREAD_VAL(LmnLinkAttr, instr, arg", i, ", ", i, ");\n"
    when "v"
      print "\tREAD_VAL(LmnInstrVar, instr, arg", i, ", ", i, ");\n"
    when "I"
      print "\tREAD_VAL(int, instr, arg", i, ", ", i, ");\n"
    when "D"
      print "\tREAD_VAL(double, instr, arg", i, ", ", i, ");\n"
    else
      print "\tREAD??(,,);\n"
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
