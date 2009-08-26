#! /bin/sh

#【説明】
#       このスクリプトは, 
#         1. SLIMの非決定実行用benchmark問題として用意された例題を, ndとltl_allで複数回ずつ実行する.
#         2. データをcsv形式で集計する.
#       ために用意している.
#       実行するSLIMはsrcディレクトリに生成されたものである.
#       また, パラメタ毎にコンパイルされた中間コードがディレクトリ./ilにあり,
#       ./LISTに記述された
#              モデル | パラメタ | 性質
#       の全ての組み合わせを実行する.
#       実行結果は全てresultディレクトリに置かれる.
#
#【注意】
#       環境によっては動かないかも…
#
#【必須】
#       データは, runtime_statusの出力を用いる.
#       故に, 使用するSLIMはconfigure時に--enable-profileオプションを付けたものでなければならない.
#
#【推奨】
#       事前に以下の設定を行うことが好ましい.
#
#       $ ulimit -tN 
#       $ ulimit -s unlimited
#
#       1. SLIMの実行時間をN秒までに制限
#       2. 使用できるスタック領域メモリの制限を解除 
#
#【使用方法】
#       $ ./RUN.sh [args]
#       このスクリプトの実行時引数[args]として, slimのオプションを追加することができる.
#       最適化などを行った際に新設されたオプション等があれば[args]へ
# 


arch=.il
bar="_"   
slimDIR=../../src/slim
paraDIR=./il/
dumpDIR=./result/

# データを取る回数の指定
benchmark_time=5

# LISTから実行するモデルがあるだけループ
line=1
while IFS="|" read model param property
do
  # LISTの先頭2行を読み飛ばす
  if test $line -le 2
  then
     line=`expr ${line} + 1`
     continue
  fi

  # ndモード
  echo "start: nd"
  for m in $model
  do
    for i in $param
    do
      echo "  $m, $i"
      echo "model=$m, $i" > "$dumpDIR$m$bar$i.csv"
      #出力値(各列)の意味を出力. とりあえず, スクリプト側で書き込ませる.
      echo "#:states, #:ltl_err, T:total(s), #:atom, #:mem, S:atm(B), S:mem(B), S:hash(B), S:m_enc(B), S:avg.(B), S:state(B), #:mhash, #T:hash(s), #:memeq, T:memeq(s), #:m_enc, T:m_enc(s), T:expnd(s), T:m_cpy(s), T:c_else(s), #:hashv, #:collision" \
                          >> "$dumpDIR$m$bar$i.csv"

      sleep 1

      for l in $(seq 1 $benchmark_time)
      do
         # runtime_statusの出力先は標準エラーなので, 標準出力(ディスクリプタ1番)は捨ててしまう.
         # 標準出力を捨てるようにしていても状態遷移グラフの出力が行われると遅くなるようなので, 状態遷移グラフを出力しない--no_dumpオプションを使用する
         # runtime_statusの出力をcsv形式にするオプション--sp_verboseも使用
         $slimDIR --nd $@ --sp_verbose --no_dump --psym --nd "$paraDIR$m$bar$i$arch" \
            1>/dev/null  2>> "$dumpDIR$m$bar$i.csv"

         sleep 2
      done
    done
  done

  # ltl_allモード
  echo start: ltl_all
  for m in $model
  do
    for i in $param
    do
      for p in $property
      do
        echo "  $m, $i, ltl=$p"
        echo "model=$m, $i" > "$dumpDIR$m$bar$i-property$p.csv"
        echo "#:states, #:ltl_err, T:total(s), #:atom, #:mem, S:atm(B), S:mem(B), S:hash(B), S:m_enc(B), S:avg.(B), S:state(B), #:mhash, #T:hash(s), #:memeq, T:memeq(s), #:m_enc, T:m_enc(s), T:expnd(s), T:m_cpy(s), T:c_else(s), #:hashv, #:collision" \
                            >> "$dumpDIR$m$bar$i-property$p.csv"
        sleep 1
        
        for l in $(seq 1 $benchmark_time)
        do
           $slimDIR --ltl_all $@ --sp_verbose --no_dump --psym "$m$p.psym" --nc "$m$p.nc" "$paraDIR$m$bar$i$arch" \
              1>/dev/null  2>> "$dumpDIR$m$bar$i-property$p.csv"
        sleep 2
        done
      done
    done
  done

done < ./LIST

echo finish
