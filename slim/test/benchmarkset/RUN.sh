#! /bin/sh

#【説明】
#       このスクリプトは, 
#         1. SLIMの非決定実行用benchmark問題として用意された例題を, ndとltl_allで複数回ずつ実行
#         2. モデル毎にデータをcsv形式で集計
#       ために用意しています.
#       実行するSLIMは, makeによってsrcディレクトリに生成されたバイナリとなります.
#       実行するプログラムは, パラメタ毎にコンパイルされた中間コードの内, ./LISTに記述された
#              モデル | パラメタ | 性質
#       の全ての組み合わせとなります. (中間コード置き場: ./il/) 
#       デフォルトでは, 各モデルの代表的なパラメタのみ指定してあります.
#       ちなみに中間コードのファイル名は, 
#              モデル名_パラメタ.il
#       の形式で用意されており, 例えばabpモデルのパラメタm10000は
#              abp_m10000.il
#       のようになっています.
#       時間が膨大にかかるorメモリ不足に陥るパラメタも多数用意してあるので, 必要に応じて追加するとよいでしょう.
#
#【注意】
#       (環境によっては動かないかも…)
#       超長期に渡る実行は, マシンが落ちる恐れがあるため注意してください.
#
#【必須】
#       実行データは, runtime_statusの出力を使用します.
#       そのため, configure時に--enable-profileオプションを付けてからmakeしたSLIMを使用してください.
#
#【推奨】
#       事前に以下の設定を行うのが好ましいでしょう.
#
#       $ ulimit -tN 
#       $ ulimit -s unlimited
#
#       1. SLIMの実行時間をN秒までに制限
#       2. 使用できるスタック領域メモリの制限を解除 
#
#【使用方法】
#       $ ./RUN.sh [args]
#       このスクリプトの実行時引数[args]として, slimのオプションを追加することができます.
#       最適化などを行った際に新設されたオプション等があれば[args]へ入力してください.
#       各パラメタにつきデフォルトでは5回ずつデータを取るようにしていますが, 過多or不足であれば
#               benchmark_time=5
#       でデータ取得回数を指定しているので, こちらを変更してください. 
# 
#【出力データ】
#       benchmarkset/result 直下に、モデル毎にcvs形式で出力します.
#

arch=.il
bar="_"   
slimDIR=../../src/slim
paraDIR=./il/
dumpDIR=./result/

# データを取る回数の指定
benchmark_time=5


# ndモード

line=1
echo "************** Non-Deterministic Mode ***************"
while IFS="|" read model param property
do
  # LISTの先頭2行を読み飛ばす
  if test ${line} -le 2
  then
     line=`expr ${line} + 1`
     continue
  fi

  for m in ${model}
  do
    for i in $param
    do
      printf '%12s : %8s : %5s' ${m} ${i} '...'
      echo "model=${m}, param=${i}" >> "${dumpDIR}${m}.csv"
      
      #出力値(各列)の意味を出力. とりあえず, スクリプト側で書き込ませる.
      echo "#:states, #:ltl_err, T:total(s), #:atom, #:mem, S:atm(B), S:mem(B), S:hash(B), S:m_enc(B), S:avg.(B), S:state(B), #:mhash, #T:hash(s), #:memeq, T:memeq(s), #:m_enc, T:m_enc(s), T:expnd(s), T:m_cpy(s), T:c_else(s), #:hashv, #:collision" \
                          >> "${dumpDIR}${m}.csv"
      sleep 1

      for l in $(seq 1 ${benchmark_time})
      do
         # runtime_statusの出力先は標準エラーなので, 標準出力(ディスクリプタ1番)は捨ててしまう.
         # 標準出力を捨てるようにしていても状態遷移グラフの出力が行われると遅くなるようなので, 状態遷移グラフを出力しない--no_dumpオプションを使用する
         # runtime_statusの出力をcsv形式にするオプション--sp_verboseも使用

         printf '%d..' ${l}         
         ${slimDIR} --nd ${@} --sp_verbose --no_dump "${paraDIR}${m}${bar}${i}${arch}" \
            1>/dev/null  2>> "${dumpDIR}${m}.csv"

         sleep 1
      done
      printf 'done\n' 
    done
  done
done < ./LIST


# ltl_allモード
line=1
printf '\n'
echo "************** Model Checker Mode *******************"
while IFS="|" read model param property
do
  if test ${line} -le 2
  then
     line=`expr ${line} + 1`
     continue
  fi

  for m in ${model}
  do
    for i in ${param}
    do
      for p in ${property}
      do
        printf '%12s : %8s : %10s%d : %5s' ${m} ${i} 'Property' ${p} '...'
        echo "model=${m}, param=${i}, prop={$p}" >> "${dumpDIR}${m}.csv"
        sleep 1
        for l in $(seq 1 $benchmark_time)
        do
           printf '%d..' ${l}
           ${slimDIR} --ltl_all ${@} --sp_verbose --no_dump --psym "${m}${p}.psym" --nc "${m}${p}.nc" "${paraDIR}${m}${bar}${i}${arch}" \
              1>/dev/null  2>> "${dumpDIR}${m}.csv"
           sleep 1
        done
        printf 'done\n'
      done
    done
  done
done < ./LIST

echo finish
