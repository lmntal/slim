# 膜のハッシュ関数
mhash.cに実装した膜のハッシュ関数について，どのような実装になっているかを読み解
くためと，備忘のためにわかりやすい形で書いておく．

\f[
 membrane(M) = \left(ADD0 + \sum_{分子 \in M}{molecule(分子)} \right)
                   \odot \left( MUL0 \times \prod_{分子 \in M}{molecule(分子)} \right) \\
\f]

Mのハッシュ値を求めている際にMのハッシュ値が必要になる場合は\f$membrane(M)\f$は定数を返す．

\f[
 molecule(分子) = \left( \sum_{計算単位 \in 分子}{unit(計算単位, 0)} \right) \odot
           \left( \prod_{計算単位 \in 分子}{unit(計算単位, 0)} \right) 
\f]

分子は膜外部には出ない．

\f[
 unit_{atom}(atom, d) =
  \begin{cases}
   link_{atom}(atom) & d = D \\
   C^N atom_{type}(atom) + \sum_{i=0}^{N-1}{C^{N-i-1} unit(atomの第i引数, d+1)} & d < D
  \end{cases}
\f]

Nはアトムのアリティ．\f$d>0\f$の場合，atomリンク先のどれかの計算単位がひとつ手前(深さがd-1)で計算されて
いるが，そのリンク先に対しては計算を行わない．

\f{eqnarray*}{
 unit_{membrane}(M, d) &=&
  \begin{cases}
   link_{mem}(M) & d = D \\
   membrane(M) \sum_{外へのリンク \in M}{unit(リンク先, d+1)
    From } & d < D
  \end{cases} \\
  From &=&  membrane(M) \odot \left( \sum_{i=0}^{T-1}{B^{T-i} membrane(M_i)} \right)
         link_{atom}(a_{from})
\f}

$a_{from}$はリンク元のアトムである．$M_i$は$a_{from}$からM外部に至るまでに貫く膜である．

\f{eqnarray*}{
 link_{atom}(a_{to}) & = & \left(argnum(a_{to})+1 \right) atom_{type}(a_{to}) \\
 link_{membrane}(M) & = &
    \left( B^T membrane(M) + \sum_{i=0}^{T-1}{B^{T-i-1} membrane(M_i)} \right)
     link(a_{to})
\f}

\f$atom_{type}\f$はファンクタに対するハッシュ値で，実装依存である．

