\ Double Cell 32bit arithmetics words

.( load additional double 32bit words )

: D/  ( d u -- d )  \ floored result
  SWAP OVER /MOD >R
  SWAP UM/MOD SWAP DROP R> ;


