
include vf-lbls-cbm.fth

Code cbmopen  ( lfn ga sa fname fnlen -- )
  5 # lda  Setup jsr
  N 8 + lda  N 6 + ldx  N 4 + ldy  SETLFS jsr
  N     lda  N 2 + ldx  N 3 + ldy  SETNAM jsr
  OPEN jsr  xyNext jmp  end-code

Code cbmclose  ( lfn -- )
  SP X) lda  CLOSE jsr
  Label xyPop   0 # ldx  1 # ldy
  Pop jmp  end-code

Code cbmchkin  ( lfn -- )
  SP X) lda  tax  CHKIN jsr  xyPop jmp  end-code

Code cbmchkout  ( lfn -- )
  SP X) lda  tax  CHKOUT jsr  xyPop jmp  end-code

Code cbmclrchn  ( -- )
  CLRCHN jsr  xyNext jmp  end-code

Code cbmbasout  ( chr -- )
  SP X) lda  CHROUT jsr  xyPop jmp  end-code

Code cbmbasin  ( -- chr )
  CHRIN jsr  Push0A jmp  end-code
