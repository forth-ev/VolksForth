\ 100* 100U/

\needs code    INCLUDE" D:TAS65.FS

CODE 100*  ( N1 - N2)
 SP X) LDA
 N     STA
 SP )Y LDA
 N  1+ STA
 N     ASL
 N  1+ ROL
 N     ASL
 N  1+ ROL
 N     LDA
 N  2+ STA
 N  1+ LDA
 N 3 + STA
 N  2+ ASL
 N 3 + ROL
 N  2+ ASL
 N 3 + ROL
 N  2+ ASL
 N 3 + ROL
       CLC
 N     LDA
 N  2+ ADC
 N     STA
 N  1+ LDA
 N 3 + ADC
 N  1+ STA
 N 2+  ASL
 N 3 + ROL
       CLC
 N     LDA
 N  2+ ADC
 SP X) STA
 N  1+ LDA
 N 3 + ADC
 SP )Y STA
  NEXT JMP
END-CODE

LABEL 4/+
 N 7 + LSR
 N 6 + ROR
 N 5 + ROR
 N 4 + ROR
 N 7 + LSR
 N 6 + ROR
 N 5 + ROR
 N 4 + ROR
       CLC
 N     LDA
 N 4 + ADC
 N     STA
 N 1+  LDA
 N 5 + ADC
 N 1+  STA
 SP X) LDA
 N 6 + ADC
 SP X) STA
 SP )Y LDA
 N 7 + ADC
 SP )Y STA
       RTS

CODE  100U/  ( U - N)
 N     STX
 N 4 + STX
 SP X) LDA
    .A ASL
 N 1+  STA
 N 5 + STA
 SP )Y LDA
    .A ROL
 SP X) STA
 N 6 + STA
       TXA
    .A ROL
 SP )Y STA
 N 7 + STA
   4/+ JSR
 N 7 + LSR
 N 6 + ROR
 N 5 + ROR
 N 4 + ROR
   4/+ JSR
  NEXT JMP END-CODE

