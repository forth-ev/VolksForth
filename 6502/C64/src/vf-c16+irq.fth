

$fcb3 >label IRQ   \ normal IRQ
$fffe >label >IRQ  \ 6502-Ptr to IRQ

\ selfmodifying code:
Label RAMIRQ       \ the new IRQ
   rom RAMIRQ $15 + sta RAMIRQ $17 + stx
(  +9) RAMIRQ $1b + $100 u/mod # lda pha
                               # lda pha
(  +f) tsx $103 ,x lda pha   \ flags
( +14) 0 # lda 0 # ldx IRQ jmp
( +1b) ram rti end-code

