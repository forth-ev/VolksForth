	.P816
	.ORG $4000
	
S1:	SEI
	SEP #$30
	SEC
	XCE
S2:	
	LDA #'a'
	
loop:	
	CLC
	XCE
	REP #$30

	CLI
	JSL $E063
	NOP 
	NOP
	NOP
	
	SEI
	SEP #$30
	SEC
	XCE
	
	CLC
	ADC #1
	CMP #'z'+1
        BNE loop
		
	CLC
	XCE
	REP #$30
	CLI 
	
	LDA #$0D
	.BYT 00
	JSL $E063
	NOP
	NOP
	NOP
	
	DEC count
	LDA count
	cmp #$01
	.byt $00
	BNE S1
	
done:	JSL $E066
	RTL

count:  .BYT $0000