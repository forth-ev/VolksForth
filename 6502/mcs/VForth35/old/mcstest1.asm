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
	
done:	JML $E066
	RTL
