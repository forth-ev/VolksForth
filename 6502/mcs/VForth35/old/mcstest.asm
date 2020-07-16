	.P816
	.ORG $4000
	.SMART +
	
S1:	SEI
	SEP #$30
	SEC
	XCE
S2:	

	
loop:	
	CLC
	XCE
	REP #$30

	DEA
        LDA $F2
	STA [$80]
	XBA
	MVP 1,2
	
	CLI
nodata:	LDA $43
	AND #1
	BEQ nodata
	JSL $E033
	STA $FE
	JSL $E063

	
	SEI
	SEP #$30
	SEC
	XCE
	

        BNE loop
		
	CLC
	XCE
	REP #$30
	CLI 
	
	DEC count
	LDA count
	cmp #$01
	BNE S1
	
done:	JSL $E066
	RTL

count:  .BYT $0000