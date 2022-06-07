                nam     Display
                ttl     program         ; module

* Disassembled 1900/00/00 00:32:30 by Disasm v1.5 (C) 1988 by RML

                use     defsfile

tylg            set     Prgrm+Objct
atrv            set     ReEnt+rev
rev             set     $01
edition		set	$02

                mod     eom,name,tylg,atrv,start,size

u0000           rmb     450
size            equ     .

name            equ     *
                fcs     /Display/
                fcb     edition

; Take a group of hex digits supplied on the command line, convert each pair to a character
; and then write the characters to the screen.
		
start           cmpd    #$0001		; check size of parameter area
                bls     ExitNoErr	; exit if less than 2		
				
                pshs    x		; save parameter pointer
                leay    ,x		; point y at parameters
		
L001F           bsr     HexByte		; get hex byte
                bcs     L0027		; branch on error
		
                stb     ,x+		; save it
                bra     L001F		; try and get another
		
L0027           tfr     x,d		; get pointer to last byte into d
                subd    ,s		; subtract stack pointer, work out how many
                tfr     d,y		; move count to y
                puls    x		; restore param pointer, now point to bytes to output
                lda     #$01		; stdout
                os9     I$Write		; go write it
                bcs     Exit		; exit if no error
		
ExitNoErr       clrb
Exit            os9     F$Exit

HexByte         ldb     ,y+		; get a byte from parameters
                cmpb    #','		; comma?
                bne     L0042		; no skip
		
L0040           ldb     ,y+		; get next char
L0042           cmpb    #' '		; space?
                beq     L0040		; yes loop and get next
		
                leay    -$01,y		; move pointer back one
                bsr     HexToBin	; convert to hex
                bcs     L0061		; branch on error
		
                pshs    b		; save it
                bsr     HexToBin	; get next digit
                bcs     L005E		; branch on error
		
                lsl     ,s		; shift first digit to MS nibble
                lsl     ,s
                lsl     ,s
                lsl     ,s
                addb    ,s		; add it in
                stb     ,s		; save it back in stack
L005E           clrb			; clear b
                puls    b		; retrieve result	
L0061           rts

HexToBin        ldb     ,y		; get character at pointer
                subb    #'0'		; convert to binary		
                cmpb    #$09		; bigger than 9?
                bls     L007A		; branch if lower
		
                cmpb    #$31		; is it lower case ?
                bcs     L0070		; no, skip
		
                subb    #$20		; make it uppercase....

L0070           subb    #$07		; convert to binary
                cmpb    #$0F		; was it valid hex (too high)?
                bhi     L007F		; no, flag error
		
                cmpb    #$0A		; was it valid hex (too low)?
                bcs     L007F		; no, flag error
		
L007A           andcc   #~Carry		; flag no error
                leay    $01,y		; move pointer to next
                rts
		
L007F           comb			; flag error
                rts

                emod
eom             equ     *
                end
