* Disassembled by BeebDIS/6809, V1.30, at 10:11 2022-06-01

                use     defsfile

tylg            set     Prgrm+Objct
atrv            set     ReEnt+rev
rev             set     1
edition		set	2
                mod     eom,name,tylg,atrv,start,size

; 678

InputID         rmb     1		; input file id
OutputID        rmb     1		; output file id
ParamPtr        rmb     2		; Parameter pointer			
CurrentWord     rmb     1		; MSB of current word, CurrentByte is LSB
CurrentByte     rmb     1		; current byte being processed
BufEnd          rmb     2		; pointer to end of buffer
Checksum        rmb     1		; checksum of data of record
u0009           rmb     2
DataLen         rmb     2
Counter         rmb     1		; byte counter within record
RecType         rmb     1		; record type
u000F           rmb     1
NameBuf         rmb     6		; buffer that has 'Name= ' put into it by code!
InputBuf        rmb     656		; input buffer
size            equ     .


name            FCS     "Exbin"         ; OS9 Module name

                FCB     edition

                FCC     "Copyright 1982 Motorola, Inc."

attributes	equ	SHARE.+PEXEC.+PWRIT.+PREAD.+EXEC.+UPDAT.

start           stx     <ParamPtr	; save parameter pointer
                lda     #READ.		; read access		
                os9     I$Open		; open it
                bhs     L003C		; skip if opened ok

Exit           	os9     F$Exit		; exit

L003C           sta     <InputID	; save file id
                stx     <ParamPtr	; save updated parameter pointer
                lda     #WRITE.		; write accesss		
                ldb     #attributes	; attributes
                os9     I$Create	; create the output
                blo     Exit		; error, exit

                sta     <OutputID	; save output file id
                stx     <ParamPtr	; save updated param pointer.
		
                ldd     #$0000
                std     <u0009
                std     <DataLen	; zero data length
                sta     <u000F
                
		ldx     #"Na"		; put "Name: " in namebuf.....
                stx     <NameBuf
                
		ldx     #"me"
                stx     <NameBuf+2
                
		ldx     #"= "
                stx     <NameBuf+4
		
ReadNextChunk   lda     <InputID	; get input file id
                leax    <InputBuf,U	; point to input buffer
                ldy     #$0100		; maximum data size
                os9     I$ReadLn	; read a line
                lblo    L0114		; skip on error
	
                leax    <InputBuf,U	; point at input buffer
                tfr     X,Y		; copy to y
                tfr     X,D		; copy to d
                addd    #$0100		; point to end of max data
                std     <BufEnd		; save it
		
L0081           lda     ,X+		; get a byte from buffer
                cmpa    #'S'		; beginning of record?	
                beq     L008D		; yes, skip ahead

                cmpx    <BufEnd		; end of buffer?
                bne     L0081		; no get next

                bra     ReadNextChunk	; loop to read next chunk

L008D           lda     ,X+		; get next character from inpus
                suba    #'0'		; convert ASCII->bin
                sta     <RecType	; save record type
                beq     L009E		; Header record, skip

                cmpa    #$09		; Terminator record?
                bne     L009B		; no....

                bra     ReadNextChunk	; loop to read next chunk

L009B           deca			; test for 16 bit data record....
                bne     ReadNextChunk	; no, loop to read next chunk

L009E           bsr     HexByte		; get hex byte

                sta     <Checksum	; set checksum 
                suba    #$03		; length of data 
                sta     <DataLen+1
                sta     <Counter
                bsr     HexWord		; get address from record	

                tst     <RecType	; is this a type 1 record?
                beq     L00CD		; yes, skip

                pshs    X
                ldx     <u0009
                lda     <u000F
                beq     L00C0

                cmpx    <CurrentWord
                beq     L00C6

                leax    L018D,PCR
                bra     L00DE

L00C0           ldx     <CurrentWord
                lda     #$01
                sta     <u000F
L00C6           ldb     <DataLen+1
                abx
                stx     <u0009
                puls    X
L00CD           bsr     HexByte		; get a byte from input stream

                sta     ,Y+		; put in output
                dec     <Counter	; decrement byte counter
                bpl     L00CD		; loop if more to do

                lda     <Checksum	; get checksum?
                inca			; valid
                beq     L00EB		; yes skip on

                leax    CkSumErr,PCR	; point at checksum message
L00DE           lda     #$02		; stderr
                ldy     #$00FF		; max bytes to write
                os9     I$WritLn	; write it
		
L00E7           clrb			; flag no error
                lbra    Exit		; exit

L00EB           lda     <RecType	; get record type
                bne     L0102		; not type 1

                lda     #C$CR		; linefeed
                sta     ,-Y		; put in buffer
                lda     #$01		; stdout?
                ldy     #$00FF		; max bytes
                leax    <NameBuf,U	; point to namebuff
                os9     I$WritLn	; write it
		
                lbra    ReadNextChunk	; read next record

L0102           lda     <OutputID	; get output file id
                ldy     <DataLen	; length of data to write
                leax    <InputBuf,U	; point to buffer
                os9     I$Write		; write it
		
                lbhs    ReadNextChunk	; do next record

                lbra    Exit		

L0114           cmpb    #E$EOF		; end of input file ?
                beq     L00E7		; yes deal with it

                lbra    Exit		; otherwise exit with error

HexWord         bsr     HexByte		; get a byte convert to hex

                sta     <CurrentWord
HexByte         lda     ,X+		; get a byte from input
                bsr     HexNibble	; convert it to binary

                lsla			; shift it to top nibble
                lsla
                lsla
                lsla
                anda    #$F0		; mask bottom bits
                pshs    A		; temp save on stack
                lda     ,X+		; get next byte from input
                bsr     HexNibble	; convert to binary

                adda    ,S+		; add upper nibble
                sta     <CurrentByte	; save current byte
                adda    <Checksum	; add current checksum, ignoring overflow
		
                sta     <Checksum	; resave checksum
                lda     <CurrentByte	; recover current byte
                rts
		
HexNibble       suba    #'0'		; convert ASCII to binary
                bmi     L0149

                cmpa    #9		; less than 9?
                ble     L0148		; yep, valid

                suba    #$07		; convert A..F
                cmpa    #$0F		; too big?
                bhi     L0149		; yes, error, stop processing

L0148           rts			; return

L0149           leax    NonHex,PCR
                bra     L00DE

NonHex          FCC     "** NON-HEX CHARACTER ENCOUNTERED"
                fcb   C$BELL,C$CR

CkSumErr        FCC     "** CHECKSUM ERROR DETECTED"
                fcb   C$BELL,C$CR

L018D           FCC     "** NON-CONTIGUOUS ADDRESS SPACE DETECTED"
                fcb   C$BELL,C$CR

                emod
eom             equ     *
                end
