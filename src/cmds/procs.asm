                nam     Procs
                ttl     program         ; module

* Disassembled 1900/00/00 01:49:42 by Disasm v1.5 (C) 1988 by RML

                use     defsfile
                
tylg            set     Prgrm+Objct
atrv            set     ReEnt+rev
rev             set     $01

edition		set	10

                mod     eom,name,tylg,atrv,start,size

BUFFSIZE	equ	80

showall         rmb     1		; showall flag
ProcID          rmb     1		; process id
UserID          rmb     2		; user id
ZerosFlag       rmb     1		; zero supression flag for dec output
BufPtr          rmb     2		; buffer pointer
BlkMask         rmb     1
u0008           rmb     1
u0009           rmb     1
u000A           rmb     31
Buffer          rmb     BUFFSIZE	; buffer
SysBlockMap     rmb     1274		; system block map
size            equ     .

name            equ     *             
		fcs     /Procs/
                fcb     edition
		
Heading1	fcs	"   Parnt User                   Mem Stack"
Heading2	fcs	"ID  ID  Numbr Pty Age Sts Signl Siz  Ptr   Primary Module"
Heading3	fcs	"--- --- ---- ---- --- --- ----- --- ----- ----------------"


start           equ     *
                clr     <showall	; assume brief display
                lda     #$01
                sta     <ProcID
                
		lda     ,x+		; get char from command line
                eora    #'E'		; is it 'E'
                anda    #$DF		; mask it
                bne     L00BF		; no, skip
		
                inc     <showall	; flag full display
L00BF           leax    <Buffer,u	
                stx     <BufPtr
		
                leax    <SysBlockMap,u	; point at buffer for system block map
                os9     F$GBlkMp	; go get it
                tfr     a,b		; get high byte of block size into b
                nega			
                sta     <BlkMask
                lda     #$FE
		
L00D1           inca
                lsrb
                bne     L00D1
                
		sta     <u0008
                
		os9     F$ID		; get process / user id
                sty     <UserID		; save user id
		
                lbsr    WriteBuf	; output line, reset buffer
  
		leay    >Heading1,pcr	; output heading 1
                lbsr    StrYtoBuf	; put in buf
                lbsr    WriteBuf	; output line, reset buffer
		
                leay    >Heading2,pcr	; output heading 2
                bsr    	StrYtoBuf	; put in buf
                lbsr    WriteBuf	; output line, reset buffer
		
		leay    >Heading3,pcr	; output heading 3
                bsr    	StrYtoBuf	; put in buf
                bsr   	WriteBuf	; output line, reset buffer
		
L00FB           inc     <ProcID		; move to next process
                beq     ExitNoError	; if zero, all done
		
                lda     <ProcID		; get process id
                leax    <SysBlockMap,u	; point to buffer
                os9     F$GPrDsc	; Get process descriptor
		bcs     L00FB		; error, do next
		
                ldd     <UserID		; get user id	
                cmpd    P$User,x	; compare our ID to process' ID
                beq     L0114		; same as process, show it
		
                tst     <showall	; are we showing all?
                beq     L00FB		; no, loop for next
		
L0114           ldb     ,x		; Get process id
                bsr     DecOutB		; output it
		lbsr    OutSpace	; output a space
                
		ldb     P$PID,x		; get parent's process id
		bsr     DecOutB		; output it
		lbsr    OutSpace	; output a space
                
                ldd     P$User,x	; get process user id
                lbsr    OutDec		; output in decimal
                lbsr    OutSpace	; output a space
		
                ldb     P$Prior,x	; get process priority
                bsr     DecOutB		; output in decimal
                lbsr    OutSpace	; output a space
		
                ldb     P$Age,x		; get process age
                bsr     DecOutB		; output in decimal
                lbsr    OutSpace	; output a space
		
                lda     #'$'		; put a $ in buffer
                bsr     StoreAInBuf
		
                lda     P$State,x	; get process status
                lbsr    DoHexOut	; output it in hex
		
                clra			; clear msb
                ldb     <P$Signal,x	; get process signal code
                lbsr    OutDec		; output as decimal
                bsr     OutSpace	; output a space
                bsr     OutSpace	; output a space
		
                ldb     P$PagCnt,x	; get memory page count
                bsr     DecOutB		; output as decimal
                bsr     OutSpace	; output a space
		
                lda     #'$'		; put a $ in buffer
                bsr     StoreAInBuf

                lda     P$SP,x		; get process stack pointer MSB
                bsr     DoHexOut	; output in hex
                lda     P$SP+1,x	; LSB
                bsr     DoHexOut	; output in hex
                bsr     OutSpace	; output a space
		
                lbsr    GetModName	; get name of module pointer in Y
                bsr     StrYtoBuf	; transfer to buffer
                bsr     OutSpace	; output a space
                bsr     WriteBuf	; write it	
                bra     L00FB		; loop for next process
		
		
ExitNoError     clrb
                os9     F$Exit

StrYtoBuf       lda     ,y 		; get byte from message             
                anda    #$7F            ; reset bit 7
                bsr     StoreAInBuf     ; store it in buff      
                lda     ,y+             ; get next char from string
                bpl     StrYtoBuf       ; loop again if not end of string
                rts

WriteBuf       	pshs    y,x,a		; save regs
                lda     #C$CR		; Put a CR in buffer		
                bsr     StoreAInBuf
		
                leax    Buffer,u	; point at beginning of buffer
                stx     <BufPtr		; put in buffer pointer
                
		ldy     #BUFFSIZE	; Write the whole buffer
                lda     #$01		; standard out	
                os9     I$WritLn	; write it
                puls    pc,y,x,a	; restore and return	

; convert the byte at x to decimal and output it to the buffer.
; this code works thus :
; take byte and subtract 100 repeatedly till it underflows....to generate 100s digit
; then add 10 repeatedly till it is again > 0 to calculate the 10's digit
; the remainder is then the units.
;
DecOutB         clr     <ZerosFlag	; clear zeros flag so we don't output leading zeros
                lda     #$FF		; init 100s counter, compensate for inc below
L0194           inca			; increment hundreds counter
                subb    #100		; subtract 100 from byte
                bcc     L0194		; keep going until less than 0
		
                bsr     L01AA		; output hundreds
		
                lda     #$0A		; init 10's counter, compensate for immediate dec
L019D           deca			; dec 10's counter
                addb    #$0A		; add 10 to byte
                bcc     L019D		; if it's still < 0 keep going
		
                bsr     L01AA		; output tens
		
                tfr     b,a		; remainder is units
                adda    #'0'		; convert to ascii
                bra     StoreAInBuf	; save it in buffer

L01AA           tsta			; is A=0?
                beq     L01AF		; yes, skip test zero flag
                sta     <ZerosFlag	; no, turn off zero flag, so zeros now output
L01AF           tst     <ZerosFlag	; should we output a zero?
                bne     L01B5		; yse branch....
		
OutSpace        lda     #$F0		; when '0' is added becomes ' ', to supress zero
L01B5           adda    #'0'		; convert to ASCII

StoreAInBuf     pshs    x               ; save x
                ldx     <BufPtr       	; point x at buffer   
                sta     ,x+             ; save a in buffer
                stx     <BufPtr         ; save buffer pointer
                puls    pc,x            ; restore and return
		
DoHexOut        pshs    a		; save value to convert
                anda    #$F0		; mask of top nibble
                lsra			; and move it to bottom 4 bits
                lsra
                lsra
                lsra
                bsr     L01CF		; convert it to hex + put in buffer
		
                puls    a		; restore value to convert
                anda    #$0F		; mask off bottom 4 bits
L01CF           adda    #'0'		; convert to ASCII
                cmpa    #'9'		; is it > 9 ?
                bls     StoreAInBuf	; no, store it
		
                adda    #$07		; yes, convert to 'A'..'F'
                bra     StoreAInBuf	; and put in buffer
				
DecadeTab	fdb	10000,1000,100,10,1
		fcb	$ff
				
OutDec           pshs	y,x,b,a		; save regs
		leax    <DecadeTab,pcr	; point at table

; upper byte of y is thousands digit, bottom is space.....
; initialized at -1, so inc will make it 0		
                ldy     #$2F20

L01ED           leay    >$0100,y	; increment thousands
                subd    ,x		; subtract decade from x
                bcc     L01ED		; keep going till we overflow
		
                addd    ,x++		; add decade back to make it +ve
                pshs    d		; save d
                tfr     y,d		; transfer output to d
                tst     ,x		; end of table?
                bmi     L0215		; yes, skip
		
                ldy     #$2F30
                cmpd    #$3020
                bne     L020F
                ldy     #$2F20
                lda     #$20
L020F           bsr     StoreAInBuf
                puls    d
                bra     L01ED

L0215           bsr     StoreAInBuf
                leas    $02,s
                puls    pc,y,x,b,a
		
GetModName     	pshs    u,x		; save regs
                leay    <P$DATImg,x	

                tfr     y,d		; starting block number
                ldx     <P$PModul,x	; get primary module
                ldy     #$0009		; get 9 bytes
                leau    u0009,u		; buffer for copy
                os9     F$CpyMem	; get module header from taget process
                
		pshs    d		; save d
                ldd     M$Name,u	; get module name pointer	
                leax    d,x		; move x to point at it
                puls    d		; restore d
		
                ldy     #$0020		; byte count
                os9     F$CpyMem	; go get name from target process
                leay    ,u		; point at name
                lda     <$1F,y		; get last charcter of name
                ora     #$80		; terminate name by setting MS bit of last byte
                sta     <$1F,y		; put character back	
                puls    pc,u,x		; restore and return
		
                emod
eom             equ     *
                end
