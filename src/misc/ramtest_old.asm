                nam     OS-9            ; Level II V1.2
                ttl     Module          ; Header
************************************************************
*
*     Module Header
*
                use     defsfile        
                                        
                                        
                                      
*
*     Routine Cold
*
*   System Coldstart; determines system state, initializes
* system data structures, searches for the remainder of the
* system and attempts to start operation.
*
LoRAM           set     $20             ; ; set low RAM limit
HiRAM           set     DAT.Blsz*RAMCount 

UseSRAM		equ	0		; use SRAM on expansion card for stack / vars
UserSerial	equ	0		; Should we echo to serial

ScreenBase	equ	$6000		; base of screen ram
ScreenRows	equ	25		; 25 rows
ScreenCols	equ	40		; 40 column screen
BytesPerChar	equ	2		; 2 bytes / char
ScreenBytesLine	equ	ScreenCols*BytesPerChar	; each character + attribute

ScreenEnd	equ	ScreenBase+(ScreenRows*ScreenBytesLine)

TestDATPage	equ	$07		; Page to use to test
PageSize	equ	$1000		; Page size
TestBase	equ	(TestDATPage*PageSize)	; Base of paged in memory
DATPage		equ	DAT.Regs+TestDATPage	; Dat register to change
MaxPossiblePage	equ	$BF		; max RAM supported
VidPage		equ	$1f		; video & stack in this paged on't test it!

		ifne	UseSRAM
VarPage		equ	$05		; Var pageno
StackPage	equ	$05		; stack page
SRAMPage	equ	$40		; first page of SRAM
		else
VarPage		equ	$06		; Var pageno
StackPage	equ	$06		; stack page	
		endc

StackOffset	equ	$990		; Offset of stack
StackBase	equ	(StackPage*PageSize)+StackOffset	; address of bottom of stack

VarOffset	equ	$0800		; offset into var page of vars
VarBase		equ	(VarPage*PageSize)+VarOffset	; address of vars

VidStackBase	equ	$6f00		; emergency stack on video page

	org	VarBase			; ram screen vars
ScreenPos	rmd	1		; pos on screen
SaveDAT		rmb	1		; saved page
CurrentPage	rmb	1		; current page under test
DirFlag		rmb	1		; Direction flag ==0 : up <>0 : down
FailPage	rmb	1		; Page of failure
FailOffset	rmd	1		; Offset of failure	
FailFlag	rmb	1		; test failed flag, 0=passed, 1=failure
MaxPage		rmb	1		; max page found
SaveScreenPos	rmd	1		; Saved screen position e.g. to output pageno                
FailWrote	rmb	1		; When a failure happens value that was written
FailRead	rmb	1		; When a failure happens value that was read
                     
	org	$c000			; main code
	zmb	$f000-*

; Output hex byte in a	
OutHexByte
	pshs		x,d		; save regs
	ldx		ScreenPos	; get screen position
	bsr		HexByte		; convert A->hex in D
	sta		,x++		; output it
	stb		,x++
	stx		ScreenPos	; update screen pointer
	puls		x,d,pc		; restore & return

; Output hex word in d
OutHexWord
	pshs		x,d		; save regs
	ldx		ScreenPos	; get screen position
	pshs		b		; save lsb
	bsr		HexByte		; convert A->hex in D
	sta		,x++		; output it
	stb		,x++
	puls		a		; restore LSB
	bsr		HexByte		; convert A->hex in D
	sta		,x++		; output it
	stb		,x++	
	stx		ScreenPos
	puls		x,d,pc		; restore & return

; in : A=byte to conv, 	
; out: D=ascii A=MSN, B=LSN

HexByte
	pshs	x			; save x
	pshs	a			; save byte to convert
	leax	HexTable,pcr		; point x at table
	anda	#$0F			; select just lower nibble
	ldb	a,x			; get hex char of LSN into b
	puls	a			; recover value to convert	
	lsra				; move MSN to LSN
	lsra
	lsra
	lsra
	anda	#$0F			; mask it
	lda	a,x			; get hex char of MSN
	puls	x,pc			; restore and return
	
HexTable
	FCC	"0123456789ABCDEF"
	
Home
	pshs	d			; save d
	ldd	#0			; 0,0
	bsr	GotoXY			; go there
	puls	d,pc			; restore and return
	
; x co-ordinate in a, y in b, updates ScreenPos	
GotoXY	pshs	d,x			; save regs			
	pshs	a			; save column no
	ldx	#ScreenBase		; point at screen base
	lda	#ScreenBytesLine	; 80 bytes / line in 40 col mode
	mul				; workout offset of beinning of line
	
	leax	d,x			; add row offset
	puls	a			; restore column no
	lsla				; multiply by 2 because of attributes
	leax	a,x			; calculate final destination
	stx	ScreenPos
 	puls	d,x,pc			; restore & return

; Write str pointed to by x, at co-ordinates in d
WriteStrAt
	pshs	x,d			; save d
	ldd	,x++			; load from string
	bsr	GotoXY			; go to coordinates
	bsr	WriteStr		; write the string
	puls	x,d,pc			; restore & return

WriteStrAtPos
	pshs	x
	bsr	WriteStrAt		; write the string 
	
	ldx	ScreenPos		; save current screen pos
	stx	SaveScreenPos
	puls	x,pc

; Write zero terminated string pointed to by X, updates x
WriteStr	
	pshs	d
	ldy	ScreenPos		; get current screen position
WriteStrLoop
	lda	,x+			; get a character to write
	beq	WriteStrEnd		; zero : exit
	
	bsr	WriteChar		; Write it
	bra	WriteStrLoop		; do next
	
WriteStrEnd
	sty	ScreenPos		; save updated screen position
	puls	d,pc			; restore & return

; Write a charcter in a at screen address in y, looping at end of screen.
WriteChar
	cmpy	#ScreenEnd		; past end of screen ?
	blo	WriteChar1		; no : just write it
	
	ldy	#ScreenBase		; otherwise wrap
WriteChar1
	sta	,y++			; write char & skip attribute
	rts

	ifeq	1
;
; Serial routines using onboard 6551.
;	
	
SerInit
	rts
	
SerWriteChar
	rts

; Output hex byte in a	
SerOutHexByte
	pshs		x,d		; save regs
	bsr		HexByte		; convert A->hex in D
	bsr		SerWriteChar	; write MSB
	exg		a,b		; get LSB
	bsr		SerWriteChar	; and write it
	puls		d,pc		; restore & return

; Output hex word in d
SerOutHexWord
	pshs		x,d		; save regs
	pshs		b		; save lsb
	bsr		HexByte		; convert A->hex in D
	sta		,x++		; output it
	stb		,x++
	puls		a		; restore LSB
	bsr		HexByte		; convert A->hex in D
	puls		x,d,pc		; restore & return

; Write str pointed to by x, at co-ordinates in d
; Takes same parameter as WriteStrAt, but ignores the co-ordinates	
SerWriteStrAt
	pshs		x		; save x
	leax		2,x		; skip co-ordinates	
	bsr		SerWriteStr	; write it
	puls		x,pc
	
SerWriteStr
	lda		,x+		; get next char
	beq		SerWriteStrEnd
	bsr		SerWriteChar	; write it
	bra		SerWriteStr	; do next
SerWriteStrEnd
	puls		x,pc
	endc

;
; Determine installed RAM, exits with page count in MaxPage
;
SizeRam		clr	MaxPage		; assume no ram

		leax	search,pcr	; point to message
		lbsr	WriteStrAtPos	; write it
		
TestPageExists	ldy	SaveScreenPos	; restore screen location
		sty	ScreenPos	

		lda	MaxPage		; get current page
		sta	DATPage		; save in DAT

		lbsr	OutHexByte	; display page being probed

		lda	TestBase	; save byte of page
		pshs	a
		
		clra
		sta	TestBase	; save it
		cmpa	TestBase	; is it the same?
		bne	TestFail	; Non zero reached end of RAM
		
		lda	#$55		; test value
		sta	TestBase	; save it
		cmpa	TestBase	; is it the same?
		bne	TestFail	; Non zero reached end of RAM
		
		lda	#$AA		; test value
		sta	TestBase	; save it
		cmpa	TestBase	; is it the same?
		bne	TestFail	; Non zero reached end of RAM

		puls	a		; restore value
		sta	TestBase	
		
		inc	MaxPage		; do next page
		lda	MaxPage		; test for max allowable page?
		cmpa	#MaxPossiblePage ; still valid?
		blo	TestPageExists
		rts
		
TestFail	puls	a		; restore value from stack
		sta	TestBase	
		
		rts
		
;
; Page test, test for corruption between pages, may be able to detect faults in
; page selection logic.
;		

PageTest	clr	FailFlag	; flag success

		clr	DirFlag		; test up
		bsr	DoPageTest	; do it

		tst	FailFlag	; failed?
		bne	PageTestEnd	; yep : bail

		com	DirFlag		; do down test
		bsr	DoPageTest	; do it
		
PageTestEnd	rts

DoPageTest	bsr	SetStartPage	; set starting page

		leax	Filling,pcr	; Print filling.... string
		lbsr	WriteStrAtPos	; write to screen
		
FillNext	ldy	SaveScreenPos	; restore screen location
		sty	ScreenPos	

		lda	CurrentPage	; get current page
		sta	DATPage		; save in DAT
		
		lbsr	OutHexByte	; output page number

		bsr	FillPage	; fill page with a
		
		lbsr	TryNextPage	; increment / check page
		bne	FillNext	; still pages to go fill them
		
		leax	TestingMess,pcr	; output message
		lbsr	WriteStrAtPos	; write to screen
		
		bsr	SetStartPage 	; start at page 0
		
TestNextPage	ldy	SaveScreenPos	; restore screen location
		sty	ScreenPos	

		lda	CurrentPage	; get current page
		sta	DATPage		; save in DAT
		
		lbsr	OutHexByte	; output page number
		bsr	TestPage	; test page
		bne	DoPageTestEnd	; error, exit
		
		lbsr	TryNextPage	; increment / check page
		bne	TestNextPage	; still pages to go fill them
		
DoPageTestEnd	rts

;
; Fill test page with value in a
;
FillPage	ldx	#TestBase	; point at page
		
FillPageLoop	sta	,x+		; save page number in page
		cmpx	#TestBase+PageSize	; end of this page?
		bne	FillPageLoop	; nope keep filling
		rts
		
		
;
; Test page against value in a, exits either with x=failed value & FailFlag set.
; or x=end of page & FailFlag clear.
;		
TestPage	ldx	#TestBase	; point at page

TestPageLoop	cmpa	,x		; Is value in page the same?
		bne	PageTestFail	; Nope : fail
		leax	1,x		; increment pointer
		cmpx	#TestBase+PageSize	; end of this page?
		bne	TestPageLoop	; nope keep filling
		rts
		
PageTestFail	stx	FailOffset	; save offset
		com	FailFlag	; flag we failed
		sta	FailWrote	; Save byte written
		ldb	,x		; Save byte read
		stb	FailRead
		
		stx	FailOffset	; save offset of fail
		sta	FailPage
		tst	FailFlag	; let caller know!
		rts

; Set starting page dependent on DirFlag
SetStartPage	tst	DirFlag		; are we doing up or down?
		bne	StartPageDown
		clr	CurrentPage	; testing up, start on page 0
		rts
		
StartPageDown	lda	MaxPage		; set start page
		deca
		sta	CurrentPage			
		rts
		
;
; Test RAM based on number of pages we found above.....
;
		
TestRAM		clr	CurrentPage	; start at page 0
		clr	FailFlag	; flag success
		
NextPage	ldy	SaveScreenPos	; restore screen location
		sty	ScreenPos	
	
		lda	CurrentPage	; get current page
		sta	DATPage		; save in DAT
		
		lbsr	OutHexByte	; display number of page under test
		
		lda	TestBase	; save byte of page

		ldx	#TestBase	; point to page under test

NextByte	ldy	SaveScreenPos	; restore screen location
		leay	6,y
		sty	ScreenPos	

		tfr	x,d		; output offset
		lbsr	OutHexWord
		
		clra			; start at zero
TestByteLoop	sta	,x		; save it in RAM
		cmpa	,x		; is it the same?
		bne	RamTestFail	; nope : fail it
		
		inca			; test next value
		bne	TestByteLoop	

		leax	1,x		; move to next byte
		
		cmpx	#TestBase+PageSize	; end of this page?
		blo	NextByte	; no do next byte

		bsr	TryNextPage	; increment to next page
		bne	NextPage	; no : continue testing
		
		rts
		
RamTestFail	
		stx	FailOffset	; save offset
		lda	CurrentPage	; and Page no
		sta	FailPage
		
		com	FailFlag	; flag failed
		rts

TryNextPage	tst	DirFlag		; are we going up or down?
		bne	TrynextPageDown	; going down do it
		
; Increment CurrentPageNo, skip VidPage, and return cc.z=1 when MaxPage reached
TryNextPageUp		
		inc	CurrentPage	; next page
		lda	CurrentPage	
		
		cmpa	#VidPage	; Are we on video / stack page?
		beq	TryNextPageUp	; yes: skip it and try next
		
		cmpa	MaxPage		; Have we done all?
		rts
		
; Decrement CurrentPageNo, skip VidPage, and return cc.z=1 when page underflows reached
TryNextPageDown		
		dec	CurrentPage	; next page
		lda	CurrentPage	
		
		cmpa	#VidPage	; Are we on video / stack page?
		beq	TryNextPageDown	; yes: skip it and try next
		
		cmpa	#0		; Have we done all?
		rts
***********************************************************
*
*     Routine DATInit
*
*   Initialize DAT for RAM in block zero and this
* ROM in block fifteen.   This code MUST reside in
* the upper 256 bytes of the ROM.
*

                ifeq    CPUType-PAL1M92 
                endc                    


                ifeq    CPUType-DRG128  
DATInit                                 
                clra                    
                tfr     a,dp            ; ; set direct page
                ldx     #DAT.Task       ; ; point at task reg pia
                                        

                ifeq    TEST            
                lda     1,X             ; ; am I the DMA processor
                lbne    beDMAC          ; ; ..no

                endc                    

                                        
                ldb     #4              
                stb     1,X             ; ; access port A
                lda     #$F0            
                sta     ,X              ; ; select task 0 (system task)
                clra    ;               ; access DDRA
                sta     1,X             ; ; Select data direction register
                lda     #%11001111      ; ; Set direction: output=1, input=0
                sta     ,x             ; ;
                lda     #$FF            
                sta     2,X             
                lda     #%00111110      ; ; select output register et al
                sta     1,X             ; ; Set value in control register A
                stb     3,X             ; ; Set value in control register B
                lda     #%11011000      
                sta     2,X             
*
* Initialize all tasks in DAT registers
*
                ldy     #DAT.Regs       
                ldb     #$F0            
DATIn1                                  
                stb     ,X              
                clra                    
                sta     ,y             ; ; assume RAM in block 0
                lda     #$1F            
                sta     $06,Y           ; ; Graphics at $6000
                lda     #ROMBlock       ; ; get ROM block number
                sta     14,y            ; ; set ROM block
                lda     #IOBlock        ; ; get I/O Block number
                sta     15,y            ; ; set I/O block
                incb                    ; ; next map
                bne     DATIn1          ; ; ..until all done

                                        
                lda     #$F0            ; ; set task 0
                sta     ,X              
                                        
* Initialise known on-board multi-purpose I/O
InitIO                                  
                ldx     #A.Mouse        ; ; start with mouse pia
                                        

                lda     #$7F            ; ; now enable DMA processor

                                        
                ldb     #4              
                stb     1,X             ; ; access port A
                sta     ,X              
                stb     3,X             ; ; access PRB
                lda     #B.MPHalt       ; ; leave MP HALT high
                sta     2,X             
                clra                    
                sta     3,X             ; ; access DDRB
                                        
                lda     #$83            
                sta     2,X             
                stb     3,X             
                lda     #2              
                                        
                sta     1,X             ; ; access DDRA
;                lda     #$FF            ; ; all outputs
                lda     #$7F            ; ; leave DMA disabled
                sta     ,X              ; ; - enables DMA processor HERE
                stb     1,X             ; ; access PRA
                ldx     #A.KBD          ; ; point at keyboard port
                lda     #$18            
                sta     ,x             
                stb     1,X             ; ; access PRB
                                        
                ldx     #A.Crtc         ; ; point at CRTC
                clrb    ;               ; start with register 0
                leay    InitCrtc,pcr    ; ; point at table
InitI01                                 
                lda     ,y+             ; ; get a byte
                stb     ,X              ; ; set register number
                sta     1,X             ; ; set register data
                incb                    ; ; next register
                cmpb    #CRTCSIZ        ; ; all done?
                bcs     InitI01         ; ; ..no

                                        
                lda     #$B0            ; ; enable mapper
                sta     DAT.Task        
                ldx     #$6000          ; ; clear out text screen
                ldd     #$2008          ; ; space + attribute
InitI03                                 
                std     ,X++            ; ; set attributes
                cmpx    #$7000          
                bne     InitI03         

                                        
InitI05         
		lds	#StackBase	; bottom of stack

		ifne	UseSRAM		
		lda	#SRAMPage	; Page of sram to use
		sta	DAT.Regs+VarPage	; page it in
		
		lda	#$55		; quick test
		sta	VarBase
		cmpa	VarBase		
		bne	SRAMFail	; not same, fail!	
		
		coma			; test other bits
		sta	VarBase
		cmpa	VarBase
		bne	SRAMFail	; not same, fail!
		
		clr	VarBase
		bra	StartTests	; All OK, start testing....
		
SRAMFail	lds	#VidStackBase	; move sp
		leax	SRAMFailMsg,pcr	; point to message
		ldy	#ScreenBase	; Point to screen
SRAMFailLoop	lda	,x+		; get a byte
		lbeq	Forever		; Zero, end of string loop forever
		sta	,y++		; store & skip
		bra	SRAMFailLoop
		
		endc	


StartTests	leax	RamTestMess,pcr	; point at test message
		lbsr	WriteStrAt	; WriteIt
		
		lbsr	SizeRAM		; go find out how many pages we have
		
		leax	RamSize,pcr	; found size message
		lbsr	WriteStrAt	; WriteIt

		lda	MaxPage		; get Max page found
		lbsr	OutHexByte	; output it

		leax	RamPages,pcr	; point to message
		lbsr	WriteStr	; write it
		
		leax	TestingMess,pcr	; point to message
		lbsr	WriteStrAt	; WriteIt
		
		ldx	ScreenPos	; record location for page number to output
		stx	SaveScreenPos	
		
		lbsr	PageTest	; Test ram paging
		
		tst	FailFlag	; did it fail?
		beq	MainTest	; no, no message	
		
		bsr	DisplayFailPage	; display message
		
		
MainTest	leax	TestingMess,pcr	; point to message
		lbsr	WriteStrAt	; WriteIt

		lbsr	TestRAM		; test the ram
		
		tst	FailFlag	; Did we fail?
		bne	DisplayFail	; Yes : show results
		
		leax	PassMess,pcr	; point to message
		lbsr	WriteStrAt	; WriteIt
		
		bra	EndTest		; goto end

DisplayFailPage
		leax	FailPageMess,pcr ; point to message
		lbsr	WriteStrAt	; WriteIt	
	
		lda	FailPage	; get page we failed at
		lbsr	OutHexByte	; output it

		leax	FailMess2,pcr	; point to message
		lbsr	WriteStrAt	; WriteIt	

		bra	FailMain

DisplayFail	leax	FailMess,pcr	; point to message
		lbsr	WriteStrAt	; WriteIt	
		
		lda	FailPage	; get page we failed at
		lbsr	OutHexByte	; output it
		
		leax	FailMess2,pcr	; point to message
		lbsr	WriteStrAt	; WriteIt	
		
FailMain	ldd	FailOffset	; get fail address
		subd	TestBase	; work out offset
		lbsr	OutHexWord	; output it
		
		leax	WroteMess,pcr	; Display message
		lbsr	WriteStr	
		
		lda	FailWrote	; get value written
		lbsr	OutHexByte
		
		leax	ReadMess,pcr	; Display message
		lbsr	WriteStr	
		
		lda	FailRead	; get value written
		lbsr	OutHexByte
		rts
		
EndTest
		leax	AgainMess,pcr	; point to message
		lbsr	WriteStrAt	; WriteIt
		
Forever		bra	Forever		; loop until reset
		
                lbra    Cold            ; ; go to cold start

                                        
InitCrtc                                
                fcb     55              ; ; Horizontal total (R0)
                fcb     40              ; ; Horizontal characters displayed (R1)
                fcb     $2E             ; ; Horizontal sync position
                fcb     $35             ; ; Sync width
                fcb     30              ; ; Vertical total (R4)
                fcb     2               ; ; Vertical total adjust (R5)
                fcb     25              ; ; Vertical displayed (R6)
                fcb     $1B             ; ; Vertical sync (R7)
                fcb     $50             ; ; Interlace mode (0) and skew (R8)
                fcb     $09             ; ; Maximum scan line (R9)
                fcb     $20             ; ; Cursor start line + blink (R10)
                fcb     $09             ; ; Last scan line of cursor (R11)
                fcb     $38             ; ; Start address register (R12)
                fcb     $00             ; ; Start address register (R13)
                fcb     $38             ; ; Cursor register (R14)
                fcb     $00             ; ; Cursor register (R15)
CRTCSIZ         equ     *-InitCrtc      
                                        
;			 0123456789012345678901234567890123456789
RamTestMess	FCB	0,0
		FCZ	'Dragon Beta RAM test 2022-03-23, Ramoth'

Search		FCB	0,1
		FCZ	'Search : '
		
RamSize		FCB	20,1
		FCZ	'Found :'

RamPages	FCZ	' Pages'

Filling		FCB	0,3
		FCZ	'Filling page : '
		
TestingMess	FCB	0,3
		FCZ	'Testing page : '

PassMess	FCB	0,14
		FCZ	'Good news, all RAM tests passed'

FailMess	FCB	0,5
		FCZ	'RAM test failed at page : '

FailPageMess	FCB	0,5
		FCZ	'RAM Page test failed at page : '		

;			 0123456789012345678901234567890123456789
FailMess2	FCB	0,6
		FCZ	'Offset : ' 

TestingMess2	FCB	0,8
		FCZ	'Testing RAM : '

FailMess3	FCB	0,9
		FCZ	'Offset : ' 

WroteMess	FCZ	' wrote: '

ReadMess	FCZ	' read: '

AgainMess	FCB	0,15
		FCZ	'Reset machine to test again!'
		
SRAMFailMsg	
		FCZ	'Error SRAM failed / not found!'
*
* DMA Processor routine
*
beDMAC                                  
                lds     #$FFFF          ; ; load stack, allow NMIs
                sync                    ; ; wait for it ..
DMACNMI                                 
                lds     #$FFFF          ; ; get ready for action
                ldx     D.DMPort        ; ; get port address
                ldu     D.DMMem         ; ; memory address
                ldb     D.DMDir         ; ; DMA direction 1=Read Memory
                bne     DMACRead        ; ; ..read

                                        
* Copy from Port to Memory
* Breaks out of loop if interrupt is longer than 3 cycles
DMACWrit                                
                sync                    ; ; wait for FIRQ
                lda     ,X              ; ; get byte from port
                sta     ,u+             ; ; put in memory
                bra     DMACWrit        ; ; ad nauseam

                                        
DMACRead                                
                lda     ,u+             ; ; get byte from memory
                sync                    ; ; wait for FIRQ
                sta     ,X              ; ; put in port
                bra     DMACRead        

                endc                    

                      
cold		bra	cold                  

                                    
                                        
		
***********************************************************
*
*     Interrupt Vector Routines
*
*   Set interrupt masks, of necessary, set system memory,
*   and jump through direct page psuedo-vector
*
* Data: none
*
* Calls: [D.SWI3], [D.SWI2], [D.FIRQ], [D.IRQ], [D.SWI], [D.NMI]
*
                                        
SWI3RQ            
		rti
                orcc    #IntMasks       ; ; set interrupt masks
                ldb     #D.SWI3         ; ; get direct page offset
                bra     Switch          

                                        
SWI2RQ          
		rti
                orcc    #IntMasks       ; ; set interrupt masks
                ldb     #D.SWI2         ; ; get direct page offset
                bra     Switch          

                                        
FIRQ                                    
		rti
                ldb     #D.FIRQ         ; ; get direct page offset
                bra     Switch          

                                        
IRQ                                     
		rti
                orcc    #IntMasks       ; ; set fast interrupt masks
                ldb     #D.IRQ          ; ; get direct page offset
                                        
Switch          equ     *               
                                        

                ifeq    DAT.Uniq        
                else                    
                lda     #SysTask        ; ; get system task number
                endc                    

                                        
                sta     DAT.Task        ; ; set system memory
Switch10                                
                clra                    
                tfr     a,dp            ; ; clear direct page register
                tfr     d,x             ; ; copy direct page ptr
                jmp     [,x]           	; ; go through vector

                                        
SWIRQ                                   
		rti
                ldb     #D.SWI          ; ; get direct page offset
                bra     Switch          

                                        
NMIRQ                                   
		rti
                ldb     #D.NMI          ; ; get direct page offset
                bra     Switch10        
                                       
                                        
offset          zmb     $FFF0-*         
                                                        
***********************************************************
*
*     System Interrupt Vectors
*
                fdb     offset          
                fdb     SWI3RQ   
                fdb     SWI2RQ   
                fdb     FIRQ     
                fdb     IRQ      
                fdb     SWIRQ    
                fdb     NMIRQ    
                fdb     DATInit  
                                        
ROMEnd          equ     *               
                                        
                end                     
