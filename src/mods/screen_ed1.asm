                nam     screen          
                ttl     os9             ; device driver

                use     defsfile        

tylg            set     Drivr+Objct     
atrv            set     ReEnt+rev       
rev             set     $01             
                mod     eom,name,tylg,atrv,start,size 
                                        
*****************************************
*
*Device driver for the screen and keyboard
*on the Dragon 128
*
* Written by Paul Dayan of Vivaway Limited
*         7th November, 1983
*
*****************************************
*********
*Constant definitions
*

C.MAXLIN	equ	25		; lines per page
C.MCOL40	equ	40		; chars per line in 40 col mode
C.MCOL80	equ	80		; chars per line in 80 col mode
C.CUROFS	equ	$20		; cursor addressing offset
C.PAGSIZ	equ	4096		; size of text ram page block
C.SPACE		equ	$20		; space character
C.DEL		equ	$7F		; DEL character
C.MTCOL		equ	7		; highest text colour
C.FCOL		equ	2		; default foreground colour
C.BCOL		equ	0		; default background colour
                                        
*colours:
* 0 black
* 1 red
* 2 green
* 3 yellow
* 4 blue
* 5 magenta
* 6 cyan (pale blue/green)
* 7 white

C.ULBIT		equ	$40		; underline bit in attribute
C.FLBIT		equ	$80		; flash bit in attribute
C.ESCCNT	equ	1		; value of escape flag on first entry
C.SHIFT		equ	$01		; bit in specials for shift
C.CNTRL		equ	$10		; for control
C.FUNCT		equ	$02		; for function ('code')
K.SPECL		equ	0		; 'specials' keyboard reg number
K.Wide		equ	10		; width of keyboard matrix
C$ESC		equ	$1B		; escape
C.LtPen		equ	8		; light pen circuitry delay
C.BCyc		equ	20		; half cycles of beeper
C.BPer		equ	100		; half period of beeper
C.Rows		equ	10		; scan rows per character                                    
BufSiz          equ     100             ; Keyboard input buffer size
C.INSIZ		equ	12		; Bytes in CRTC initialization table
C.IRPT		equ	50		; initial repeat delay
C.RPT		equ	5		; subsiquent repeat delay
C.CAPL		equ	$FF		; 'toggle caps lock' key
C.SWITCH	equ	$FE		; 'switch displays' key                           
C.DebLP		equ	4		; debounce count for light pen
C$Lead		equ	$1D		; lead-in character for light pen sequence
C$LtPen		equ	'A'		; code char for light pen sequence
C$Mouse		equ	'B'		; B and C for mouse switches
NOGRAFIX	equ	0		; include graphics routines

		use 	gfxstat
				
V.CLine         rmb     1		; current text line               
V.Column        rmb     1               ; current text column
V.Cursor        rmb     2               ; current cursor pos
V.MaxLine       rmb     1               ; lines per page
V.MaxCol        rmb     1               ; columns per line
V.Fore          rmb     1               ; foreground colour
V.Back          rmb     1               ; background colour
V.RFore         rmb     1               ; foreground colour when in reverse field
V.RBack         rmb     1               ; background colour when in reverse field
V.Rev           rmb     1               ; reverse field flag
V.ULine         rmb     1               ; underline flag / mask
V.Flash         rmb     1               ; flash flag / mask
V.Attribute     rmb     1           	; current attribute byte    
V.CurAdd        rmb     2               ; cursor address in ram, used by print
V.Start         rmb     2               ; current logical start address
V.ScrSize       rmb     3               ; characters per screen
V.Mode40        rmb     1               ; 40 column mode flag
V.FunPad        rmb     1               ; function key pad flag
V.NCur          rmb     1		; new cursor set flag               
V.NStart        rmb     1               ; new start address flag
V.PhysAdd       rmb     3               ; physical address of RAM
V.Block         rmb     2               ; physical memory block number
V.REGS          rmb     K.Wide+1 	; keyboard 'registers'             
V.TREGS         rmb     K.Wide+1       	; debounced 	      
V.SRegs         rmb     K.Wide+1	; before debounce
V.BRegs         rmb     K.Wide          ; key conflict flags 
V.InvKey        rmb     1               ; 'no repeatable key' flag
V.DeBnce        rmb     1               ; debounce counter
V.RPT           rmb     1               ; key repeat counter
V.Link          rmb     2               ; entry point of the graphics subroutine module
V.GfxMod        rmb     2               ; address of the graphics subroutine module
V.LtPen         rmb     1               ; light pen signal flag
V.LtPenAd       rmb     2               ; light pen address
V.LPRep         rmb     1               ; light pen repeat counter
V.LPDeb         rmb     1               ; light pen debounce counter
V.LtPenY        rmb     1               ; light pen Y address
V.LtPenX        rmb     1               ; light pen X address
V.MouseB        rmb     1               ; May be something to do with mouse buttons ???? port selec flag
V.MouseX        rmb     2               ; Mouse X co-ordinate
V.MouseY        rmb     2               ; Mouse Y co-ordinate
V.ChrS1         rmb     1               ; charset 1 / 3 selected
V.ChrS2         rmb     1               ; charset 2 selected
V.CurType       rmb     1               ; cursor type
V.InCnt         rmb     1  		; input buffer byte count             
SigPrc          rmb     2               ; process waiting for data ready signal and code
V.NxtI          rmb     1               ; next in (keyboard buffer) pointer
V.Nxt0          rmb     1               ; next out (keyboard buffer) pointer
V.Intens        rmb     2               ; intensity flag
V.AKey          rmb     1               ; key value
		rmb	4
V.Shift         rmb     1		; shift flag               
V.Cntrl         rmb     1               ; control flag
V.Func          rmb     1               ; function flag
V.Key           rmb     1               ; key code
V.CAPL          rmb     1               ; caps lock flag
V.Active        rmb     1               ; write routine active flag
V.Switch        rmb     1               ; switch displays request
V.InBuf         rmb     BufSiz		; keyboard input buffer             
size            equ     .               
                fcb     $03             

name            equ     *               
                fcs     /screen/        
                fcb     $01             
                                        
start           equ     *               
                lbra    Init            
                lbra    Read            
                lbra    Write           
                lbra    GetStat         
                lbra    SetStat         
                lbra    Term            

*
* Mouse interrupt polling packet
*
MsPoll          fcb     $00,$c0,$80     
                                                                                
* Init
*
* Entry:
*    Y  = address of device descriptor
*    U  = address of device memory area
*
* Exit:
*    CC = carry set on error
*    B  = error code
*
                                        
Init            ldb     #1              ; reserve 1 page
                os9     F$Gmap          ; F$Gmap
                lbcs    InitExit        ; Error : exit
                
		stx     >V.Block,u   	; save block number     
                
		lbsr    T.MapIn         ; map in text blocks  
                bcs     InitExit        ; error, exit
                
		lda     >V.Block+1,u   	; make physical address     
                lsla                    
                lsla                    
                lsla                    
                sta     >V.PhysAdd+2,u 	; save physical address       
                anda    #$3F            
                sta     >V.PhysAdd,u        
		
                inc     >V.Mode40,u 	; start in 40 col mode       
                lda     #$60         	; initialise cursor type   
                sta     >V.CurType,u    
    
                lda     #C.FCOL   	; set foreground colour         
                lbsr    T.SelF1           
                lda     #C.BCOL         ; set background colour
                lbsr    T.SelB1           
                                        
                ldd     #A.Mouse+1      ; Mouse control reg
                leay    >IRQHandler,pcr ; IRQ Service address
                leax    >MsPoll,pcr    	; poll packet  
                os9     F$IRQ           ; setup poll
                                        
                bcs     InitExit        ; on error, exit
   
                ldx     #A.Mouse	; now enable interrupts (from PIA)          
                lda     #$1F            ; enable both Cx1 and Cx2, positive edge
                sta     1,x           	; set them

                ldx     #A.Kbd		; point at keyboard port          
                leay    >V.SRegs,u      ; point at keyboard registers
                lbsr    T.GetRGS        ; setup keyboard
                lbsr    T.ResPag      	; setup CRTC, clear screen     
                leax    >T.Tick,pcr   	; install timer routine   
                os9     F$Timer         
InitExit        rts                     
                                        
* GetStat
*
* Entry:
*    A  = function code
*    Y  = address of path descriptor
*    U  = address of device memory area
*
* Exit:
*    CC = carry set on error
*    B  = error code
*
                                        
GetStat                                 
                ldx     PD.RGS,y	; get pointer to callers regs 
                cmpa    #SS.Ready       ; Check keys ready?  
                bne     ErrNotRdy           ; no skip
	
                ldb     >V.InCnt,u      ; get input character count
                beq     L009D           ; branch if zero, not ready
                stb     R$B,x           ; save in callers B	
ExitNoErr       clrb                    ; flag no error
                rts                     ; return
		
L009D           comb                    
                ldb     #E$NotRdy	; return not ready           
                rts                     
		
ErrNotRdy       cmpa    #SS.EOF		; end of file?            
                beq     ExitNoErr           ; yes, do it
		
                cmpa    #SS.Mouse	; get mouse position            
                bne     L00BD           ; no skip

; Function SS.Mouse returns :
; 	X = Mouse x co-ordinate, signed 16 bit
; 	Y = Mouse y co-ordinate, signed 16 bit
;	A = Button status 
		
                ldd     >V.MouseX,u   	; get mouse X pos     
                std     R$X,x           ; return in caller's X
                ldd     >V.MouseY,u	; get mouse Y pos
                std     R$Y,x           ; return in caller's Y
                lda     >V.MouseB,u	; get mouse buttons        
                sta     R$A,x           ; return in caller's A
                clrb                   	; no error 
                rts                     

; Function SS.Size on stdin, returns the current graphic mode :
; Y = MSB = 0 lower 64K page only
;         = 1 both pages
;     LSB = 0 graphics not active
;         = 1 320x256 mode
;         = 2 640x512 mode
; X = number of block in lower page
		
L00BD           cmpa    #SS.Size	; get page size            
                bne     Unknown         ; no error, exit
		
                lda     <Bytes,u        ; get on/two page flag
                ldb     <Resol,u        ; get display type (0=no memory)
                lsrb                    ; make it 0=none, 1=320x256, 1=640x512
                std     R$Y,x           ; return it to user
                ldb     >MemStrt,u    	; make up block numbber
                addb    >CGPage,u       ; base+current page
                lsrb                    ; shift to block number
                lsrb                    
                lsrb                    
                clra                    ; 16 bit 
                std     R$X,x           ; return to user
                rts        
             
Unknown         comb                    ; flag error
                ldb     #E$UnkSvc	; unknown service            
                rts                     
                                        
* SetStat
*
* Entry:
*    A  = function code
*    Y  = address of path descriptor
*    U  = address of device memory area
*
* Exit:
*    CC = carry set on error
*    B  = error code
*
                                        
SetStat                                 
                cmpa    #SS.SSig        ; send signal?
                bne     Puts.A		; no, skip   		
        
                lda     PD.CPR,y       	; get process id    
                ldx     PD.RGS,y        ; get pointer to caller's regs in y
                ldb     R$X+1,x		; get signal code
           
                pshs    cc              ; save masks
                orcc    #IntMasks 	; disable IRQs          
                tst     >V.InCnt,u      ; any data available?
                bne     L00F9           ; yes, data ready
		
                std     >SigPrc,u       ; save signal data
                puls    cc              ; restore masks
                bra     ExitNoErr       ; no error, exit
		
L00F9           puls    cc              ; pull flags
                os9     F$Send          ; send the signal
                clrb                    ; no error
                rts                     
		
Puts.A          cmpa    #SS.Relea	; release signal?            
                bne     Puts.B          ; no skip
		
                lda     PD.CPR,y        ; get process id
                cmpa    >SigPrc,u       ; waiting for data?
                bne     ExitNoErr	; no, exit no error
           
                clr     >SigPrc,u       ; otherwise release it
                rts                     
		
Puts.B          cmpa    #SS.Mouse	; reset mouse co-ordinates            
                bne     Unknown 	; no error, exit          
                clra                    ; zero d
                clrb                    
                std     >V.MouseX,u   	; zero mouse X and Y     
                std     >V.MouseY,u        
                rts                     
                                        
                                        
* Term
*
* Entry:
*    U  = address of device memory area
*
* Exit:
*    CC = carry set on error
*    B  = error code
*
                                        
Term            ldx     #$0000		          
                os9     F$Timer         ; remove driver from polling table
		
                ldx     #$0000          
                os9     F$IRQ           ; remove mouse port from IRQ polling table
		
                bsr     T.MapOut   	; map out text mode memory
        
                ldx     >V.Block,u      ; deallocate the memory
                ldb     #$01            
                os9	F$GClr		
		
		ldx     >V.Link,u 	; look for grapics link       
                beq     L015C           ; no, skip
		
                ldd     -$02,x          ; get offset to terminate routine
                lbsr    TCallG      	; call it     
		
Terminate2      ldx     <D.Proc       	; save process descriptor pointer   
                pshs    u,x             ; and static storage pointer
                
		ldx     <D.SysPrc       ; replace with system pd pointer   
                stx     <D.Proc     
    
                ldu     >V.GfxMod,u  	; get graphics module address      
                os9     F$UnLink        ; unlink it
                puls    u,x             ; restore static & proc ptr
		
                stx     <D.Proc         ; restore proc pointer
                ldx     #$0000          ; clear graphics link
                stx     >V.Link,u        
L015C           clrb                    ; flag no error
                rts                     
		
******************************
* Map out any presently mapped screen memory
		
T.MapOut        pshs    u,y             ; save static
                ldx     <D.Proc       	; save process descriptor pointer   
                pshs    x               
		
                ldx     <D.SysPrc     	; replace with system descriptor
                stx     <D.Proc          
		
                ldu     >LBADDR,u	; get addr?        
                beq     L0173		; skip if zero
           
                ldb     #$01            ; else clear it
                os9     F$ClrBlk        
		
L0173           puls    u               ; retrieve out proc pd
                stu     <D.Proc          
		
                puls    u,y             ; retrieve static
                ldx     #$0000          ; clear LBADDR
                stx     >LBADDR,u        
                rts                     
		
******************
* Map in the appropreate screen blocks
		
T.MapIn         bsr     T.MapOut 	; release anything currently mapped 
                tst     >V.GRAFIX,u    	; are we in graphics mode?
                bne     L0196          	; yes, deal with it
 
                ldx     >V.Block,u    	; first text block    
                bsr     T.MapOne        ; map it in   
                bcs     L0195           ; error!
		
                stx     >LBADDR,u      	; save it
L0195           rts              
       
L0196           tst     <Resol,u        ; any graphics memory?
                beq     L0195           ; no, exit
		
                ldb     >MyBlk,u        ; make graphics block number
                lsrb                    
                lsrb                    
                lsrb                    
                clra                    
                tfr     d,x             ; put in x
                pshs    x               ; save
		
                bsr     T.MapOne      	; map it     
                bcs     L01AF           ; error, exit
                stx     >LBADDR,u      	; save addresss  
L01AF           puls    pc,x            

T.MapOne        pshs    u,y             ; save static
                ldu     <D.Proc         ; get our process id pointer
                pshs    u               ; save it
		
                ldu     <D.SysPrc       ; replace with system id pointer   
                stu     <D.Proc          
		
                ldb     #$01            ; map the block
                os9     F$MapBlk        
		
                tfr     u,x             ; save the pointer in x
                puls    u               ; restore our process id pointer
                stu     <D.Proc          
                puls    pc,u,y          ; restore and return
                                        
* Write
*
* Entry:
*    A  = character to write
*    Y  = address of path descriptor
*    U  = address of device memory area
*
* Exit:
*    CC = carry set on error
*    B  = error code
*
                                        
Write           inc     >V.Active,u     ; flag write active   
                bsr     Write00       	; go do write    
		
                pshs    cc              ; save error flag (in carry)
                clr     >V.Active,u    	; flag write done....    
                puls    pc,cc   	; restore and return
        
Write00         ldb     <ESCFlag,u	; in escape mode?        
                lbeq    Write6      	; nope, skip
     
                cmpb    #$01            ; first time through?
                lbne    Write5   	; no, skip
        
                leax    >T.EscTab,pcr   ; point at escape table   
                lbsr    T.FindEsc     	; go find it      
                bcs     Write10         ; not found, skip, and check for graphics.....
  
                lbsr    T.DispText  	; ensure text display         
                tst     >V.GRAFIX,u    	; graphics mapped in?    
                beq     Write1         	; no, ok process it
  
                clr     >V.GRAFIX,u    	; set for text
                bra     Write11  	; map it in
         
Write10         ldx     >V.Link,u      	; get graphics link  
                bne     Write2    	; got it...
       
                ldx     <D.Proc        	; save our process descriptor pointer
                pshs    x               
                ldx     <D.SysPrc     	; and replace with system one     
                stx     <D.Proc          
		
                leax    >T.GfxName,pcr 	; point at graphics module name     
                pshs    u,y,a           ; save regs
                lda     #SBRTN+OBJCT	; type of module            
                os9     F$Link          ; attempt to link it
		
                puls    a               ; retrieve character
                bcc     Write3		; got link....go to it           
                puls    u,y             ; restore regs
                puls    x               ; restore our pd pointer
                stx     <D.Proc        
  
Write4          lbra    T.Exit 		; invalid code, ditch it
          
Write3          tfr     u,x             ; save module address
                ldu     $02,s           ; rerieve static pointer
                sty     >V.Link,u      	; save gfx entry point  
                stx     >V.GfxMod,u    	; and module address    
                ldx     $04,s           ; restore process descriptor pointer
                stx     <D.Proc          
		
                tfr     y,x             ; put table address in X
                puls    u,y             ; retrieve regs
                leas    $02,s           ; discard scratch
		
Write2          lbsr    T.FindEsc     	; find the entry
                bcs     Write4          ; error, exit 

; if esc code is a graphics one, check that graphics is running, if so pass it the escape.
; if graphics is not running, check to see if the code is init graphics, otherwise fail.
                 
		tst     >GfxFlag,u	; graphics running?        
                bne     Write21      	; yes      
                cmpa    -$02,x          ; initialize graphics code?
                beq     Write21        	; yes, do it    
                clr     <ESCFlag,u      ; abort the sequence  
                
		comb                    ; flag error
                ldb     #E$BMode	; bad mode            
                rts                     
		
Write21         tst     >V.GRAFIX,u 	; graphics already mapped in?       
                bne     Write1        	; yes, skip
                inc     >V.GRAFIX,u  	; set for graphics

Write11         pshs    x,a             ; save regs
                lbsr    T.MapIn         ; map screen memory
                puls    x,a             ; restore regs
                bcs     Write8 		; error!          

Write1          suba    -$02,x          ; convert the code to an offset
                lsla                    
                sta     >Action,u       ; save it for later
                ldd     a,x             ; get routine offset
                leax    d,x             ; make it absolute
                stx     >PVector,u    	; save routine address    
		
                leax    >Buffer,u	; reset buffer pointer     	   
                stx     >BufPon,u      
  
Write5          ldx     >BufPon,u       ; get buffer pointer
                tst     >V.GRAFIX,u    	; going to graphics    
                bne     Write51         ; yes, skip  
                jmp     [>PVector,u]    ; jump to handler routine  

Write51         pshs    dp,a            ; save dp and char
                tfr     u,d             ; get base page in a
                tfr     a,dp            ; and put it in dp
                puls    a               ; retrieve char
                jsr     [>PVector,u]  	; jump to graphics routine
    
                puls    dp              ; restore dp
                
		tst     <ESCFlag,u   	; routine finished?
                bne     Write52     	; no     
                
		tst     >GfxFlag,u     	; graphics still active?   
                bne     Write52        	; yes   
                
		pshs    b,cc            ; save flags and return code
                lbsr    Terminate2 	; terminate graphics          
                puls    b,cc            ; restore flags and return code
Write52         rts
                     
Write6          cmpa    #C$ESC		; escape code?            
                bne     Write7  	; no, skip
        
                inc     <ESCFlag,u     	; yes set the flag and return   
                rts                    
 
Write7          tst     >V.GRAFIX,u     ; graphics mapped in?
                beq     Write9     	; no, skip on
      
                pshs    a               ; save the char 
                clr     >V.GRAFIX,u    	; set for text    
                lbsr    T.MapIn        	; map in text memory   
                puls    a               ; restore char
                bcs     Write8         	; error (in mapping)
  
Write9          bsr     T.DispText      ; ensure text display
                cmpa    #C$SPAC		; lower than space (control code)?           
                lbcs    CONT		; yes continue, do it
          
                cmpa    #C.DEL		; higher than delete (control code)?            
                lbeq    CONT           	; yes do it
		
                lbra    Print  		; printable, display it         
Write8          rts                     

**********************
*Search a set of escape tables

T.FindEsc       cmpa    ,x+		; in this table?               
                bcs     T.FindEsc1      ; no, lower than beginning of table
                cmpa    ,x+             ; in this table?
                bhi     T.FindEsc2      ; no, higher than end of table
                clrb                    ; otherwise flag valid
                rts
                     
T.FindEsc1      leax    $01,x           ; adjust pointer
T.FindEsc2      ldb     -$01,x          ; calculate the number of entries
                subb    -$02,x          
                incb                    ; base 1
                lslb                    ; 2 bytes per entry
                abx                     ; skip the table
                ldb     ,x              ; first char of next table
                bne     T.FindEsc     	; if not zero, not last table....    
                comb                    ; otherwise last, flag it
                rts                     
                                        
T.GfxName       fcs     'gfxdrvr' 	; graphics driver name if loaded      

************
* Ensure text display is on
*
                                        
T.DispText      pshs    x,a             ; save regs
                tst     >GfxDisp,u    	; displaying graphics ?    
                beq     T.Dispt1 	; no, ok          
                                        
                lbsr    T.Init  	; set for text display         
                                        
T.Dispt1        puls    pc,x,a   	; restore and return

************
* Update light pen
*
       
T.LtPen         lda     <D.LtPen	; get control reg from direct page
                bpl     T.LtPen1        ; not light pen interrupt
   
                lda     #$10            ; first light pen reg no
                ldx     #A.Crtc		; point to CRTC
                sta     ,x              ; select CRTC register
                ldb     $01,x           ; read CRTC register (high byte)
                pshs    b               ; save it
                inca                    ; select second CRTC register
                sta     ,x              
                ldb     $01,x           ; and get value
                puls    a               ; 16 bit value now in D
                subd    #C.LtPen	          
                subd    >V.PhysAdd,u        ; make logical address
                tst     >V.LtPen,u 	; already seen one?       
                bne     T.LtPen2     	; yes, skip
      
T.LtPen3        std     >V.LtPenAd,u   	; save light pen address     
                lda     #C.DebLP 	; set lightpen debounce count
                sta     >V.LPDeb,u 	       
                sta     >V.LtPen,u      ; and set flag  
                clr     >V.LPRep,u    	; clear lightpen repeat counter    
                rts                     

T.LtPen2        cmpd    >V.LtPenAd,u  	; same as last       
                bne     T.LtPen3        ; no....
                
		tst     >V.LPDeb,u    	; already debounced?    
                beq     T.LtPen4	; yes, skip           
                dec     >V.LPDeb,u    	; decrement debounce counter    
                rts                     
		
T.LtPen4        tst     >V.LPRep,u    	; repeating?
                beq     T.LtPen5   	; no, skip
        
                dec     >V.LPRep,u    	; decrement repeat count    
                bne     T.LtPen6        ; branch if not time for another
		
                bsr     T.LPOut      	; output to buffer
		
                lda     #C.RPT		; reset repeat count            
T.LtPen7        sta     >V.LPRep,u        
T.LtPen6        clrb              	; flag no error      
                rts                     
		
T.LtPen5        bsr     T.LPOut   	; put in buffer
                lda     #C.IRPT		; set initial repeat delay            
                bra     T.LtPen7       	
    
T.LtPen1        clr     >V.LtPen,u    	; flag light pen not active  	  	
                rts                     
		
T.LPOut         lda     #C$Lead		; send lead in sequence   		         
                bsr     T.LPOut1           
                bsr     T.GLtPen       	; get light pen address    

                lda     >V.LtPenX,u 	; send X first       
                adda    #C.CurOfs	; add offset            
                bsr     T.LPOut1       	; send it
    
                lda     >V.LtPenY,u  	; now send Y      
                adda    #C.CurOfs	; add offset            
T.LPOut1        lbra    T.InBuf  	; put char in buffer    

**********
* Return light pen status
*   
   
T.GLtPen        ldd     >V.LtPenAd,u  	; get the light pen character address      
                clr     ,-s             ; clear screatch on stack for Y value
		
T.Gt1           inc     ,s              ; count rows
                subb    >V.MaxCol,u  	; subtract characters per line      
                sbca    #$00            
                bcc     T.Gt1   	; until got no of rows        
                dec     ,s              ; make zero based
                addb    >V.MaxCol,u    	; restore column    
                puls    a               ; retrieve line
                std     >V.LtPenY,u   	; save Y     
		clrb                    
		rts                     
                                        
;
; IRQ routine
;
                                        
IRQHandler                              
                ldx     #A.Mouse        ; Point to PIA at $FC24
                lda     $01,x           ; Get CRA
                ldb     ,x              ; Get Port A (Clear inturrupt)
                ldb     $02,x           ; Get Port B contents mouse inputs
		
                leax    >V.MouseY,u     ; point at mouse y counter
                tsta          		; Check for CA1 int          
                bpl     L03B7   	; no skip
        
                leax    >V.MouseX,u     ; point at mouse x counter
                bitb    #$10            ; check if x bit is set
                bra     L03BB  		; branch
         
L03B7           eorb    #$20            ; flip y bit 
                bitb    #$20            ; and test it
		
L03BB           bne     L03C4           ; bit not set, skip
                ldy     ,x              ; get x or y current value
                leay    -$01,y          ; decrement it
                bra     L03C9           
		
L03C4           ldy     ,x              ; get x or y current value
                leay    $01,y           ; increment it
L03C9           sty     ,x              ; store value back
                clrb                    ; flag no error
T.Null          clrb                    
                rts      
               
*************************                                        
*
*Control vector routine
*
*Input: A - Control code
* 
CONT           	leax    >T.ContTab,pcr  ; point at control table    
                cmpa    #C.Del		; Del?            
                bne     Cont1  		; no, skip         
                lda     #$20            ; use $20 for del
		
Cont1           lsla                    ; 2 bytes per entry
                ldd     a,x             ; get offset
                jmp     d,x             ; go to it....
		
***********************************
*
*Escape sequence routines 
*
***********************************		
*********		
*Beeper (Bell)
*
T.Bell          ldx     #A.Beeper	; point to beeper          
                pshs    y               ; save y
                ldy     #C.BCyc		; number of half cycles         

T.Bell1         pshs    y         	; save cycle count
      
                pshs    cc              ; save flags
		orcc    #IntMasks 	; disable IRQs
                lda     ,x              ; get port b
                eora    #B.Bell		; flip speaker bit            
                sta     ,x              ; put back in port
                puls    cc              ; restore flags
		
                ldy     #C.BPer         ; get period delay
T.Bell2         leay    -$01,y          
                bne     T.Bell2           
		
                puls    y               ; pull cycle count
                leay    -$01,y          ; decrement
                bne     T.Bell1         ; loop if not zero
                clrb                    ; flag no error
                puls    pc,y     
       
*************************                                        
*
*Backspace - move back one column
*       
T.Back          dec     >V.Column,u    	; move back one    
                bpl     T.Back1        	; skip if no wrap round
   
                lda     >V.MaxCol,u     ; move to end of line   
                deca                    
                sta     >V.Column,u   	; update column     
                lbra    T.UpLine  	; move up one line
         
T.Back1         lbra    T.NewCur 	; set new cursor          

*************************                                        
*
*Line feed - move down one
*
T.Feed          lda     >V.CLine,u	; get current line        
                inca                    ; increment it
                cmpa    >V.MaxLine,u  	; past maximum?      
                lbeq    T.UpScroll    	; scroll up if so
       
                sta     >V.CLine,u	; update current line        
                lbra    T.NewCur       	; set new cursor
    
*************************                                        
*
*Carriage return - set column to zero
*
T.Return        clr     >V.Column,u    	; zero current column
                lbra    T.NewCur        ; set new cursor 

*************************                                        
*
*Clear the screen
*  
T.Clear         bsr     T.Home 		; home the cursor
          
T.CEos          ldd     >V.CLine,u 	; get current cursor co-ordinates       
                pshs    d		; save
             
T.CEoS1         lbsr    T.CEOL  	; clear to end of line         
                clr     >V.Column,u     ; reset column to 0
                inc     >V.CLine,u      ; move to next line
                lda     >V.CLine,u     	; get current line   
                cmpa    >V.MaxLine,u   	; past end of screen?     
                bcs     T.CEoS1         ; no loop again for next line
 
                puls    d             	; recove home co-ordinates
                std     >V.CLine,u	; restore them	        
                rts                     
*************************                                        
*
*Home cursor to 0,0
*		
T.Home          clr     >V.CLine,u   	; reset current line to 0
                clr     >V.Column,u     ; reset current column to 0   
                lbra    T.NewCur        ; set new cursor    
		

***********************************
*
*Escape sequence routines 
*
***********************************
*************************                                        
*
*Exit - general exit clearing escape flag
*
T.Exit          clr     <ESCFlag,u  	; clear escape flag
                rts                     
*************************                                        
*
*Left - move left one
*
T.Left          lbsr    T.Back          ; backspace
                bra     T.Exit          ; and exit
*************************                                        
*
*Right - move right one
*
T.FWD           lda     >V.Column,u    	; get current column    
                inca                    ; increment it
                sta     >V.Column,u    	; save it back    
                cmpa    >V.MaxCol,u    	; past last column?    
                bcs     T.FWD1     	; no, skip
      
                clr     >V.Column,u   	; reset current column to 0     
                lbra    T.Feed        	; do a line feed   
T.FWD1          lbra    T.NewCur        ; set new cursor    

*************************                                        
*
   
T.Right         bsr     T.FWD           ; move forward one
                bra     T.Exit          ; and exit 

*************************                                        
*
*Up - move up one
*
T.UpLine        dec     >V.CLine,u  	; decrement current line      
                bpl     T.FWD1         	; branch if not first line
  
                clr     >V.CLine,u     	; reset current line to 0   
                lbra    T.DScroll   	; scroll screen down
        
                bsr     T.UpLine           
                bra     T.Exit      

*************************                                        
*
*Down - move down one
*     
T.Down          lbsr    T.Feed        	; move down one
                bra     T.Exit          ; and exit

*************************                                        
*
*cursor addressing - expects x, then y 
*binary bytes offset of C.CurOfs
*

T.CurAdd        ldb     <ESCFlag,u	; get escape flag        
                suba    #C.CurOfs      	; subtract offset
                inc     <ESCFlag,u     	; bump it   
                subb    #C.EscCnt	; first time through?            
                bne     T.Cur1         	; skip if not  
                rts                     ; else return
		
T.Cur1          decb                    ; x co-ordinate?
                bne     T.Cur3      	; if not, must be y
     
T.Cur6          cmpa    >V.MaxCol,u   	; valid column?     
                bcs     T.Cur2         	; skip if so  

                lda     >V.MaxCol,u   	; stop at end of line     
                deca                    
T.Cur2          sta     >V.Column,u    	; update column
                clrb                    
                rts                     
		
T.Cur3          bsr     T.Cur7  	; validate line
T.Cur5          lbsr    T.NewCur      	; set new cursor         
                bra     T.Exit1           

*************************                                        
*
*cursor addressing y then X
*
T.CurYX
                ldb     <ESCFlag,u 	; get escape count       
                suba    #C.CurOfs	; subtract offset            
                inc     <ESCFlag,u     	; bump count   
                subb    #C.EscCnt	; first time through?         	   
                bne     T.CurY1        	; no, skip
                rts                     

T.CurY1         decb                    ; y co-ordinate
                beq     T.Cur7         	; yes, deal with it  
                bsr     T.Cur6          ; set the x co-ordinate
                bra     T.Cur5          ; and exit

T.Cur7          cmpa    >V.MaxLine,u	; valid line?        
                bcs     T.Cur4 		; yes, skip
          
                lda     >V.MaxLine,u  	; stop at bottom of page
                deca                    
T.Cur4          sta     >V.CLine,u    	; set current line
                clrb                    ; and exit
                rts                     
		
*************************                                        
*
*Clear to end of line
*
T.ClrEol        lbsr    T.CEOL           ; clear to end of line
T.Exit1         lbra    T.Exit           ; exit

*************************                                        
*
* Clear to end of screen
*
T.ClrEos        lbsr    T.CEos  	; clear to end of screen         
                bra     T.Exit1       	; exit    
		
*************************                                        
*
* Set intensity
*
T.Intens        ldb     #$01            ; max value
                bsr     T.GetDigit    	; get digit
       
                sta     >V.Intens,u   	; save intensity     
                lbsr    T.Init0        	; init   
                bra     T.Exit1         ; exit  

*************************                                        
*
*Select character set (0-3, single ASCII digit)
*

T.SelSet        ldb     #$03         	; max value
                bsr     T.GetDigit	; get parameter, validated           
                clrb                    ; clear flag
                lsra                    ; character set 1 or 3 (odd?)
                bcc     T.SelS1    	; no, skip       
		
                ldb     #$80            ; set character bit 7
T.SelS1         stb     >V.ChrS1,u      ; set first flag  
                sta     >V.ChrS2,u      ; set second flag
  
                lbsr    T.Init0   	; set new port        
                bra     T.Exit1           

*************************                                        
*
*Go into text mode
*
		
T.Text          lbsr    T.Init       	; init text mode
                bra     T.Exit1           

*************************                                        
* Select cursor type (0, 1 or 2, single ASCII digit)
*	0 - block
*	1 - underline
*	2 - no cursor
*

T.SelCur        ldb     #$02          	; maximum value
                bsr     T.GetDigit      ; go get validated digit     
                
		leax    <T.CurTab,pcr  	; point at cursor value table
                ldb     a,x             ; get selected value
                stb     >V.CurType,u    ; update cursor type in statics
    
                lda     #$0A            ; CRTC register 10
                sta     >A.Crtc		; select register          
                stb     >A.Crtc+1	; write value          
                bra     T.Exit1        	; exit   
                                        
T.CurTab        fcb     $60,$69,$20     ; table of cursor types

*************************                                        
* Get a single ASCII digit as a parameter
*	(B) is max allowed value
*
                                        
T.GetDigit      pshs    b             	; save max value  
                ldb     <EscFlag,u     	; get escape count
                inc     <EscFlag,u      ; bump it
                subb    #C.EscCnt	; first time through?            
                bne     T.GetD2         ; no, skip
                                        
                leas    $03,s           ; abort one level 
                clrb                    ; no error
                rts                     
                                        
T.GetD2         suba    #$30            ; convert ASCII to binary
                bcs     T.GetD1  	; branch if invalid         
                
		cmpa    ,s              ; compare to max value
                bhi     T.GetD1        	; branch if not within range
   
                puls    pc,b            ; restore & return

T.GetD1         leas    $03,s           ; abort one level 
                bra     T.Exit2         
 
*************************                                        
* Select foreground colour
* 

T.SelFore       ldb     #C.MtCol       	; max value
                bsr     T.GetDigit      ; go get it     

T.SelF1         sta     >V.RBack,u  	; set reverse background colour      
                lsla                    ; shift to foreground
                lsla                    
                lsla                    
                sta     >V.Fore,u  	; save foreground colour      
                bra     T.Exit21     
      
*************************                                        
* Select foreground colour
* 

T.SelBack       ldb     #C.MtCol       	; max value
                bsr     T.GetDigit      ; go get it     

T.SelB1         sta     >V.Back,u 	; set background colour       
                lsla                    ; shift to foreground
                lsla                    
                lsla                    
                sta     >V.RFore,u	; save reverse forground colour        
                bra     T.Exit21       	; and exit
*************************                                        
*Underline on
*   
T.ULOn          lda     #C.ULbit	; get underline mask            
                sta     >V.ULine,u 	; save it       
                bra     T.Exit21       	; exit    

*************************                                        
*Underline off
*   
T.ULOff         clr     >V.ULine,u	; clear underline flag        
                bra     T.Exit21        ; exit   


*************************                                        
*Flash on
*   
T.Flash         lda     #C.FlBit	; get flash mask            
                sta     >V.Flash,u 	; save it       
T.Exit21        lbsr    T.GetAtt       	; setup attribute byte    
T.Exit2         lbra    T.Exit           

*************************                                        
*Flash off
*   
T.FlashOff      clr     >V.Flash,u      ; clear flash mask
                bra     T.Exit21    	; and exit   
		
*************************                                        
* Select 40x25 display
*
T.Sel40         lda     #$01            
                sta     >V.Mode40,u   	; set 40 column mode  	   

TSel401         lbsr    T.ResPag    	; reset mode, clear screen 
                bra     T.Exit2        	; exit
   
*************************                                        
* Select 80x25 display
*
T.Sel80         clr     >V.Mode40,u   	; set 80 column mode
                bra     TSel401        	; go program CRTC, clear screen   
		
*************************                                        
* Select numeric keypad
*

T.SelNum        clr     >V.FunPad,u     ; Clear function pad flag
                bra     T.Exit2         ; and exit
		
*************************                                        
* Select function keypad
*
T.SelFun        lda     #$01            ; Flag function pad
                sta     >V.FunPad,u     
                bra     T.Exit2      	; and exit
     
*************************                                        
* Select reverse field 
*
T.SetRev        lda     #$01            ; flag reverse field
                sta     >V.Rev,u        
                bra     T.Exit21  	; and exit
         
*************************                                        
* Clear reverse field 
*

T.ClrRev        clr     >V.Rev,u        ; clear reverse field flag
                bra     T.Exit21        ; and exit
   
*************************                                        
* Set ASCII numeric parameter mode
*

T.SetAsc        lda     #$01            ; set ascii flag
                sta     >Binary,u        
                bra     T.Exit2         ; and exit
  
*************************                                        
* Set binary numeric parameter mode
*
T.SetBin        clr     >Binary,u   	; clear ascii flag
                bra     T.Exit2        	; exit
   
*************************************
* 
* Print - display a printable character
*

Print           ldb     >V.Attribute,u  ; get attribute byte      
                tsta            	; test character to print        
                bpl     L05EC           ; branch if < 128

                cmpa    #$A0 		; is it > $A0           
                bls     L05E4           ; yes
                clrb                    
                rts   
                  
L05E4           bcs     L05EA           
                lda     #$7F            
                bra     L05EC           
L05EA           anda    #$1F            
L05EC           ora     >V.ChrS1,u     	; or in character set 1 flag
                std     [>V.CurAdd,u]	; put it on screen      
                lbra    T.FWD           ; move cursor forward

*************************************
* 
* Get attribute byte
*

T.GetAtt        ldb     >V.ULine,u  	; get underline flag
                orb     >V.Flash,u     	; or in flash flag   
                tst     >V.Rev,u        ; is reverse field active?
                bne     T.GetAtt1    	; yes, skip
       
                orb     >V.Fore,u     	; or in foreground
                orb     >V.Back,u       ; and background  
                bra     T.GetAtt2           
		
T.GetAtt1       orb     >V.RFore,u   	; reverse, or in reverse forground     
                orb     >V.RBack,u      ; and background  
T.GetAtt2       stb     >V.Attribute,u  ; save it      
                rts                     
***********************************
*
*Display action routines 
*
***********************************

*************************************
*Set a new cursor
*		
T.NewCur        ldd     >V.CLine,u     	; get current line(a) & column(b)  
                lbsr    T.LogAdd    	; calculate logical address
       
                pshs    d		; save offset address             
                lsra                    ; divide it by 2
                rorb                    
                std     >V.Cursor,u	; save as cursor pos        
                puls    d		; restore logical address
             
                addd    >LBADDR,u       ; add begining of video RAM
                std     >V.CurAdd,u        
                ldb     #$01      	; show new cursor set      
                stb     >V.NCur,u  	      
                clrb                    
                rts                     

*************************************
* Clear to end of line
*
		
T.CEOL          ldd     >V.CLine,u  	; get cursor line and column
                lbsr    T.ScrAddrX          	; get pointer to screen ram at line,col in X
 
                ldb     >V.MaxCol,u    	; get line width    
                subb    >V.Column,u    	; subtract our column (calc no of columns to do)  
                pshs    b               ; save it
		
                ldb     >V.Attribute,u  ; get current attribute byte
                andb    #$FF-C.ULBit-C.FlBit	; mask out flash and underline            
                lda     #C$SPAC		; space char
            
T.CEOL1         std     ,x++            ; save character and attribute
                dec     ,s              ; decrement counter
                bne     T.CEOL1         ; keep going till all done
                clrb                    ; no error
                puls    pc,a            ; restore and return
		

*************************************
* Scroll screen up one line
*		
T.UpScroll      ldd     >V.CLine,u    	; get current line and column    
                pshs   	d              	; save them
		
                lda     >V.MaxLine,u   	; move to bottom line
                deca                    
                clrb                    
                std     >V.CLine,u    	; save them    
                clrb                    
                lda     #$01            
                pshs    u,y             
		
                lbsr    T.ScrAddrX  	; get physical address of second line         
                ldy     >V.ScrSize,u   	; get screen size     
                ldu     >LBADDR,u     	; get base address of video ram   
                bsr     T.MoveUp           
                puls    u,y             
		
T.UpScroll1     bsr     T.CEOL         	; clear to end of line  
                puls    d		; restore original cursor pos             
                std     >V.CLine,u    	; save it    
                lbra    T.NewCur       	; and display cursor there
    
T.MoveUp        ldd     ,x++            ; chr, attr 0
                std     ,u++            
                ldd     ,x++            ; chr, attr 1
                std     ,u++            
                ldd     ,x++            ; chr, attr 2
                std     ,u++            
                ldd     ,x++            ; chr, attr 3
                std     ,u++            
                leay    -$04,y          ; decrement by number of characters moved (4)	
                bne     T.MoveUp       	; loop if more to do    
                rts                 
    
*************************************
* Scroll screen down one line
*		
T.DScroll       ldd     >V.CLine,u    	; get current line and column    
                pshs   	d              	; save them
		
		clra                    ; reset ot 0,0
                clrb                    
                std     >V.CLine,u    	; save 	    
                clrb                    
                lda     >V.MaxLine,u  	; get last line of screen      
                deca                    ; minus one
		
                pshs    u,y             ; save regs
                bsr     T.ScrAddrX    	; get physical screen address (of 0,0)       
                ldy     >V.ScrSize,u  	; get screen size in chars      
                ldu     >LBADDR,u      	; get base of screen  
                tfr     y,d             ; screen size into d
                lslb                    ; multiply by 2 (because of char, attr)
                rola                    
                leau    d,u             ; calculate end of screen
		
                bsr     T.MoveDown     	; scrool down      
                puls    u,y		; restore regs             
                bra     T.UpScroll1    	; clear to end of line, reset cursor pos etc       
		
T.MoveDown      ldd     ,--x            ; chr, attr 0
                std     ,--u            
                ldd     ,--x            ; chr, attr 1
                std     ,--u            
                ldd     ,--x            ; chr, attr 2
                std     ,--u            
                ldd     ,--x            ; chr, attr 3
                std     ,--u            
                leay    -$04,y          ; decrement by number of characters moved (4)	
                bne     T.MoveDown     	; loop if more to do    
                rts                     
;
; Get physical address of cursor in X
; Input:  A=Line, B=Col
; Output: X=physical address
;
T.ScrAddrX      pshs    d   		; save co-ordinates          
                bsr     T.LogAdd       	; calculate logical address    
                ldx     >LBADDR,u      	; get base of screen memory  
                leax    d,x             ; add logical to make physical
                puls    pc,d          	; restore and return

*************************************
* 
* Calculate the logical address, relative to the start of the block
*

T.LogAdd        lslb                    ; multiply column by 2 for char,attr
                pshs    b               ; save column
		
                ldb     >V.MaxCol,u     ; get number of columns per line
                lslb                    ; multiply by 2 for char,attr
                mul                     ; multiply no of lines, by columns
                addb    ,s+             ; add on column
                adca    #$00            ; take account of carry
                rts                     ; return offset address in D

*************************************
*Select columns per line for this mode 
*point to the appropreate CRTC table
*		
T.Cols          leax    >T.IN80,pcr   	; assume 80 col   
                ldb     #C.MCol80	; no of cols
                tst     >V.Mode40,u    	; is this 40 char mode?    
                beq     T.Cols1        	; no, skip   
                
		leax    >T.IN40,pcr   	; setup for 50 cols   
                ldb     #C.Mcol40	; no of cols
           
T.Cols1         stb     >V.MaxCol,u    	; set max column    
                lda     #C.MaxLin	; max number of lines            
                sta     >V.MaxLine,u  	; save it  	    
                deca                    ; make zero based....
                mul                     ; calculate chars / screen
                std     >V.ScrSize,u  	; save it      
                rts          
           
T.Init          bsr     T.Cols		; set columns per line           
                clra                    ; first register is 0
                ldb     #C.InSiz      	; number of registers to write      
                pshs    cc              ; save cc
		
                orcc    #IntMasks	; disable interrupts            
                bsr     T.CRTC   	; write regs
        
                lda     #$0A            ; set cursor type
                sta     >A.Crtc		; set reg          
                lda     >V.CurType,u    ; get type    
                sta     >A.Crtc+1	; write value
          
                lda     #$01            
                sta     >V.NStart,u  	; set new start address      
                sta     >V.NCur,u      	; set new cursor address  
                puls    cc              ; restore flags
		
T.Init0         lda     >V.PhysAdd+2,u   	; get high byte of physical address
                anda    #$C0            ; keep top two bits
                ora     >V.Intens,u     ; or in intesnsity setting   
                tst     >V.ChrS2,u    	; char set 2?    
                beq     T.Init4		; yes            
                ora     #$02            ; or in ?
		
T.Init4         tst     >V.Mode40,u    	; 40 column mode?    
                bne     T.Init2        	; yes, skip
   
                ora     #$2C            ; set for 80 col mode
                bra     T.Init3        	; skip
   
T.Init2         ora     #$08            ; set for 40 col mode
T.Init3         pshs    cc              ; save flags
                orcc    #IntMasks	; disable interrupts
            
                sta     >A.GCon		; set PIA register          
                sta     <SINTH+1      	    
                puls    cc      	; restore flags
        
                clr     >GfxDisp,u    	; flag text mode    
                rts                     

*************************************
*Reset and clear the display
*

T.ResPag        clra                    ; home cursor
                clrb                    
                std     >V.CLine,u        
                std     >V.Cursor,u     
   
                lbsr    T.Cols        	; set columns   
                lbsr    T.Clear        	; clear the screen   
                lbra    T.Init         	; init the display
  
*************************************
*Set 6845 registers
*
*Input:	A - Start register
*	B - Number of registers
*	X - Address to take register values from
*

T.CRTC          pshs    y,b             ; save regs	
                ldy     #A.Crtc		; point at CRTC
          
T.CRTC1         sta     ,y              ; set register no to write
                ldb     ,x+             ; get register value to write
                stb     $01,y           ; write it
                inca                    ; move to next register
                dec     ,s              ; decrement counter
                bne     T.CRTC1       	; loop if more    
		
                puls    pc,y,b       	; restore and return

*************************************
* Tick routine - called by the clock module on VSync
* Entry point is T.Tick
*
   
T.Tick2         tst     >GfxFlag,u   	; graphics active?
                beq     T.Tick1    	; no, ignore
       
                ldx     >V.Link,u     	; get graphics link address   
                beq     T.Tick1       	; none, skip
		
                ldd     -$04,x          ; get offset to display routine
                bsr     TCallG        	; call graphics routine   
                clrb                    ; no error
                rts         
            
TCallG          leax    d,x             ; add offset in
                leax    $02,x           ; plus 2 to make routine address
                pshs    dp              ; save dp
                tfr     u,d             ; get page into a
                tfr     a,dp            ; and now into dp
                jsr     ,x              ; call graphics
                puls    pc,dp           ; restore and return

T.Tick          bsr     T.VSync 	; handle start address and cursor          
                lbsr    T.LtPen        	; check light pen
                bsr     T.KBD         	; check keyboard
  
                tst     >V.Active,u  	; in the middle of writing?      
                bne     T.Tick1        	; yes, skip
		
                tst     >V.Switch,u	; want to switch displays?     
                beq     T.Tick1       	; no, skip
  	  
                clr     >V.Switch,u     ; clear display switch flag
                tst     >GfxDisp,u     	; currently showing graphics?   
                beq     T.Tick2        	; no, show graphics   
                lbsr    T.Init         	; else show text 	 
T.Tick1         rts                     

*************************************
* 
* Interrupt routine called on VSync to update
* cursor and start address
*
*************************************

T.VSync         tst     >V.NCur,u     	; new cursor address?
                beq     T.VSync1    	; skip if not
       
                lda     #$0E            ; cursor register number
                leax    >V.Cursor,u   	; point at logical address     
                bsr     T.VSync3   	; send to CRTC        

                clr     >V.NCur,u    	; clear new cursor flag
    
T.VSync1        tst     >GfxDisp,u   	; currently showing graphics?
                bne     T.VSync2 	; yes, don't touch start address
          
                tst     >V.NStart,u  	; new start address?
                beq     T.VSync2      	; skip if not     
                
		lda     #$0C         	; start address register
                leax    >V.Start,u 	; get new start address       
                bsr     T.VSync3       	; send to CRTC
    
                clr     >V.NStart,u    	; clear new start address flag    
T.VSync2        rts                     

T.VSync3        pshs    a               ; save register number
                ldd     ,x              ; get logical address
                addd    >V.PhysAdd,u	; add physical address        
                tfr     d,x             ; put it in X
		
                puls    a               ; restore register number
                pshs    x               ; put x in scratch
                leax    ,s             	; point at it 
                ldb     #$02            ; 2 bytes
                lbsr    T.CRTC          ; copy to CRTC
                puls    pc,x           	; restore and return 
                                        
**************************************
*
*Keyboard handling routine
*
*Called on inturrupt to poll the keyboard
*
*
**************************************
*
* Keyboard polling routine
*
* PB2		1 = One or more input lines low
* CB2	1)	Output load; low enables paralell load of the
*		input shift registers.
* 	2)	Positive edge clocks data into the output
*		shift registers.
* PB4	1)	data out, clocked by CB2.
*	2)	positive edge clocks data out of the input
*		shift register.
*	3)	Caps lock indicator, a low on PB4, and a paralell
*		load (CB2 low), turns the LED on.
* PB5		data in, clocked by PB4.
*
                                        
K.Any           equ     4               ; PB2 (bitmask)
K.OutClk        equ     8               ; CB2
K.Load          equ     K.OutClk        
K.InClk         equ     16              ; PB4 (bitmask)
K.OutDat        equ     K.InClk         
K.InDat         equ     32              ; PB5 (bitmask)
K.High          equ     7               ; Input shiftregister height (length?)
K.DeBnce        equ     1               ; Debounce count
                                        
                                        
T.KBD           lbsr    T.KbdChk        ; any key down ?
                bne     T.K10           ; yes : process it
T.K05           clr     >V.AKey,u        ; no : flag no key, clear carry
                rts                     
                                        
T.K10           lda     >V.AKey,u        ; Was there a key ?
                bne     T.K30           ; Yes
                lda     #$01            
                sta     >V.INVKEY,u     
                sta     >V.AKey,u        
                leay    >V.TREGS,u      ; Point at temp regs
                lbsr    T.GetRGS        ; Get regs
                bcs     T.K05           ; Key conflict, exit
                leay    >V.REGS,u       ; Clear regs
                ldb     #K.Wide         ; number of regs ($0A)
                clra    quicker         ; than lda #0

T.K15           sta     ,y+             ; clear reg
                decb                    ; decrement count
                bne     T.K15           ; done all : no do next
                                        
T.K20           ldb     #K.DeBnce       ; set debounce count ($01)
                stb     >V.DeBnce,u     
                                        
T.K25           clrb    no              ; error
                rts                     
                                        
T.K30           leay    >V.SRegs,u      ; Point at scratch regs
                lbsr    T.GetRGS        ; Get them
                bcs     T.K25           ; Key conflict
                                        
                leay    >V.SRegs,u      ; Repoint at scratch regs
                leax    >V.TREGS,u      ; Point at previos regs values
                clr     ,-s             ; Clear change flag (on stack)
                ldb     #K.Wide+1       ; number of regs ($0B), [this differs from asm src, in src this is $0A]

T.K40           lda     ,y+             ; Get next reg from new set
                cmpa    ,x+             ; compare to same reg in old
                beq     T.K50           ; Reg the same ? yes : skip
                inc     ,s              ; Flag we have a changed reg
                sta     -$01,x          ; Update saved regs
                                        

T.K50           decb                    ; Decrement count
                bne     T.K40           ; Done all ? : No do next
                                        
                lda     ,s+             ; Get changed flag
                bne     T.K20           ; Changed : yes, set debounce
                                        
                lda     >V.DeBnce,u     ; Already debounced ?
                lbeq    T.K200          ; yes : do auto-repeat
                dec     >V.DeBnce,u     ; Debounced ?
                beq     T.K60           ; yes : jump ahead
                clrb    flag            ; no error
                rts                     
                                        
;
; In DDSrc, the routine at T.K65 is inline, the actual disasembled src
; has this as a sub-routine, I guess this saveas a few bytes :)
;
T.K65           pshs    u,a             ; Save regs
                ldu     $06,s           ; Retrieve static pointer
                lda     #$01            
                sta     >V.INVKEY,u     ; Can't auto-repeat
                puls    u,a             ; restore
                rts                     ; return (puls u,a,pc ???)
                                        
T.K60           leay    >V.REGS,u       ; Point at old regs
                leax    >V.TREGS,u      ; Point at new regs
                lda     $0A,x           ; (not in DD src)
                sta     >V.MouseB,u       ; (not in DD src)
                ldb     #K.Wide+1       ; number of saved regs ($0B) [again dif from dd src]
                pshs    u               ; Save static pointer
                leau    >V.SRegs,u      ; Point U at scratch regs
                clr     ,-s             ; Clear change flag
                                        
T.K70           lda     ,x+             ; Get new reg
                eora    ,y+             ; Get changes
                sta     ,u+             ; Save in scratch
                beq     T.K80           ; No changes : skip ahead
                                        
                anda    -$01,x          ; Keys pressed, and not released
                cmpa    -1,u            ; Any released ?
                beq     T.K75           ; no : jump ahead
                bsr     T.K65           ; Call sub above, this is inline in DD src.
                                        
T.K75           sta     -1,u            ; Set newly pressed
                beq     T.K78           ; none : jump ahead
                                        
                inc     ,s              ; Flag change (key pressed)
                                        
; Following 3 instructions not in DD src
                                        
                cmpb    #$01            
                bne     T.K78           
                bsr     T.K65           
                                        
T.K78           lda     -$01,x          ; Update current regs from changed
                sta     -$01,y          
                                        
T.K80           decb                    ; Decrement counter
                bne     T.K70           ; All done ? : no loop again
                                        
                lda     ,s+             ; Get change flag
                puls    u               ; retrieve static pointer
                beq     T.K200          ; no changes : do auto-repeat
                                        
                clr     >V.Shift,u      ; Clear shift flag
                clr     >V.Cntrl,u      ; Clear control flag
                clr     >V.Func,u       ; Clear function flag
                lda     >V.REGS,u       ; Get specials reg (V.regs+K.specl in dd src)
                bita    #C.Shift        ; test for shift key ($01)
                beq     T.K92           ; No : skip
                inc     >V.Shift,u      ; Set shift flag
                                        
T.K92           bita    #C.Cntrl        ; Test for control key ($10)
                beq     T.K94           ; No: skip
                inc     >V.Cntrl,u      ; Set control flag
                                        
T.K94           bita    #C.Funct         ; Test for function key ($02)
                beq     T.K96           ; No: skip
                inc     >V.Func,u       ; Set function flag
                                        
T.K96           lda     #C.IRPT         ; Set initial repeat count ($32)
                sta     >V.RPT,u        
                lda     >V.SRegs,u      ; Get changes in special [K.SPECL+V.Sregs in dd src]
                bita    #C.Shift+C.Funct+C.Cntrl ; Get speciuals ($13)
                beq     T.K110          ; No specials, must be some other change !
                anda    #$FF-(C.Shift+C.Funct+C.Cntrl) ; Remove specials from changes ($EC)
                sta     >V.SRegs,u      ; Save it [V.SRegs+KSpecl in dd src]
                lda     #$01            
                sta     >V.INVKEY,u     ; Not to be repeated (cryptic DD coments strike again !????)
                                        
T.K110          ldd     #K.Wide+1       ; Set count (B), clear reg no (A) ($000B)
                pshs    b,a             ; save
                leay    >V.SRegs,u      ; Point at change regs
                                        
T.K120          ldb     ,y+             ; Get a reg
                beq     T.K150          ; No changes : skip
                clra    zero            ; bit count
                                        
T.K130          inca                    ; Increment bit count
                lsrb                    ; shift bit into carry ?
                bcc     T.K130          ; Bit found ? no : loop and do next
                                        
                deca                    ; Decrement bit count, as no bit set
                pshs    b,a             ; Save bist + count
                lda     $02,s           ; Retrieve reg number
                ldb     #$07            ; Bits per reg
                mul                     
                addb    ,s              ; Add in bitcount
                stb     >V.Key,u        ; Save keycode
                lbsr    T.K250          ; Save in buffer
                puls    b,a             ; Restore regs
                inca                    
                tstb    Any             ; more in this reg ?
                bne     T.K130          ; Yes, process them
                                        
T.K150          inc     ,s              ; Increment reg number
                dec     $01,s           ; Decrement reg count
                bne     T.K120          ; Done all ? no : do next
                puls    pc,b,a          ; Restore and return
                                        
T.K200          dec     >V.RPT,u        ; Count delay
                beq     T.K210          ; Count zero, send key
                                        
T.K205          clrb    no              ; error
                rts                     
                                        
T.K210          lda     #C.RPT          ; Reset repeat counter ($05)
                sta     >V.RPT,u        
                tst     >V.INVKEY,u     ; Valid key to repeat ?
                bne     T.K205          ; no
                                        
T.K250          lda     >V.Key,u        ; Get key matrix code
                leax    >TKTab,pcr      ; Point at translation table
                tst     >V.Func,u       ; Function key ?
                beq     T.K260          
                leax    >TFTab,pcr      ; Point at function key tab
                                        
T.K260          tst     >V.Cntrl,u      ; Control key ?
                beq     T.K270          ; Nope
                leax    >TcTab,pcr      ; Point at control key tab
                                        
T.K270          tst     >V.Shift,u      ; Shift key ?
                beq     T.K280          ; Nope
                leax    >TsTab,pcr      ; Point at shift key tab
                                        
T.K280          lda     a,x             
                tst     >V.CAPL,u       ; In caps lock ?
                beq     T.K290          ; No : skip on
                                        
                cmpa    #'a'            ; Is char not lower case, below 'a' ? 
                bcs     T.K290          ; nope skip
                cmpa    #'z'            ; Is char not lower case, above 'z' ? 
                bhi     T.K290          ; yes skip
                suba    #$20            ; Make char upper case
                                        
T.K290          cmpa    #C.CAPL         ; Is caps lock flag on ? ($FF)
                bne     T.K295          ; nope : skip
                                        
                com     >V.CAPL,u       ; switch caps lock flag
                                        
T.K292          lda     #$01            
                sta     >V.INVKEY,u     ; Can't be repeated (caps lock isn't autorepeating ?)
                clrb    No              ; error
                rts                     
                                        
T.K295          cmpa    #C.Switch       ; Switch displays ? ($FE)...wonder ehat this does ???
                bne     T.K300          ; nope
                                        
                lda     #$01            ; Set request flag
                sta     >V.Switch,u     
                bra     T.K292          ; Not autorepeating
                                        
T.K300          clr     >V.INVKEY,u     ; Got a key that can be repeated, clear invalid repeat flag
                tsta    Function        ; key code ?
                bpl     T.K310          ; Nope
                                        
                tst     >V.FunPad,u     ; Function pad on ?
                bne     T.K310          ; Yes
                                        
                pshs    x               ; save table address
                leax    >TFTab,pcr      ; Point to function key table
                cmpx    ,s++            ; Got it from function key table ?
                beq     T.K310          ; yes
                anda    #$3F            ; Make it a non-function key
                                        
T.K310          lbra    T.InBuf         ; Put in buffer
                                        
*
* Read in the registers (from the keyboard).
*
*
* Entry :-
* 	X	= pointer to PIA I/O location ($FC22)
*	Y	= pointer to buffer for regs read from keyboard.
*
                                        
T.GetRGS                                
                lda     >A.Mouse+2      ; ($FC26) Get contents of PIA port B data
                anda    #$0C            ; Mask it %00001100
                lsra                    ; Shift masked bits int bits 0,1
                lsra                    
                sta     $0A,y           ; Save it somewhwere on end of keyboard regs
                                        
                lda     #$2C            ; Set for auto clock
                sta     $01,x           ; Set control reg
                ldb     ,x              ; Get port
                orb     #K.OutDat       ; Set output data high ($10)
                bsr     T.All1          ; clock K.Wide-1 (9 times)
                andb    #$FF-K.OutDat   ; Set output data high ($EF)
                stb     ,x              
                lda     #$3C            ; Not auto strobe
                sta     $01,x           
                orb     #K.OutDat       ; Subsiquent ones high ($10)
                stb     ,x              
                clr     ,-s             ; Clear key non-conflict flag
                ldb     #K.Wide         ; number of regs to get ($0A)
                pshs    u,b             ; Save regcount, and static pointer
                leau    >V.BRegs,u      ; Point at key conflict flags
                                        
T.GetR1         clr     ,y+             ; asume nothing at first, and therefore no conflict
                clr     ,u+             
                ldb     ,x              ; Get the port
                lda     #$34            ; Load input, clock output
                sta     $01,x           
                lda     #$3C            
                sta     $01,x           
                bitb    #K.Any          ; Anything ? ($04)F
                beq     T.GetR4         ; no : skip ahead
                                        
                clr     ,-s             ; Clear bits found counter
                lda     #K.High         ; Bits to get ($07)
                pshs    a               ; save bitcount
                clra    Clear           ; input
                                        
T.GetR2         andb    #$FF-K.InClk    ; clock the data in ($EF)
                stb     ,x              
                orb     #K.InClk        ; +ve edge, leave it high ($10)
                stb     ,x              
                ldb     ,x              ; Get the data bit
                lsla                    ; Make room for it
                bitb    #K.InDat        ; is it active (high) ? ($20)
                beq     T.GetR3         ; no : skip
                inca                    ; set the bit +ve logic
                inc     $01,s           ; count the bits (found)
                                        
T.GetR3         dec     ,s              ; Decrement bitcount
                bne     T.GetR2         ; All done ? : no do next
                leas    $01,s           ; drop bitcount
                sta     -$01,y          ; save the reg
                lda     ,s+             ; Get bits found count
                beq     T.GetR4         ; None, no conflict
                deca                    ; just one key ?
                beq     T.GetR4         ; therefore also no conflict
                sta     -1,u            ; possible conflict from keys
                inc     $03,s           ; So set conflict flag
                                        
T.GetR4         dec     ,s              ; Done all regs ?
                bne     T.GetR1         ; no : don next
                                        
                puls    u,a             ; Resore static pointer & drop scratch
                andb    #$FF-K.OutDat   ; Set output data low ($EF)
                lda     #$2C            ; Auto stobe
                sta     $01,x           
                bsr     T.All           ; Set all outputs low
                lda     #$34            ; Set paralell load
                sta     $01,x           
                orb     #K.OutDat       ; Default to LED off ($10)
                tst     >V.CAPL,u       ; Caps lock on ?
                beq     T.GetR5         ; Nope, skip
                andb    #$FF-K.OutDat   ; Keyboard LED on ($EF)
                                        
T.GetR5         stb     ,x              ; Set the LED
                lda     ,s+             ; Get conflict flag
                bne     T.ChkR          ; Yes : check it out
                clrb    No              ; error
                rts                     
                                        
T.All           stb     ,x              ; Clock the output K.Wide times
                                        
T.All1          stb     ,x              ; Clock the output K.Wide-1 times
                stb     ,x              
                stb     ,x              
                stb     ,x              
                stb     ,x              
                stb     ,x              
                stb     ,x              
                stb     ,x              
                stb     ,x              
                rts                     
                                        
*
* Check for possible matrix key conflicts.
*
                                        
T.ChkR          leay    -K.Wide,y       ; Reset the pointer (-$0a)
                leax    >V.BRegs,u      
                lda     #K.Wide         ; Number to check ($0A)
                pshs    y,a             ; save no to check + pointer
                                        
T.ChkR1         lda     ,y+             ; Get reg
                ldb     ,x+             ; Possible problem here ?
                beq     T.ChkR2         ; No, skip ahead
                                        
                ldb     #K.Wide         ; number to check ($0A)
                pshs    y               ; save regs pointer
                ldy     $03,s           ; Point at first reg
                                        
T.ChkR4         bita    ,y+             ; Is this a conflict ?
                beq     T.ChkR3         ; No, skip
                                        
                cmpy    ,s              ; is this the reg we are currently checking ? (do not compare with self !)
                beq     T.ChkR3         ; Yes, skip
                leas    $05,s           ; ditch scratch
                comb                    ; Flag error
                rts                     
                                        
T.ChkR3         decb                    ; Finished this check yet ?
                bne     T.ChkR4         ; Nope, continue checking
                puls    y               ; retrieve regs pointer
                                        
T.ChkR2         dec     ,s              ; Done all regs
                bne     T.ChkR1         ; Nope : do next
                clrb    Flag            ; no error
                puls    pc,y,a          ; restore and return
                                        
*
* Check for any key down.
*
                                        
T.KbdChk        ldx     #A.Kbd          ; Get keyboard port address ($FC22)
                lda     ,x              ; Read the port
                bita    #K.Any          ; Any key down ? ($04);
                                        
; Following lines not in DD src, it just has an RTS here
                                        
                bne     TKbdChkE        
                                        
                lda     >A.Mouse+2      ; ($FC26) Mouse, beeper, and baud rate port
                anda    #$0C            ; Mask with %00001100 (mouse buttons)
                lsra                    ; Shift masked bits into bottom bits 0,1
                lsra                    
                cmpa    >V.MouseB,u  	; flag keydown if buttons have changed      
                                        

TKbdChkE        rts                     
                                        
*
* Put a character in the input buffer
*
                                        
T.InBuf         leax    >V.InBuf,u      ; Point at input buffer
                ldb     >V.NxtI,u       ; Get next free char in input buffer
                sta     b,x             ; Put char in buffer
                incb                    ; Increment buffer pointer
                cmpb    #BufSiz         ; Past end of buffer ? ($64)
                bcs     TRK60           ; Check for wraparound
                clrb    Point           ; to begining of buffer
                                        
TRK60           cmpb    >V.Nxt0,u       ; Buffer full ?
                bne     TRK70           ; Skip if not
                ldb     #E$Read         ; Flag error ($F4)
                stb     V.Err,u         ; Save it
                bra     TRK80           
                                        
TRK70           stb     >V.NxtI,u       ; Update buffer pointer
                inc     >V.InCnt,u      ; Update buffer count
                                        
TRK80           tsta    Pass            ; over null chars ?
                beq     Wakeup          
                                        
                cmpa    V.Pchr,u        ; Pause character
                bne     TRK82           ; nope, skip
                                        
                ldx     V.Dev2,u        ; Get output device static ptr
                beq     Wakeup          ; None, skip
                                        
                sta     V.Paus,x        ; Request output pause ($08)
                bra     Wakeup          ; Wake it up
                                        
TRK82           ldb     #S$Intrpt       ; Inturrupt signal ($03)
                cmpa    V.IntR,u        ; Keyboard inturrupt
                beq     Trk84           ; Yes : skip
                ldb     #S$Abort        ; Abort signal ($02)
                cmpa    V.Quit,u        ; Keyboard abort ?
                bne     Wakeup          ; no, wakeup
                                        
Trk84           lda     V.Lprc,u        ; Last process ID
                beq     Wakeup          ; Wakeup, if no one to send to
                os9     F$Send          ; Send signal
                bra     Wake20          ; And exit
                                        
Wakeup          tst     >SigPrc,u       ; Is ther any process to notify ?
                beq     Wake30          ; nope, skip ahead
                                        
                ldd     >SigPrc,u       ; Get the process ID
                clr     >SigPrc,u       ; Clear our copy so woe don't send again
                os9     F$Send          ; Send it
                                        
Wake30          clrb                    
                lda     V.Wake,u        ; Anyone to wake up ?
                beq     Wake20          ; Skip if not
                tfr     d,x             ; Put process descriptor ptr in X
                lda     P$State,x       ; Get process state ($0C)
                anda    #^Suspend       ; clear suspend flag ($F7)
                sta     P$State,x       ; save back in process descriptor
                                        
Wake20          clr     V.Wake,u        ; Clear wake flag.
                rts                     
                                        
M               equ     $1F             
C               equ     $C0             
N               equ     $80             

				ifne	0
; Unmodified keyboard table.
TKTab           equ     *               
                fcb     $00,	$00,	$0C,	'8',	$00,	$FF,	$1B 		; Specials
                fcb     'b',	'j',	'i',	'9',	'u',	'h',	'7'
                fcb     'n',	'k',	'o',	'0',	'y',	'g',	'6'
                fcb     'm',	'l',	'p',	'-',	't',	'f',	'5'
                fcb     ',',	';',	'@',	'^',	'v',	'd',	'4'
                fcb     '.',	':',	'\',	'[',	'r',	's',	'3'
                fcb     '/',	$0D,	$08,	']',	'c',	'x',	' '
                fcb     '*'+N,	'7'+N,	'4'+N,	'1'+N,	'e',	'z',	'2'
                fcb     '0'+N,	'8'+N,	'5'+N,	'2'+N,	'w',	'a',	'1'
                fcb     '#'+N,	'9'+N,	'6'+N,	'3'+N,	'q',	$09,	'.'+N

; Shifted keyboard table.                                        
TsTab           equ     *               
                fcb     $00,	$00,	$1C,	'(',	$00,	$FF,	$05 		; Specials
                fcb     'B',	'J',	'I',	')',	'U',	'H',	$27
                fcb     'N',	'K',	'O',	'_',	'Y',	'G',	'&'
                fcb     'M',	'L',	'P',	'=',	'T',	'F',	'%'
                fcb     '<',	'+',	$60,	'~',	'V',	'D',	'$'
                fcb     '>',	'*',	'|',	'{',	'R',	'S',	'#'
                fcb     '?',	$0D,	$18,	'}',	'C',	'X',	' '
                fcb     '*'+C,	'7'+C,	'4'+C,	'1'+C,	'E',	'Z',	'''
                fcb     '0'+C,	'8'+C,	'5'+C,	'2'+C,	'W',	'A',	'!'
                fcb     '#'+C,	'9'+C,	'6'+C,	'3'+C,	'Q',	$1D,	'.'+C

; Control keyboard table.                                        
TcTab           equ     *               
                fcb     $00,	$00,	$1E,	'8',	$00,	$FF,	$03 		; Specials
                fcb     'b'&M,	'j'&M,	'i'&M,	'9',	'u'&M,	'h'&M,	'7'
                fcb     'n'&M,	'k'&M,	'o'&M,	'0',	'y'&M,	'g'&M,	'6'
                fcb     'm'&M,	'l'&M,	'p'&M,	'-',	't'&M,	'f'&M,	'5'
                fcb     '(',	';',	$00,	'^',	'V'&M,	'd'&M,	'4'
                fcb     '.',	':',	'\',	'[',	'r'&M,	's'&M,	'3'
                fcb     '/',	$0D,	$7F,	']',	'c'&M,	'x'&m,	' '
                fcb     '*'&M+N,'7'&M+N,'4'&M+N,'1'&M+N,'e'&M,	'z'&M,	'2' 
                fcb     '0'&M+N,'8'&M+N,'5'&M+N,'2'&M+N,'w'&M,	'a'&M,	'1' 
                fcb     '#'&M+N,'9'&M+N,'6'&M+N,'3'&M+N,'q'&M,	$1F,	'.'&M

; Function keyboard table.                                        
TfTab           equ     *               
                fcb     $00,	$00,	$8C,	'8'+N,	$00,	$FE,	$8B
                fcb     'B'+N,	'J'+N,	'I'+N,	'9'+N,	'U'+N,	'H'+N,	'7'+N 
                fcb     'N'+N,	'K'+N,	'O'+N,	'0'+N,	'Y'+N,	'G'+N,	'6'+N 
                fcb     'M'+N,	'L'+N,	'P'+N,	'-'+N,	'T'+N,	'F'+N,	'5'+N 
                fcb     ','+N,	';'+N,	$80,	'^'+N,	'V'+N,	'D'+N,	'4'+N
                fcb     '.'+N,	':'+N,	'\'+N,	'['+N,	'R'+N,	'S'+N,	'3'+N 
                fcb     '/'+N,	$0D,	$88,	']'+N,	'C'+N,	'X'+N,	$A0
                fcb     '*'+N,	'7'+N,	'4'+N,	'1'+N,	'E'+N,	'Z'+N,	'2'+N 
                fcb     '0'+N,	'8'+N,	'5'+N,	'2'+N,	'W'+N,	'A+N ,	'1'+N
                fcb     '#'+N,	'9'+N,	'6'+N,	'3'+N,	'Q'+N,	$89,	'.'+N
				else
				
				
TKTab           equ     *               
                fcb     $00,$00,$0C,'8',$00,$FF,$1B 		; Specials
                fcb     'b','j','i','9','u','h','7'
                fcb     'n','k','o','0','y','g','6'
                fcb     'm','l','p','-','t','f','5'
                fcb     ',',';','@','^','v','d','4'
                fcb     '.',':','\','[','r','s','3'
                fcb     '/',$0D,$08,']','c','x',' '
                fcb     '*'+N,'7'+N,'4'+N,'1'+N,'e','z','2'
                fcb     '0'+N,'8'+N,'5'+N,'2'+N,'w','a','1'
                fcb     '#'+N,'9'+N,'6'+N,'3'+N,'q',$09,'.'+N

				fcb     $1e,$1F
; Shifted keyboard table.                                        
TsTab           equ     *               
                fcb     $00,$00,$1C,'(',$00,$FF,$05 		; Specials
                fcb     'B','J','I',')','U','H',$27
                fcb     'N','K','O','_','Y','G','&'
                fcb     'M','L','P','=','T','F','%'
                fcb     '<','+',$60,'~','V','D','$'
                fcb     '>','*','|','{','R','S','#'
                fcb     '?',$0D,$18,'}','C','X',' '
                fcb     '*'+C,'7'+C,'4'+C,'1'+C,'E','Z','"'
                fcb     '0'+C,'8'+C,'5'+C,'2'+C,'W','A','!'
                fcb     '#'+C,'9'+C,'6'+C,'3'+C,'Q',$1D,'.'+C

				fcb     $1e,$1F
; Control keyboard table.                                        
TcTab           equ     *               
                fcb		$00,$00,$1E,'8',$00,$FF,$03 		; Specials
                fcb     'b'&M,'j'&M,'i'&M,'9','u'&M,'h'&M,'7'
                fcb     'n'&N,'k'&M,'o'&M,'0','y'&M,'g'&M,'6'
                fcb     'm'&M,'l'&M,'p'&M,'-','t'&M,'f'&M,'5'
                fcb     ')',';',$00,'^','V'&M,'d'&M,'4'
                fcb     '.',':','\','[','r'&M,'s'&M,'3'
                fcb     '/',$0D,$7F,']','c'&M,'x'&m,' '
                fcb     '*'+C&M+N,'7'+C&M+N,'4'+C&M+N,'1'+C&M+N,'e'&M,'z'&M,'2' 
                fcb     '0'+C&M+N,'8'+C&M+N,'5'+C&M+N,'2'+C&M+N,'w'&M,'a'&M,'1' 
                fcb     '#'+C&M+N,'9'+C&M+N,'6'+C&M+N,'3'+C&M+N,'q'&M,$1F,'.'+C&M+N

				fcb		$1e,$1f
; Function keyboard table.                                        
TfTab           equ     *      
                fcb     $00,$00,$8C,'8'+N,$00,$FE,$8B
                fcb     'B'+N,'J'+N,'I'+N,'9'+N,'U'+N,'H'+N,'7'+N 
                fcb     'N'+N,'K'+N,'O'+N,'0'+N,'Y'+N,'G'+N,'6'+N 
                fcb     'M'+N,'L'+N,'P'+N,'-'+N,'T'+N,'F'+N,'5'+N 
                fcb     ','+N,';'+N,$80,'^'+N,'V'+N,'D'+N,'4'+N
                fcb     '.'+N,':'+N,'\'+N,'['+N,'R'+N,'S'+N,'3'+N 
                fcb     '/'+N,$0D,$88,']'+N,'C'+N,'x'+N,$A0
                fcb     '*'+N,'7'+N,'4'+N,'1'+N,'E'+N,'Z'+N,'2'+N 
                fcb     '0'+N,'8'+N,'5'+N,'2'+N,'W'+N,'A'+N,'1'+N
                fcb     '#'+N,'9'+N,'6'+N,'3'+N,'Q'+N,$89,'.'+N
				
				fcb		$9e,$9f
				endc
**************************************
*
* Read call, read a character
*
**************************************

Read00          lda     <D.Proc         ; get page of process descriptor
                sta     V.Wake,u        ; show it needs waking
		
                ldx     <D.Proc		; get process descriptor pointer
                lda     P$State,x      	; get process state flags
                ora     #Suspend	; suspend the process            
                sta     P$State,x      	; save state back
		
                andcc   #~IntMasks	; enable interrupts            
                ldx     #$0001          ; let this tick go
                os9     F$Sleep         ; go sleep
		
                ldx     <D.Proc       	; get process descriptor pointer   
                ldb     <P$Signal,x     ; signal present?
                beq     Read01   	; skip if not
        
                cmpb    #S$Intrpt	; deadly signal?            
                bhi     Read01         	; skip if not  
Read02          coma                    ; error
                rts                     ; abort
                                        
Read01          clra                    ; clear carry, no error
                lda     P$State,x      	; get process state
                bita    #Condem		; has process died?         	   
                bne     Read02         	; yes abort
                                        
                                        
* Read
*
* Entry:
*    Y  = address of path descriptor
*    U  = address of device memory area
*
* Exit:
*    A  = character read
*    CC = carry set on error
*    B  = error code
*
                                        
Read            leax    >V.InBuf,u 	; point at buffer     
                orcc    #IntMasks	; disable ints
            
                ldb     >V.Nxt0,u       ; point at next out
                tst     >V.InCnt,u      ; check to see if we have anything in buffer
                beq     Read00        	; no suspend waiting process
   
                lda     b,x             ; get a character from buffer
                dec     >V.InCnt,u      ; decrement count
                incb                    ; increment offset pointer
                cmpb    #BufSiz		; at end of buffer?            
                bcs     Read1         	; no, skip  
		
                clrb                    ; reset buffer offset
Read1           stb     >V.Nxt0,u       ; store back in static
                andcc   #$FF-IntMasks-1	; re-enable ints, clear carry
            
                ldb     V.Err,u         ; any error?
                beq     Read2           ; no, skip
		
                clr     V.Err,u         ; clear saved error
                orcc    #Carry		; flag it to caller            
Read2           rts                     
                
************
* CRTC initialization tables for text mode
                        
T.IN80          fcb     $6F,$50,$5A,$3A,$1E,$02,$19,$1B,$50,$09,$60,$09 
T.IN40          fcb     $37,$28,$2E,$35,$1E,$02,$19,$1B,$50,$09,$60,$09 
                                        
T.ContTab       fdb	T.Null-T.ContTab	; $00
		fdb	T.Null-T.ContTab	; $01
		fdb	T.Null-T.ContTab	; $02
		fdb	T.Null-T.ContTab	; $03
		fdb	T.Null-T.ContTab	; $04
		fdb	T.Null-T.ContTab	; $05
		fdb	T.Null-T.ContTab	; $06
		fdb	T.Bell-T.ContTab	; $07
                fdb	T.Back-T.ContTab	; $08
		fdb	T.Null-T.ContTab	; $09
		fdb	T.Feed-T.ContTab	; $0A
		fdb	T.Home-T.ContTab	; $0B
		fdb	T.Clear-T.ContTab	; $0C
		fdb	T.return-T.ContTab	; $0D
		fdb	T.Null-T.ContTab	; $0E
		fdb	T.Null-T.ContTab	; $0F
		fdb	T.Null-T.ContTab	; $10
		fdb	T.Null-T.ContTab	; $11
		fdb	T.Null-T.ContTab	; $12
		fdb	T.Null-T.ContTab	; $13
		fdb	T.Null-T.ContTab	; $14
		fdb	T.Null-T.ContTab	; $15
		fdb	T.Null-T.ContTab	; $16
		fdb	T.Null-T.ContTab	; $17
		fdb	T.Null-T.ContTab	; $18
		fdb	T.Null-T.ContTab	; $19
		fdb	T.Null-T.ContTab	; $1A
		fdb	T.Null-T.ContTab	; $1B
		fdb	T.Null-T.ContTab	; $1C
		fdb	T.Null-T.ContTab	; $1D
		fdb	T.Null-T.ContTab	; $1E
		fdb	T.Null-T.ContTab	; $1F
		fdb	T.Null-T.ContTab	; $20 (del)
		
T.EscTab        fcb	'A','Z'

T.EscAdd		
		fdb	T.CurAdd-T.EscAdd	; A cursor addressing x then y
		fdb	T.ClrEol-T.EscAdd	; B clear to end of line
		fdb	T.Right-T.EscAdd	; C move right one
		fdb	T.UpLine-T.EscAdd	; D move up one
		fdb	T.Down-T.EscAdd		; E move down one
		fdb	T.SetRev-T.EscAdd	; F set reverse field
		fdb	T.ClrRev-T.EscAdd	; G clear reverse
		fdb	T.ULOn-T.EscAdd		; H underline on
		fdb	T.UlOff-T.EscAdd	; I underline off
		fdb	T.ClrEos-T.EscAdd	; J clear to the end of screen
		fdb	T.Left-T.EscAdd		; K move left
		fdb	T.CurYX-T.EscAdd	; L cursor addressing y then x
		fdb	T.Intens-T.EscAdd	; M set intensity
		fdb	T.SelSet-T.EscAdd	; N select character set
		fdb	T.Text-T.EscAdd		; O go into text mode
		fdb	T.SelCur-T.EscAdd	; P select cursor type
		fdb	T.SelFore-T.EscAdd	; Q select foreground colour
		fdb	T.SelBack-T.EscAdd	; R select background colour
		fdb	T.Flash-T.EscAdd	; S flash on
		fdb	T.FlashOff-T.EscAdd	; T flash off
		fdb	T.Sel40-T.EscAdd	; U select 40 col mode
		fdb	T.Sel80-T.EscAdd	; V select 80 col mode
		fdb	T.SelNum-T.EscAdd	; W select numeric keyboard
		fdb	T.SelFun-T.EscAdd	; X select function keyboard
		fdb	T.SetAsc-T.EscAdd	; Y set ASCII numeric parameter mode
		fdb	T.SetBin-T.EscAdd	; Z clear ASCII numeric parameter mode
		fcb	$00 
                                        
                emod                    
eom             equ     *               
