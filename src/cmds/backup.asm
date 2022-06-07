                nam     Backup          
                ttl     program         ; module
                                        
* Disassembled 1900/00/00 00:11:42 by Disasm v1.5 (C) 1988 by RML
                                        
                use     defsfile 

StdIn		equ	0
StdOut		equ	1
StdErr		equ	2
				
tylg            set     Prgrm+Objct     
atrv            set     ReEnt+rev       
rev             set     $01  
edition		set 	7

                mod     eom,name,tylg,atrv,start,size 

BackBuffSize	equ	14*256		; backup buffer size

SrcFID          rmb     1               ; source file id
DestFID         rmb     1               ; destination file id
TempPtr         rmb     2               
u0004           rmb     5               
ZerosFlag       rmb     1		; zero supression flag for hex output
NoPrompt        rmb     1               ; no prompt to retry on error?
PromSingle      rmb     1               ; prompt for disk swap on single drive
NoVerify        rmb     1         	; no verification flag      
SrcErr          rmb     1               ; source disk error code
CurrLSN         rmb     3		; Current LSN.               
SecBufPtr       rmb     2               ; Sector buffer pointer
numpages        rmb     1               ; number of pages available for backup
PageCount       rmb     1               ; page counter whilst reading / writing sectors
DestNameBuf     rmb     32              ; destination name buffer
DestStatBuf     rmb     32              
BufPtr          rmb     2               
StrBuf          rmb     424             
Stack           rmb     81              
; DestLSN0 and SrcLSN0 *MUST* be together and in this order!
DestLSN0        rmb     256             ; destination disk LSN0 buffer
SrcLSN0         rmb     256		; source disk LSN0 buffer
backbuf		rmb	BackBuffSize
		
size            equ     .           
    
name            equ     *               
                fcs     /Backup/        
                fcb     edition            

defparms 	fcc   	"/d0 /d1"
		fcb   	C$CR
		
HelpMsg         fcb     C$LF
		fcc   	"Use: Backup [e] [s] [-v] [/dev1 [/dev2]]"
		fcb   	C$LF
		fcc   	"  e - abort if read error"
		fcb   	C$LF
		fcc   	"  s - single drive pause messages"
		fcb   	C$LF
		fcc   	" -v - inhibit verify pass"

EolMess          	fcb   	$80+C$CR            
ReadPrompt      fcc   	"Ready to backup from"
Space           fcs     " "             
To          	fcs	" to "
OK          	fcc	"Ok"
ask		fcs	" ?: "          	
ReadySrc        fcs   	"Ready Source, hit a key: "
ReadyDest       fcs   	"Ready Destination, hit a key: "
Sector          fcs	"Sector $"
SectorsCpy      fcs	"Number of sectors copied: $"
VerifyP         fcb   	C$LF
		fcc   	"Verify pass"
		fcb   	$80+C$CR
		
SectorsVer      fcs	"Number of sectors verified: $"
Scratched       fcc   	" is being scratched"
		fcb   	$80+C$CR

NotSame         fcc   	"Disks are not formatted identically"
		fcb   	C$LF
		
Aborted         fcc   	"Backup Aborted"
		fcb   	$80+C$CR
		
start           equ     *               
                leas    >Stack,u	; move stack here        
                pshs    d		; save param area size	             
                pshs    u               ; save lowest address
                tfr     y,d             ; move top of param area to d
                subd    ,s++            ; subtract bottom, leaving size of param area
                subd    #(BackBuf-Stack)-1  ; a is number of 256 byte pages         
                sta     <numpages       ; save number of pages
	   
                clr     <PromSingle          
                clr     <NoPrompt          
                clr     <NoVerify          
                clr     <SrcErr          

                leay    <StrBuf,u	; get addrress of our buffer        
                sty     <BufPtr 	; save it's address         
                ldd     ,s++            ; get parameter length
                beq     UseDefaults     ; no params, use defaults      
		
L01A9           ldd     ,x+             ; get 2 bytes from command line
                cmpa    #' '		; space?            
                beq     L01A9		; yes, loop again
           
                cmpa    #','		; Comma?            
                beq     L01A9           ; yes, loop again
		
                eora    #'E'		; check for abort if read error flag            
                anda    #$DF            ; mask it
                bne     L01C1           ; nope, skip
		
                cmpb    #'0'		; branch if character after option is > $30            
                bcc     L01C1           
		
                inc     <NoPrompt          
                bra     L01A9    	; loop again
       
L01C1           lda     -$01,x          ; load a with previous character
                eora    #'S'  		; check for 'prompt to swap disks flag          
                anda    #$DF            ; mask it
                bne     L01D1           ; nope skip ahead
		
                cmpb    #'0'		; charcter following option > $30?            
                bcc     L01D1		; yes, skip on           
		
                inc     <PromSingle     ; set single drive flag     
                bra     L01A9           ; loop for next
		
L01D1           ldd     -$01,x          ; load previous characters
                cmpa    #'-'        	; check for '-'    
                bne     L01E7           ; nope, skip on
		
                eorb    #'V'		; check for 'V'           
                andb    #$DF            ; mask it
                bne     L01E7           ; nope, skip on
		
                ldd     ,x+             ; check for char after option > '0'
                cmpb    #'0'            
                bcc     L01E7		; yes, skip
           
                inc     <NoVerify  	; flag no verify pass        
                bra     L01A9           ; loop again
		
L01E7           lda     ,-x             ; backup pointer
                cmpa    #'/'		; path delimiter?            
                beq     L01F7           ; yes, go read paths
                cmpa    #$0D            ; End of line?
                lbne    ShowHelp           ; yes, skip on, invalid params?
		
UseDefaults     leax    >defparms,pcr   ; point to default params   
L01F7           leay    >ReadPrompt,pcr ; print reading disk prompt     
                lbsr    StrYtoBuf	; copy it to buffer
           
                ldy     <BufPtr         ; get buffer pointer
                sty     <TempPtr          
                lbsr    ParseNameToBuf	; parse the name and copy to buffer		    
       
L0207           lda     ,x+           	; get next character from parameters  
                cmpa    #' '		; space?            
                beq     L0207           ; yes, loop for next
		
                cmpa    #','		; comma?            
                beq     L0207           ; yes, loop for next
		
                cmpa    #C$CR		; end of input?            
                bne     L021B           ; no, skip
		
                inc     <PromSingle  	; only 1 drive specified therefore single drive        
                ldx     <TempPtr          ; restore buffer pointer
		
                lda     ,x+             ; get a char
L021B           cmpa    #'/'            ; path delimiter?
                lbne    ShowHelp        ; nope, show help
		
                leax    -$01,x  	; backup buffer pointer        
                leay    >To,pcr      	; point to 'to '
                lbsr    StrYtoBuf      	; move it to output buffer     
                ldy     <BufPtr         ; get current buffer pointer 
                sty     <u0004          ; save it
                lbsr    ParseNameToBuf 	; parse 'to' drive to buffer          
                leay    >ask,pcr      	; point to ask prompt
                lbsr    PromptUser      ; prompt user     
		
                comb                    
                eora    #'Y'		; check for afermative response            
                anda    #$DF            ; mask it
                lbne    ExitNoError     ; no, exit      
		
                ldx     <TempPtr          
                ldd     #'@'*256+' '    ; terminate path with raw device flag
L0248           cmpb    ,x+    		; skip past any spaces         
                bne     L0248           
		
                std     -$01,x          
                ldx     <TempPtr 	; point to source disk name         
                lda     #READ.		; open for read            
                os9     I$Open          
                bcs     L028C           ; error, skip
		
                leax    >SrcLSN0,u 	; point at buffer for LSN0 of source disk       
                ldy     #$0100          ; read one sector
                os9     I$Read          ; go read it
                bcs     L028C           ; error, skip
		
                os9     I$Close         ; close source disk file
		
                ldx     <TempPtr 	; point to source disk name
                lda     #READ.		; open for read            
                os9     I$Open          ; do it
                bcs     L028C           ; error, skip
                
		sta     <SrcFID   	; save file id       
                ldx     <u0004          ; load saved dest buff position
                leay    <DestNameBuf,u	; point at destination name buff	        
L0277           ldb     ,x+             ; transfer a character
                stb     ,y+             
                cmpb    #' '		; space?            
                bne     L0277           ; no loop again
		
                ldd     #'@'*256+' '	; terminate path with raw device flag   
                std     -$01,y          
                leax    <DestNameBuf,u	; point to destination name        
                lda     #UPDAT.		; open for update            
                os9     I$Open          ; open it

L028C           lbcs    Abort           ; error, skip
                sta     <DestFID       	; save destination disk file id   
                clr     <CurrLSN        ; zero current LSN  
                clr     <CurrLSN+1          
                clr     <CurrLSN+2          
                lbsr    SwapPrompt           
                
		lda     <DestFID      	; get destination disk file ID    
                leax    >DestLSN0,u    	; point to buffer to read LSN into    
                ldy     #$0100          ; read one sector
                os9     I$Read          ; go read it
		
                pshs    u,x             ; save regs
                ldx     #$0000          ; filepos = 0
                leau    ,x              
                os9     I$Seek          ; seek back to beginning of destination disk
                puls    u,x             ; restore regs
			
                bcs     L028C           ; error, skip
		
; Compare LSN0 from source and destination, assumes they are in contiguous memory locations		
                ldd     >$0100,x        ; get first word from source LSN0
                cmpd    ,x              ; compare to first word in destination LSN0
                bne     L02C7           ; not equal, skip

                ldb     >$0102,x        ; get second word from source LSN0
                cmpb    $02,x           ; compare to second word in destination LSN0
                beq     L02CE           ; all OK, continue
		
L02C7           leay    >NotSame,pcr    ; point to not same type of disk error  
                lbra    WriteErrorExit	; write error and exit.           
		
L02CE           leax    >DestLSN0,u    	; point at destination LSN0    
                lda     #$BF 		           
                sta     <DD.OPT,x       ; overwrite opts   
                leay    <DD.NAM,x      	; point at dest disk name in LSN0    
                lbsr    StrYtoBuf      	; copy it to buffer     
                leay    >Scratched,pcr 	; point to scratched message     
                lbsr    WriteError     	; write it      
                leay    >OK,pcr      	; point to OK message
                lbsr    PromptUser     	; prompt user to continue
      
                comb                    
                eora    #'Y'		; was it yes?            
                anda    #$DF            ; make it
                lbne    ExitNoError   	; no, exit
        
                lda     <DestFID       	; get destination file id   
                leax    >SrcLSN0,u    	; point to source LSN0    
                ldy     #$0100          ; one sector worth
                os9     I$Write         ; write it to destination
                lbcs    Abort           ; error, skip
		
                pshs    u               ; save u
                ldx     #$0000          ; filepointer=0
                leau    ,x              
                os9     I$Seek          ; seek to it
                puls    u               ; restore u
		
                leax    >SrcLSN0,u	; point to source LSN0        
                os9     I$Read          ; read 		
                lbcs    Abort           ; error, skip
		
                os9     I$Close         ; close destination disk
                
		leax    <DestNameBuf,u	; point to destination disk name        
                lda     #WRITE.		; open for write            
                os9     I$Open          ; do open
                lbcs    Abort           ; error, skip
		
                sta     <DestFID      	; save new destination file ID    
                leax    <DestStatBuf,u  ; point at destination stats buffer
                ldb     #SS.Opt		; get option area            
                os9     I$GetStt        ; go get them
                
		ldb     #$01            ; flag verify writes
                stb     (PD.VFY-PD.OPT),x     	; store in path buffer    
                ldb     #SS.Opt		; set option area            
                os9     I$SetStt        ; go set it
                lbcs    Abort           ; branch on error
		
L0342           leay    >ReadySrc,pcr  	; Point to ready source disk message    
                lbsr    CondPrompt      ; conditional prompt     
                
		lda     <numpages     	; get number of pages for buffer     
                sta     <PageCount      ; save it in counter
                leax    >DestLSN0,u   	; point at destination LSN0     
                lbsr    FillBuffSrc     ; fill buffer from source disk      
                lbsr    SwapPrompt    	; prompt to swap disks if needed       
		
                ldd     <SecBufPtr  	; get sector buffer pointer        
                leax    >DestLSN0,u    	; point to destination LSN0 (256 bytes before sec buffer)    
                stx     <SecBufPtr   	; save in sector buffer pointer       
                subd    <SecBufPtr   	; subtract old buffer pointer       
                beq     L036C           ; branch if buffer empty?
                
		tfr     d,y             ; move buffer size to y (write length)
                lda     <DestFID        ; get destination file id  
                os9     I$Write         ; go write it
                
		bcs     Abort           ; error, abort
		
L036C           lda     <SrcErr        	; get source error  
                cmpa    #$D3            ; end of file?
                bne     L0342           ; no.....
		
                leay    >SectorsCpy,pcr	; point to sectors copied message      
                lbsr    FmtCopiedMsg    ; format and output message and LSN       
                
		tst     <NoVerify     	; should we verify copy?     
                bne     ExitNoError   	; no, exit, all done
        
                leay    >VerifyP,pcr 	; point to verify message     
                lbsr    WriteError    	; and output it
       
                lda     <SrcFID     	; get source file id     
                os9     I$Close         ; close it
                bcs     Abort           ; on error, abort
		
                lda     <DestFID     	; get destination file id     
                os9     I$Close         ; close it
                bcs     Abort           ; on error, abort
		
                leax    <DestNameBuf,u	; point to destination filename        
                lda     #READ.		; open for read            
                os9     I$Open          ; open it
                bcs     Abort           ; on error, abort
                
		sta     <SrcFID        	; save file id  
                clr     <CurrLSN      	; zero current LSN    
                clr     <CurrLSN+1          
                clr     <CurrLSN+2          
                clr     <SrcErr          
		
L03A6           lda     <numpages     	; get number of pages available     
                sta     <PageCount    	; set page count  	    
                leax    >DestLSN0,u  	; point at LSN0      
                bsr     FillBuffSrc   	; fill the buffer        
		
                lda     <SrcErr        	; any errors?  
                cmpa    #$D3          	; end of file?  
                bne     L03A6          	; no keep going
 
                leay    >SectorsVer,pcr	; point a sectors verified message      
                lbsr    FmtCopiedMsg   	; output it        
                bra     ExitNoError    	; and exit, all done       

Abort           os9     F$PErr		; print error code in b          
                leay    >Aborted,pcr  	; point at aborted message    

WriteErrorExit  lbsr    WriteError           
                comb               	; flag error
     
ExitNoError     ldb     #$00            ; flag no error!
                os9     F$Exit          ; exit
		
ReadOneSec      ldy     #$0100 		; one sector's worth         
                lda     <SrcFID       	; get source file id   
                os9     I$Read          ; go read it
                bcc     L03EC           ; all ok, skip on
		
                stb     <SrcErr 	; save souce error         
                cmpb    #$D3            ; end of file?
                beq     L041D           ; yes, deal with it, return to caller
		
                lbsr    FmtSecErr     	; format sector error in buffer      
                ldb     <SrcErr        	; get error code  
                tst     <NoPrompt     	; should we prompt user?     
                bne     Abort           ; nope, abort
		
                os9     F$PErr          ; print error
		
L03EC           ldd     <CurrLSN+1   	; increment current LSN       
                addd    #$0001          ; LSW
                std     <CurrLSN+1      ; save it back    
                bcc     L03F7          	; if we have carry MSB needs adjusting 
                inc     <CurrLSN        ; so increment by 1  
		
L03F7           tst     <SrcErr       	; error on source read?   
                beq     L040D           ; no, branch ahead, read next sec
		
                pshs    u               ; save u
                ldx     <CurrLSN        ; get MSW of current LSN  
                tfr     b,a             ; convert it to a byte offset
                clrb                    ; by effectivly multiplying by 256
                tfr     d,u             ; so byte offset in x:u
		
                lda     <SrcFID         ; get source file id
                os9     I$Seek          ; seek back ready to retry?
                puls    u               ; restore u
                clr     <SrcErr         ; clear error
L040D           ldx     <SecBufPtr    	; get sector buffer pointer      
                leax    >$0100,x        ; increment 1 sector

FillBuffSrc     stx     <SecBufPtr	; save sector buffer pointer          
                lda     <PageCount    	; get page count      
                suba    #$01           	; subtract 1  
                sta     <PageCount      ; save page count back    
                bcc     ReadOneSec      ; nonzero, branch buffer not yet full
L041D           rts                     

ShowHelp        leax    <StrBuf,u 	; point to string buffer       
                stx     <BufPtr        	; save pointer in buffer pointer  
                leay    >HelpMsg,pcr   	; point to help text	   
                bra     WriteErrorExit	; go write it and exit....
           
SwapPrompt      leay    >ReadyDest,pcr 	; point to ready destination message     
CondPrompt      tst     <PromSingle    	; should we prompt?      
                beq     L0449           ; no, return

; neat code, psuhes regs then does a read of 1 byte to the stack, so that when
; the regs are pulled a contains the read character.
PromptUser      bsr     WriteError   	; write message from buffer        
                pshs    y,x,b,a         ; save regs
                leax    ,s              ; read to stack.....
                ldy     #$0001          ; one byte
                clra                    ; from standard in
                os9     I$Read          ; go read it
		
                leay    >EolMess,pcr   	; point at EOL sequence   
                bsr     WriteError     	; write it      
                puls    y,x,b,a         ; restore regs
                anda    #$7F            ; mask read byte
L0449           rts                     ; return

ParseNameToBuf  pshs    x               
                os9     F$PrsNam        
                puls    x               
                bcs     ShowHelp           

L0453           lda     ,x+             
                bsr     StoreAInBuf           
                decb                    
                bpl     L0453           
                rts                     

StrYtoBuf       lda     ,y 		; get byte from message             
                anda    #$7F            ; reset bit 7
                bsr     StoreAInBuf     ; store it in buff      
                lda     ,y+             ; get next char from string
                bpl     StrYtoBuf       ; loop again if not end of string
L0465           rts                     ; return

WriteError      bsr     StrYtoBuf	; move string to buffer           
                pshs    y,x,a           ; save regs
                ldd     <BufPtr 	; get buffer pointer
                leax    <StrBuf,u     	; bufptr:=strbuf   
                stx     <BufPtr          
                subd    <BufPtr         ; d=BufPtr-StrBuf
                tfr     d,y          	; number of bytes to write   	
                lda     #StdErr      	; write to standard error      
                os9     I$WritLn        ; go write it
                puls    pc,y,x,a	; restore and return
        
FmtSecErr       leay    >Sector,pcr   	; sector message
FmtCopiedMsg    bsr     StrYtoBuf     	; copy to buffer      
                lda     <CurrLSN 	; output current LSN MSB        
                bsr     DoHexOutStart   ; output in hex
     
                inc     <ZerosFlag          
                lda     <CurrLSN+1     	; get middle byte of LSN     
                bsr     DoHexOut        ; output in hex
		
                lda     <CurrLSN+2      ; and LSB of lsn.....    
                bsr     DoHexOut        ; output in hex   
                leay    >Space,pcr      ; point at space
                bra     WriteError      ; output error     

; hex conversion
DoHexOutStart   clr     <ZerosFlag       ; clear flag   
DoHexOut        pshs    a               ; save a
                lsra                    ; get MSN of a
                lsra                    
                lsra                    
                lsra                    
                bsr     L04A4           ; convert top to hex
		
                puls    a               ; recover a
                anda    #$0F            ; convert bottom nibble

L04A4           tsta    		; is a zero                
                beq     L04A9           ; yes, skip
                sta     <ZerosFlag     	; no, flag nonzero     
		
L04A9           tst     <ZerosFlag     	; test zero flag  	   
                beq     L0465           ; yes : rts
		
                adda    #'0'		; convert to ASCII            
                cmpa    #'9'		; bigger than '9'            
                bls     StoreAInBuf     ; no, store in buffer      
                adda    #$07            ; convert to 'A'..'Z'

StoreAInBuf     pshs    x               ; save x
                ldx     <BufPtr       	; point x at buffer   
                sta     ,x+             ; save a in buffer
                stx     <BufPtr         ; save buffer pointer
                puls    pc,x            ; restore and return
                emod                    
eom             equ     *               
                end                     
