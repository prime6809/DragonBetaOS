********************************************************************
* p[wx]d - Print work/execution directory
*
* $Id$
*
* Edt/Rev  YYYY/MM/DD  Modified by
* Comment
* ------------------------------------------------------------------
*   1      ????/??/??
* From Tandy OS-9 Level One VR 02.00.00.
*
* Modified to match the disassembly of the Dragon Beta PWD / PXD.
* This has meant backing out some of the changes made to the Nitros version.
* as I wanted this to assemble to be binary exactly equal to the original
* binary.
*
                                        
                nam     p[wx]d          
                ttl     Print           ; work/execution directory
                                        
* Disassembled 98/09/10 23:50:10 by Disasm v1.6 (C) 1988 by RML
                                        
                ifp1                    
                use     defsfile        
                endc                    
                                        
tylg            set     Prgrm+Objct     
atrv            set     ReEnt+rev       
rev             set     $01             
edition         set     1               
                                        
                mod     eom,name,tylg,atrv,start,size 
                                        
                org     0               
fildes          rmb     1               
bufptr          rmb     2               
dotdotfd        rmb     3               ; LSN of ..
dotfd           rmb     3               ; LSN of .
ddcopy          rmb     5               
dentry          rmb     160             
buffer          rmb     1               
sttbuf          rmb     282             
size            equ     .               
                                        
                ifne    PXD             
name            fcs     /pxd/           
                else                    
                ifne    PWD             
name            fcs     /pwd/           
                endc                    
                endc                    
                fcb     edition         
                                        
                ifne    PXD             
badnam          fcc     'pxd'           
                else                    
                ifne    PWD             
badnam          fcc     'pwd'           
                endc                    
                endc                    
                fcc     ': bad name in path' 
                fcb     C$CR            
dotdot          fcc     '.'             
dot             fcc     '.'             
cr              fcb     C$CR            
rdmsg           fcc     'read error'    
                fcb     C$CR            

                ifne    PXD             
openattr        equ	DIR.+EXEC.+READ.           
                else                    
                ifne    PWD             
openattr        equ	DIR.+READ.
                endc                    
                endc                    
	
                                        
start           leax    buffer,u        ; point X to buffer
                lda     #C$CR           ; get CR
                sta     ,x              ; store at start of buffer
                stx     <bufptr         ; store buffer pointer
                leax    >dot,pcr         ; point to '.'
                bsr     open            ; open directory
                sta     <fildes         ; save path
                lbsr    rdtwo           ; read '.' and '..' entries
                ldd     <dotdotfd       ; get 24 bit LSN of ..
                std     <ddcopy         
                lda     <dotdotfd+2     
                sta     <ddcopy+2       ; and save copy
pdloop                                  
* Inlined the atroot routine - BGP 03/09/06
;                leax    dotdotfd,u      ; point X at .. entry
;                leay    dotfd,u         ; point Y at . entry
;                bsr     attop           ; check if we're at the top
		bsr	atroot	
                beq     there           ; branch if so
                leax    >dotdot,pcr      ; else point to '..'
* Inlined the chdir routine - BGP 03/09/06
;                ifne    PXD             
;                lda     #DIR.+EXEC.+READ. 
;                else                    
;                ifne    PWD             
;                lda     #DIR.+READ.     
;                endc                    
;                endc                    
;                os9     I$ChgDir        
		bsr	chdir
                lda     <fildes         ; get path to previous dir
                os9     I$Close         ; close it
                bcs     exit            ; branch if error
                leax    >dot,pcr         ; point X to new current dir
                bsr     open            ; open it
                bsr     rdtwo           ; read . and .. entires of this dir
                bsr     findmtch        ; search for match
                bsr     prsent          
                ldd     <dotdotfd       
                std     <ddcopy         
                lda     <dotdotfd+2     
                sta     <ddcopy+2       
                bra     pdloop          
there                                   
* Inlined the getdevnm routine - BGP 03/09/06
		lbsr	getdevname
		
;                lda     <fildes         
;                ldb     #SS.DevNm       
;                leax    sttbuf,u        
;                os9     I$GetStt        
;                bsr     prsnam          
                                        
                ldx     <bufptr         ; point to buffer
                ldy     #$0081          ; get bytes to write
                lda     #$01            ; to stdout
                os9     I$WritLn        ; write
                lda     <fildes         ; get path
                os9     I$Close         ; close
                clrb                    
exit		os9   	F$Exit            
;		lbra    exit1           

chdir		lda     #openattr
                os9     I$ChgDir        
		rts   
                                      
open            lda     #openattr
                os9     I$Open          
                rts                     
                                        
* Read directory entry
readent         lda     <fildes         
                leax    dentry,u        
                ldy     #DIR.SZ         
                os9     I$Read          
                rts                     
                                        
findmtch        lda     <fildes         ; get path to current dir
                bsr     readent         ; read entry
                bcs     cantread        ; branch if error
                leax    dentry,u        ; point to entry buffer
                leax    <DIR.FD,x       ; point X to FD LSN
                leay    ddcopy,u        ; point Y to copy of LSN
                bsr     attop           ; compare the two
                bne     FindMtch        ; keep reading until we find match
                rts                     
                                        
* Compare 3 bytes at X and Y
attop           ldd     ,x++            
                cmpd    ,y++            
                bne     L00C5           
                lda     ,x              
                cmpa    ,y              
L00C5           rts                     

; From DD beta src
atroot		leax  	dotdotfd,u	; point X at .. entry
		leay  	dotfd,u		; point Y at . entry
		bsr   	attop		; compare
		rts                         
                  
rdtwo           bsr     readent         ; * read '.' from directory
                ldd     <dentry+DIR.FD  
                std     <dotfd          
                lda     <dentry+DIR.FD+2 
                sta     <dotfd+2        
                bsr     readent         ; * read '..' from directory
                ldd     <dentry+DIR.FD  
                std     <dotdotfd       
                lda     <dentry+DIR.FD+2 
                sta     <dotdotfd+2     
                rts                     
                                        
* Get name from directory entry
prsent          leax    dentry,u        
prsnam          os9     F$PrsNam        
                bcs     IlglName        
                ldx     <bufptr         
prsloop         lda     ,-y             
                anda    #$7F            ; mask hi bit
                sta     ,-x             ; save
                decb                    
                bne     prsloop         
                lda     #PDELIM         
                sta     ,-x             
                stx     <bufptr         
                rts                     
                                        
getdevname      lda     <fildes         
                ldb     #SS.DevNm       
                leax    sttbuf,u        
                os9     I$GetStt        
                bsr     prsnam          
		rts

IlglName        leax    badnam,pcr      
                bra     wrlnerr           
                                        
cantread        leax    rdmsg,pcr 
		bra   	wrlnerr
wrerr           lda     #$02            
                os9     I$Write
		bcs	L0128
		rts

		bsr	wrerr
		leax	>cr,pcr
wrlnerr		lda	#$02
		os9   	I$WritLn
L0128		ldb	#$00
exit1           os9     F$Exit          
                                        
                emod                    
eom             equ     *               
                end                     
                                        
