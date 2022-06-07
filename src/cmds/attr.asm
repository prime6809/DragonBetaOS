********************************************************************
* Attr - Modify file attributes
*
* $Id$
*
* Edt/Rev  YYYY/MM/DD  Modified by
* Comment
* ------------------------------------------------------------------

                nam     Attr            
                ttl     Modify          ; file attributes
                                        
* Disassembled 98/09/11 11:44:51 by Disasm v1.6 (C) 1988 by RML
                                        
                ifp1                    
                use     defsfile        
                endc                    
                                                                                
tylg            set     Prgrm+Objct     
atrv            set     ReEnt+rev       
rev             set     $01             
edition         set     8              
                                        
                mod     eom,name,tylg,atrv,start,size 
                                        
fpath           rmb     1    	; u0000 
devfid		rmb	1	          
parmptr         rmb     2       ; u0001        
cmdperms        rmb     2       ; u0002        
u0006           rmb     1               
u0007           rmb     1               
pathopts        rmb     20      ; u0008  
u001C    	rmb   	2
u001E    	rmb   	1
u001F    	rmb   	9      
dirent          rmb     32      ; u0028        
filename        rmb     32      ; u0048        
fdesc           rmb     16      ; u0068        
u0078           rmb     46              
u00A6           rmb     414             
size            equ     .               
                                        
name            fcs   	/Attr/ 
                fcb     edition         
                                        
HelpMsg         fcb     C$LF            
                fcc     'Use: Attr <pathname> {[-]<opts>}' 
                fcb     C$LF            
                fcc     ' opts: -d s r w e pr pw pe -a' 
                fcb     C$CR            
NotOwner        fcb     C$LF            
                fcc     'You do not own that file.' 
                fcb     C$CR            
UseMkDir        fcb     C$LF            
                fcc     'Use Makdir to create a directory' 
                fcb     C$CR            
DirNtEmt        fcb     C$LF            
                fcc     'ERROR; the directory is not empty' 
                fcb     C$CR            
Attrs           fcc   	'dsewrewr' 
                fcb     $FF             
                                        
start           stx     <parmptr     	; save param ptr
                clr     <u0007          
                com     <u0007          
		
                clra                    ; no mode
                os9     I$Open          ; open the fils
                bcc     L00D9           ; branch if ok
		
* If error, try to open as directory with read/write permissions
                ldx     <parmptr        ; get saved param ptr            
		lda     #DIR.+READ.    	; load perms
		os9     I$Open          ; open as directory
                bcc     L00D9           ; branch if ok
		
* One last time, try open as directory only
                ldx     <parmptr        ; get param ptr
                lda     #DIR.           ; load different perms
                os9     I$Open          ; try one more time
                bcs     L0114           ; branch if error
		
L00D9           sta     <fpath          ; save off path
                stx     <cmdperms       ; save updated parm ptr
                leax    pathopts,u      ; point X to buffer
                ldb     #SS.Opt         ; load with status code
                os9     I$GetStt        ; get status
                bcs     L0114           ; branch if error

		clrb  
		lda   	,x
		cmpa  	#$01
		lbne  	ShowHelp				
		
		ldx   	<parmptr	; point at parameters
		leay  	<filename,u	; point at filename
		lda   	,x+		; get char from parameters
		cmpa  	#'/'		; check for slash
		bne   	L0106		; no....

L00FA    	sta   	,y+		; save in filename buffer
		lda   	,x+		; get next char
		cmpa  	#'.'		; check for period
		bcs   	L0106		; yes, skip
		
		cmpa  	#'/'		; check for slash
		bne   	L00FA		; no loop again
		
L0106    	lda   	#'@'		; terminate dev name with @		
		ldb   	#' '
		std   	,y++
         
		leax  	<filename,u	; open file to entire block device
		lda   	#UPDAT.		; open for update
		os9   	I$Open
   
L0114    	lbcs  	ShowHelp	; error, show help
         
		sta   	<devfid		; save file id of dev file
		lda   	<fpath		; get file id of filename
		clr   	<u001F,u	
		
		pshs  	u		; save u
		ldx   	<u001C,u	; get seek pointer
		ldu   	<u001E,u
		lda   	<devfid		; get device file id
		os9   	I$Seek   	; seek to it
		puls  	u		; restore u
		
		bcs   	ShowHelp	; show help on error	
         
		leax  	<fdesc,u	; point to desc buffer 
		ldy   	#$0010		; 16 bytes
		os9   	I$Read   	; read into descriptor buffer
		bcs   	ShowHelp	; error, show help
						
                os9     F$ID            ; get ID
                cmpy    #$0000          ; super user?
                beq     L014B           ; branch if so
                cmpy    <fdesc+FD.OWN,u ; is user same as file's owner?
                bne     L01C1           ; branch if not

L014B           ldx     <cmdperms       ; point to perms on cmd line
                lbsr    L021D           
                bcs     L018B           
L0152           lbsr    L021D           
                bcc     L0152           
                clrb                    
                lda     ,x              
                cmpa    #C$CR           
                bne     ShowHelp        

		pshs  	u		; save u
		ldx   	<u001C,u	; get offset  
		ldu   	<u001E,u
		lda   	<devfid		; device file id
		os9   	I$Seek   	; seek to it
		puls  	u		; restore u
		bcs   	ShowHelp	; show help on error
		
		leax  	<fdesc,u	; point to desc buffer
		ldy   	#$0001		; write 1 byte
		os9   	I$Write  	; go write it
		bcs   	ShowHelp	; show help on error	
		
		os9   	I$Close  	; close dev file
		bcs   	ShowHelp	; show help on error
				
		lda     <fpath          ; get file path
                os9     I$Close         ; close file
                bcs     ShowHelp        ; branch if error
                ldb     <u0007          
                beq     Exit           

L018B           ldb     <fdesc,u        ; get attribute
                leax    >Attrs,pcr      
                leay    <u0078,u        
                lda     ,x+             
L0197           lslb                    
                bcs     L019C           
                lda     #'-'             
L019C           sta     ,y+             
                lda     ,x+             
                bpl     L0197           
                lda     #C$CR           
                sta     ,y+             
                leax    <u0078,u        
                clrb                    
                bra     L01B0           
ShowHelp        equ     *               
                leax    >HelpMsg,pcr    
                
L01B0           pshs    b               
                lda     #2              
                ldy     #256            
                os9     I$WritLn        
                comb                    
                puls    b               
Exit            os9     F$Exit    
      
L01C1           clrb                    
                leax    >NotOwner,pcr   
                bra     L01B0           
L01C8           leax    >UseMkDir,pcr   
                clrb                    
                bra     L01B0           
L01CF           pshs    u,y,x           
                lda     <fpath          
                ldx     #$0000          
                ldu     #DIR.SZ*2       
                os9     I$Seek          
                ldu     $04,s           
                bcs     Exit           
L01E0           leax    <dirent,u       
                ldy     #DIR.SZ         
                os9     I$Read          
                bcs     L01F7           
                tst     ,x              
                beq     L01E0           
                leax    >DirNtEmt,pcr   
                clrb                    
                bra     L01B0           
L01F7           puls    u,y,x           
                cmpb    #E$EOF          
                bne     ShowHelp        
                rts                     
L01FE           fdb     $ff41           
                fdb     $ff80,$44ff,$4053,$ff01,$52ff,$0257,$ff04,$45ff 
                fdb     $0850,$52ff,$1050,$57ff,$2050,$45ff 
                fcb     $00             
L021D           clr     <u0006          
L021F           lda     ,x+             
                cmpa    #C$SPAC         
                beq     L021F           
                cmpa    #C$COMA         
                beq     L021F           
                cmpa    #'-             
                bne     L0231           
                com     <u0006          
                lda     ,x+             
L0231           leax    -1,x            
                leay    >L01FE,pcr      
L0237           ldb     ,y+             
                pshs    y,x             
                beq     L027F           
L023D           lda     ,x+             
                eora    ,y+             
                anda    #$DF            
                beq     L023D           
                lda     -1,y            
                bmi     L0251           
                puls    y,x             
L024B           lda     ,y+             
                bpl     L024B           
                bra     L0237           
L0251           lda     ,-x             
                cmpa    #$30            
                bcc     L027F           
                cmpb    #$FF            
                beq     L0278           
                bitb    #$80            
                beq     L0268           
                tst     <u0006          
                lbeq    L01C8           
                lbsr    L01CF           
L0268           puls    y,b,a           
                lda     <fdesc,u        
                eora    <u0006          
                ora     -$01,y          
                eora    <u0006          
                sta     <fdesc,u        
                clrb                    
                rts                     
L0278           eorb    <u0006          
                stb     <u0007          
                clrb                    
                puls    pc,y,b,a        
L027F           coma                    
                puls    pc,y,x          
                                        
                emod                    
eom             equ     *               
                end                     
