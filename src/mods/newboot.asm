                nam     Boot            
                                        
                ttl     General         ; bootstrap module
                                        
****************************************
*
* Called by OS-9 on reset if IOMAN can't
* be found in ROM.
* Links to the device descriptor whose
* name is embedded in the program, and
* calls the boot entry of its device driver,
* with the Y register pointing at the
* device descriptor.
*
****************************************
                                        
****************************************
*
* Written by Paul Dayan 20th August 1982
*
* (c) Vivaway Ltd 1982
*
****************************************
*
* Multi-Boot, by Phill Harvey-Smith 2022-05-01
*
****************************************
                                        
                use     defsfile        
                                        
                ttl     General         ; bootstrap module
                                        
Type            set     SYSTM+OBJCT     
Revs            set     REENT+1         
                                        
                mod     Btend,Btnam,Type,Revs,Btent,0 
                                        
                fcb     $FF             ; capabilities
                                        
Btnam           fcs     'Boot'          
                                        
Edition         fcb     1               
   
BootEntry	equ	D$TERM+3	; offset of boot entry for block device drivers   
   
****************************************
*
Devnam
		fcs     'bootdev'       ; name of device(s) to boot from
		fcs	'bootdev2'
		fcb	0		; zero terminator
*
****************************************
                fcc	'newboot'
                        
Btent           pshs    u,y   

                leax    >Devnam-1,pcr   ; point at name table
TableNext	leax	1,x		; point to start of next name	
		lda	,x		; end of table?
		beq	TabEnd		; yes, exit with error
		
		pshs	x		; save pointer
		
		bsr	TryBoot		; try booting this device
		bcc	BootOk		; boot ok, exit
		
		puls	x		; retrieve table pointer
		
TableNextChar	leax	1,x		; move to next byte
		tst	,x		; test byte
		bmi	TableNext	; end of name
		bra	TableNextChar	; scan next char
		

TabEnd          orcc	#Carry		; set carry flag for error
		puls    pc,u,y
		
BootOk		leas	2,s		; drop table pointer
		puls    pc,u,y          
      
          
TryBoot         lda     #DEVIC+OBJCT  	; link the module  
                os9     F$Link          

                bcs     Bterr2          ; abort if can't find it

                pshs    u               ; save module address
		ldd     M$PDev,u      	; get offset to driver name
                leax    d,u             ; point at driver name
                lda     #DRIVR+OBJCT    ; link to device driver
                os9     F$Link          

                leax    BootEntry,y     ; entry point for boot routine
                ldy     ,s              ; get device descriptor pointer in Y
                bcs     Bterr1          ; abort if can't find driver

                pshs    u               ; save address of driver module
                jsr     ,x              ; call boot routine

                puls    u               ; unlink device driver
                pshs    x,b,a,cc        ; save results
                os9     F$UnLink        

                puls    x,b,a,cc        ; retrieve results
                                        
Bterr1          puls    u               ; unlink device descriptor
                pshs    x,b,a,cc        ; save results
                os9     F$UnLink        

                puls    x,b,a,cc        ; retrieve results
                                        
Bterr2          rts
			
                emod                    
Btend           equ     *               
