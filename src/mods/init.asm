                nam     Init            
                ttl     OS-9            ; Configuration Module
                                        
************************************************************
*
*     Configuration Module
*
                                        
                use     defsfile        

*
* configuration constants
*
TopRAM1         equ     $0B             ; Top of RAM MSB
TopRAM2         equ     $FF             ; Top of RAM middle byte
TopRAM3         equ     $FF             ; Top of RAM LSB
SizPoll         equ     14              ; irq polling table size
SizDevic        equ     14              ; device table size

ExecOff		equ	(TopRAM1*$100)+TopRAM2
DataSz		equ	(TopRAM3*$100)+SizPoll

; Not the Dragon Data source file, only has 4 parameters to mod, but lwasm
; flags this as an error! 
                                        
                mod     ConEnd,ConNam,systm,reent+1,ExecOff,DataSz
                                        
                                        
*
* configuration module body
*
; Following two lines commented out because of mod above.
;                fcb     TopRam1,TopRam2,TopRam2 
;                fcb     SizPoll         ; irq polling table size
                fcb     SizDevic        ; device table size
                fdb     RunMod          ; first executable module
                fdb     DefDir          ; default directory
                fdb     StdIO           ; standard path
                fdb     BootMod         ; bootstrap module

                ifne    EXTERR          
                fdb     ErrNam          ; error messages path name
                endc                    

                                        
ConNam          fcs     'Init'          
RunMod          fcs     'SysGo'         

		ifne	USEDD
DefDir          fcs     '/DD'           
		else	
DefDir          fcs     '/D0'
		endc
		
StdIO           fcs     '/term'         
BootMod         fcs     'Boot'          

                ifne    EXTERR          
ErrNam          fcs     'errmsg'        
                endc                    

                emod                    ; end of configuration module
                                        
                                        
ConEnd          equ     *               
