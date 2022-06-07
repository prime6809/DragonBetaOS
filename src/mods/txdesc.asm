
				use		defsfile
                
TPort           set     A.T1            
TName           set     '1              

DDSrc			set		0 
                       
***************
*  Tx device  module
                mod     TxEnd,TxNam,DEVIC+OBJCT,REENT+1,TxMGR,TxDrv 
                fcb     UPDAT.          ; mode
                fcb     IOBlock/DAT.BlCt ; port bank
                fdb     TPort           ; port address
                fcb     OptEnd-*+1      ; initilization table size
                fcb     DT.SCF          ; Device Type: SCF
                                        
* Default path options
                                        
                fcb     0               ; case=UPPER and lower
                fcb     1               ; backspace:0=bsp,1=bsp then sp & bsp
                fcb     0               ; delete:0=bsp over line,1=return
                fcb     1               ; echo:0=no echo
                fcb     1               ; auto line feed:0=off
                fcb     0               ; end of line null count
                fcb     1               ; pause:0=no end of page pause
                fcb     24              ; lines per page
                fcb     C$BSP           ; backspace character
                fcb     C$DEL           ; delete line character
                fcb     C$CR            ; end of record character
                fcb     C$EOF           ; end of file character
                fcb     C$RPRT          ; reprint line character
                fcb     C$RPET          ; duplicate last line character
                fcb     C$PAUS          ; pause character
                fcb     C$INTR          ; interrupt character
                fcb     C$QUIT          ; quit character
                fcb     C$BSP           ; backspace echo character
                fcb     C$BELL          ; line overflow char
				
                ifne DDSrc
				fcb     A.T.init        ; ACIA initialization
				else
				fcb		0				; this matches the file from the OS9 boot disk.
				endc
                
				fcb     6               ; baud rate = 9600
                fdb     TxNam           ; copy of descriptor name address
                fcb     C$XON           ; X-ON char
                fcb     C$XOFF          ; X-Off char
OptEnd          set     *               

                ifne    EXTEND          
                fcb     80              ; number of columns for display
                fcb     0               ; No-edit flag (4 = no-edit)
                fcb     $00             ; Lead-in character for input
                fcb     $1B             ; Lead-in character for output
                fcb     $08             ; Move left code
                fcb     $C3             ; Move right code
                fcb     $0C             ; Move left key (Ctrl-K)
                fcb     $0A             ; Move right key (Ctrl-L)
                fcb     $1A             ; Delete chr under cursor key (Ctrl-F)
                fcb     $1C             ; Delete to end of line key (Ctrl-G)
                endc                    


                ifeq    Pass            
TxNam           equ     *               
                endc                    

                fcb     't,TName+$80    ; device name

                ifeq    Pass            
TxMGR           equ     *               
                endc                    

                fcs     'SCF'           ; file manage

                ifeq    Pass            
TxDrv           equ     *               
                endc                    

                fcs     'ACIA51'        ; driver
                                        
                emod                    
                                        

                ifeq    Pass            
TxEND           equ     *               
                endc                    

                                        
Pass            set     Pass+1          
*TPort set A.T2
*TName set '2
* use txdesc.asm
*TPort set A.T3
*TName set '3
* use txdesc
                end                     
                                        
                emod                    ; Module CRC
                                        
TxEnd           equ     *               
