                nam     P1               
                ttl     Device          ; Descriptor for 'P1' (serial printer)
                                                                                
************************************************************
*
*     P1 Module, SCF/ACIA device
*
       		                                 
                use     defsfile        
                                        
                mod     PSEND,PSNAM,DEVIC+OBJCT,1,PSMGR,PSDRV
                fcb     Updat.          ; mode
                fcb     IOBlock/DAT.BlCt              ; port bank
                fdb     A.P1            ; port address
                fcb     PSNAM-*-1      ; option byte count
                fcb     DT.SCF          ; Device Type: SCF
                                        
* Default path options
                                        
                fcb     0               ; case=UPPER and lower
                fcb     0               ; backspace=BS char only
                fcb     1               ; delete=CRLF
                fcb     0               ; no auto echo
                fcb     1               ; auto line feed on
                fcb     0               ; no nulls after CR
                fcb     0               ; no page pause
                fcb     66              ; lines per page
                fcb     0               ; no backspace char
                fcb     0               ; no delete line char
                fcb     C$CR            ; end of record char
                fcb     C$EOF           ; end of file char
                fcb     0               ; no reprint line char
                fcb     0               ; no dup last line char
                fcb     0               ; no pause char
                fcb     0               ; no abort character
                fcb     0               ; no interrupt character
                fcb     0               ; backspace echo char
                fcb     0               ; no line overflow char
                fcb    	0		; no parity
                fcb     $06             ; undefined baud rate
                fdb     0               ; no echo device
		fcb	C$XON		; transmit enable
		fcb	C$XOFF		; transmit disable
PSNAM           equ	*
		fcs     'p1'		; device name
PSMGR           equ	*
		fcs     "SCF"           ; file manager
PSDRV           equ	*
		fcs     "ACIA51"        ; driver
                emod                    
                                        
PSEND		equ     *               
                                        
