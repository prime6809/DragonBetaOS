                opt     -l              
                ttl     Sequential      ; File Manager (SCF) Definitions
                page                    
**********
* Static storage requirements
*   SCF Devices must reserve this space for SCF
                                        
                org     V.USER          
V.TYPE          rmb     1               ; Device type or parity
V.LINE          rmb     1               ; lines left until end of page
V.PAUS          rmb     1               ; immediate Pause request
V.DEV2          rmb     2               ; attached device's static
V.INTR          rmb     1               ; Interrupt char
V.QUIT          rmb     1               ; Quit char
V.PCHR          rmb     1               ; Pause char
V.ERR           rmb     1               ; Accumulated errors
V.XON           rmb     1               ; X-On char
V.XOFF          rmb     1               ; X-Off char
V.RSV           rmb     12              ; Reserve bytes for future expansion
V.SCF           equ     .               ; total SCF manager static overhead
                                        
***************
* Character definitions
C$NULL          set     0               ; null char
C$RPET          set     $01             ; (ctl A - SOH) REPEAT LAST INPUT LINE
C$INTR          set     $03             ; (ctl C - ETX) KEYBOARD INTERRUPT
C$RPRT          set     $04             ; (ctl D - EOT) REPRINT CURRENT INPUT LINE
C$QUIT          set     $05             ; (ctl E - ENQ) Keyboard Abort
C$BELL          set     $07             ; (ctl G - BEL) Line overflow warning
C$BSP           set     $08             ; (ctl H - BS ) BACK SPACE
C$LF            set     $0A             ; LINE FEED
C$CR            set     $0D             ; CARRIAGE RETURN
C$FORM          set     $0C             ; (ctl L - FF ) Form Feed
C$XON           set     $11             ; (ctl Q - DC1) Transmit Enable
C$XOFF          set     $13             ; (ctl S - DC3) Transmit Disable
C$PAUS          set     $17             ; (ctl W - ETB) PAUSE charACTER
C$DEL           set     $18             ; (ctl X - CAN) DELETE LINE
C$EOF           set     $1B             ; (ctl [ - ESC) END of FILE
C$SPAC          set     $20             ; SPACE
C$PERD          set     '.              
C$COMA          set     ',              
                                        
***************
* FILE DESCRIPTOR OFFSETS
*
                org     PD.FST          
PD.DV2          rmb     2               ; OUTPUT DEV TBL PTR
PD.RAW          rmb     1               ; READ/WRITE OR RDLIN/WRLIN MODE
PD.MAX          rmb     2               ; READLINE HIGH BYTE COUNT
PD.MIN          rmb     1               ; DEVICES ARE 'MINE' IF CLEAR
PD.STS          rmb     2               ; Status routine module addr
PD.STM          rmb     2               ; reserved for Status routine
                org     PD.OPT          
                rmb     1               ; DEVICE TYPE
PD.UPC          rmb     1               ; CASE (0=BOTH, 1=UPPER ONLY)
PD.BSO          rmb     1               ; BACKSP (0=BSE, 1=BSE,SP,BSE)
PD.DLO          rmb     1               ; DELETE (0=BSE OVER LINE, 1=CRLF)
PD.EKO          rmb     1               ; ECHO (0=NO ECHO)
PD.ALF          rmb     1               ; AUTOLF (0=NO AUTO LF)
PD.NUL          rmb     1               ; END of LINE NULL COUNT
PD.PAU          rmb     1               ; PAUSE (0=NO END of PAGE PAUSE)
PD.PAG          rmb     1               ; LINES PER PAGE
PD.BSP          rmb     1               ; BACKSPACE charACTER
PD.DEL          rmb     1               ; DELETE LINE charACTER
PD.EOR          rmb     1               ; END of RECORD char (READ ONLY)
PD.EOF          rmb     1               ; END of FILE char
PD.RPR          rmb     1               ; REPRINT LINE char
PD.DUP          rmb     1               ; DUP LAST LINE char
PD.PSC          rmb     1               ; PAUSE char
PD.INT          rmb     1               ; KBD INTR char (ctl c)
PD.QUT          rmb     1               ; KBD QUIT char (ctl q)
PD.BSE          rmb     1               ; BACKSPACE ECHO charACTER
PD.OVF          rmb     1               ; LINE OVERFLOW char (BELL)
PD.PAR          rmb     1               ; PARITY CODE
PD.BAU          rmb     1               ; ACIA BAUD RATE (Color Computer)
PD.D2P          rmb     2               ; OFFSET of DEV2 name
PD.XON          rmb     1               ; ACIA X-ON char
PD.XOFF         rmb     1               ; ACIA X-OFF char
PD.Width        rmb     1               ; Chars per line
PD.Edit         rmb     1               ; 0=Extended editing features
OPTCNT          set     .-PD.OPT        ; Total user settable options
PD.ERR          rmb     1               ; most recent I/O error status
PD.TBL          rmb     2               ; Device Table addr (copy)
                                        
                org     PD.ERR          
PD.LdIn         rmb     1               ; Lead-in character for input
PD.LdOut        rmb     1               ; Lead-in character for output
PD.MLft         rmb     1               ; Move left code
PD.MRt          rmb     1               ; Move right code
PD.Left         rmb     1               ; Move left key
PD.Right        rmb     1               ; Move right key
PD.DelCh        rmb     1               ; Delete chr under cursor key
PD.DEol         rmb     1               ; Delete to end of line key
                opt     l               
