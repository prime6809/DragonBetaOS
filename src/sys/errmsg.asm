                use     defsfile

tylg            set     Data+Objct
atrv            set     ReEnt+rev
rev             set     1
edition		set	1
                mod     eom,name,tylg,atrv,0,size

size            equ     0

		fdb	$16

name            FCS     "errmsg"        ; OS9 Module name
		fcb	edition
		
                FCC     "1 UNCONDITIONAL ABORT"
		FCB	C$CR
		
                FCC     "2 KEYBOARD ABORT"
		FCB	C$CR

                FCC     "3 KEYBOARD INTERRUPT"
		FCB	C$CR

                FCC     "10 Unrecognized Symbol"
		FCB	C$CR

                FCC     "11 Excessive Verbage"
		FCB	C$CR

                FCC     "12 Illegal Statement Construction"
		FCB	C$CR

                FCC     "13 I-code Overflow"
		FCB	C$CR

                FCC     "14 Illegal Channel Reference"
		FCB	C$CR

                FCC     "15 Illegal Mode (Read/Write/Update)"
		FCB	C$CR

                FCC     "16 Illegal Number"
		FCB	C$CR

                FCC     "17 Illegal Prefix"
		FCB	C$CR

                FCC     "18 Illegal Operand"
		FCB	C$CR

                FCC     "19 Illegal Operator"
		FCB	C$CR

                FCC     "20 Illegal Record Field Name"
		FCB	C$CR

                FCC     "21 Illegal Dimension"
		FCB	C$CR

                FCC     "22 Illegal Literal"
		FCB	C$CR

                FCC     "23 Illegal Relational"
		FCB	C$CR

                FCC     "24 Illegal Type Suffix"
		FCB	C$CR

                FCC     "25 Too-Large Dimension"
		FCB	C$CR

                FCC     "26 Too-Large Line Number"
		FCB	C$CR

                FCC     "27 Missing Assignment Statement"
		FCB	C$CR

                FCC     "28 Missing Path Number"
		FCB	C$CR

                FCC     "29 Missing Comma"
		FCB	C$CR

                FCC     "30 Missing Dimension"
		FCB	C$CR

                FCC     "31 Missing DO Statement"
		FCB	C$CR

                FCC     "32 Memory Full"
		FCB	C$CR

                FCC     "33 Missing GOTO"
		FCB	C$CR

                FCC     "34 Missing Left Parenthesis"
		FCB	C$CR

                FCC     "35 Missing Line Reference"
		FCB	C$CR

                FCC     "36 Missing Operand"
		FCB	C$CR

                FCC     "37 Missing Right Parenthesis"
		FCB	C$CR

                FCC     "38 Missing THEN statement"
		FCB	C$CR

                FCC     "39 Missing TO"
		FCB	C$CR

                FCC     "40 Missing Variable Reference"
		FCB	C$CR

                FCC     "41 No Ending Quote"
		FCB	C$CR

                FCC     "42 Too Many Subscripts"
		FCB	C$CR

                FCC     "43 Unknown Procedure"
		FCB	C$CR

                FCC     "44 Multiply-Defined Procedure"
		FCB	C$CR

                FCC     "45 Divide by Zero"
		FCB	C$CR

                FCC     "46 Operand Type Mismatch"
		FCB	C$CR

                FCC     "47 String Stack Overflow"
		FCB	C$CR

                FCC     "48 Unimplemented Routine"
		FCB	C$CR

                FCC     "49 Undefined Variable"
		FCB	C$CR

                FCC     "50 Floating Overflow"
		FCB	C$CR

                FCC     "51 Line with Compiler Error"
		FCB	C$CR

                FCC     "52 Value out of Range for Destination"
		FCB	C$CR

                FCC     "53 Subroutine Stack Overflow"
		FCB	C$CR

                FCC     "54 Subroutine Stack Underflow"
		FCB	C$CR

                FCC     "55 Subscript out of Range"
		FCB	C$CR

                FCC     "56 Parameter Error"
		FCB	C$CR

                FCC     "57 System Stack Overflow"
		FCB	C$CR

                FCC     "58 I/O Type Mismatch"
		FCB	C$CR

                FCC     "59 I/O Numeric Input Format Bad"
		FCB	C$CR

                FCC     "60 I/O Conversion: Number out of Range"
		FCB	C$CR

                FCC     "61 Illegal Input Format"
		FCB	C$CR

                FCC     "62 I/O Format Repeat Error"
		FCB	C$CR

                FCC     "63 I/O Format Syntax Error"
		FCB	C$CR

                FCC     "64 Illegal Path Number"
		FCB	C$CR

                FCC     "65 Wrong Number of Subscripts"
		FCB	C$CR

                FCC     "66 Non-Record-Type Operand"
		FCB	C$CR

                FCC     "67 Illegal Argument"
		FCB	C$CR

                FCC     "68 Illegal Control Structure"
		FCB	C$CR

                FCC     "69 Unmatched Control Structure"
		FCB	C$CR

                FCC     "70 Illegal FOR Variable"
		FCB	C$CR

                FCC     "71 Illegal Expression Type"
		FCB	C$CR

                FCC     "72 Illegal Declarative Statement"
		FCB	C$CR

                FCC     "73 Array Size Overflow"
		FCB	C$CR

                FCC     "74 Undefined Line Number"
		FCB	C$CR

                FCC     "75 Multiply-Defined Line Number"
		FCB	C$CR

                FCC     "76 Multiply-Defined Variable"
		FCB	C$CR

                FCC     "77 Illegal Input Variable"
		FCB	C$CR

                FCC     "78 Seek Out of Range"
		FCB	C$CR

                FCC     "79 Missing Data Statement"
		FCB	C$CR

                FCC     "200 PATH TABLE FULL"
		FCB	C$CR

                FCC     "201 ILLEGAL PATH NUMBER"
		FCB	C$CR

                FCC     "202 INTERRUPT POLLING TABLE FULL"
		FCB	C$CR

                FCC     "203 ILLEGAL DEVICE"
		FCB	C$CR

                FCC     "204 DEVICE TABLE FULL"
		FCB	C$CR

                FCC     "205 ILLEGAL MODULE HEADER"
		FCB	C$CR

                FCC     "206 MODULE DIRECTORY FULL"
		FCB	C$CR

                FCC     "207 PROCESS TABLE FULL"
		FCB	C$CR

                FCC     "208 ILLEGAL SERVICE REQUEST"
		FCB	C$CR

                FCC     "209 MODULE BUSY"
		FCB	C$CR

                FCC     "210 BOUNDARY ERROR"
		FCB	C$CR

                FCC     "211 END OF FILE"
		FCB	C$CR

                FCC     "212 NOT YOUR MEMORY"
		FCB	C$CR

                FCC     "213 NON-EXISTING SEGMENT"
		FCB	C$CR

                FCC     "214 NO PERMISSION"
		FCB	C$CR

                FCC     "215 BAD PATH NAME"
		FCB	C$CR

                FCC     "216 PATH NAME NOT FOUND"
		FCB	C$CR

                FCC     "217 SEGMENT LIST FULL"
		FCB	C$CR

                FCC     "218 FILE ALREADY EXISTS"
		FCB	C$CR

                FCC     "219 ILLEGAL BLOCK ADDRESS"
		FCB	C$CR

                FCC     "221 MODULE NOT FOUND"
		FCB	C$CR

                FCC     "223 SUICIDE ATTEMPT"
		FCB	C$CR

                FCC     "224 ILLEGAL PROCESS NUMBER"
		FCB	C$CR

                FCC     "226 NO CHILDREN"
		FCB	C$CR

                FCC     "227 ILLEGAL SWI CODE"
		FCB	C$CR

                FCC     "228 KEYBOARD ABORT"
		FCB	C$CR

                FCC     "229 PROCESS TABLE FULL"
		FCB	C$CR

                FCC     "230 ILLEGAL PARAMETER AREA"
		FCB	C$CR

                FCC     "231 BACKTRACK ERROR"
		FCB	C$CR

                FCC     "232 INCORRECT MODULE CRC"
		FCB	C$CR

                FCC     "233 SIGNAL ERROR"
		FCB	C$CR

                FCC     "234 MODULE NOT IN MEMORY"
		FCB	C$CR

                FCC     "235 BAD NAME"
		FCB	C$CR

                FCC     "236 BAD MODULE HEADER"
		FCB	C$CR

                FCC     "237 MEMORY FULL"
		FCB	C$CR

                FCC     "238 BAD PROCESS ID"
		FCB	C$CR

                FCC     "239 NO TASK NUMBER AVAILABLE"
		FCB	C$CR

                FCC     "240 DEVICE NUMBER UNKOWN"
		FCB	C$CR

                FCC     "241 SECTOR OUT OF RANGE"
		FCB	C$CR

                FCC     "242 DISK WRITE PROTECTED"
		FCB	C$CR

                FCC     "243 DISK CRC ERROR"
		FCB	C$CR

                FCC     "244 READ ERROR"
		FCB	C$CR

                FCC     "245 WRITE ERROR"
		FCB	C$CR

                FCC     "246 DEVICE NOT READY"
		FCB	C$CR

                FCC     "247 SEEK ERROR"
		FCB	C$CR

                FCC     "248 MEDIA FULL"
		FCB	C$CR

                FCC     "249 WRONG MEDIA TYPE"
		FCB	C$CR

                FCC     "250 DEVICE BUSY"
		FCB	C$CR

                FCC     "251 DISK ID CHANGED"
		FCB	C$CR

                FCC     "252 RECORD IS IN USE"
		FCB	C$CR

                FCC     "253 FILE IS IN USE"
		FCB	C$CR

                FCC     "254 DEADLOCK"
		FCB	C$CR


                emod
eom             equ     *
                end

