DATA 	SEGMENT
;-------------主界面------------------------------------------------------------------------------------------------
    SHOW00 	DB    0AH, 0DH, "#############################################################$";|
    SHOW01      DB    0AH, 0DH, "# 			  THE  	       SHOW                 #$";|
    SHOW02      DB    0AH, 0DH, "#############################################################$";|
    SHOW03      DB    0AH, 0DH, "#                      #                              	    #$";|
    SHOW04      DB    0AH, 0DH, "#  		       #			            #$";|
    SHOW05      DB    0AH, 0DH, "# 		       #                 	            #$";|
    SHOW06      DB    0AH, 0DH, "#            	       #                                    #$";|
    SHOW07      DB    0AH, 0DH, "#          	       #                                    #$";|
    SHOW08      DB    0AH, 0DH, "#############################################################$";|
    SHOW09      DB    0AH, 0DH, "#                     #  				    #$";|
    SHOW10      DB    0AH, 0DH, "#                     #  				    #$";|
    SHOW11      DB    0AH, 0DH, "#                     #  				    #$";|
    SHOW12      DB    0AH, 0DH, "#                     #                                     #$";|
    SHOW13  	DB    0AH, 0DH, "#                     # 	                            #$";|
    SHOW14 	DB    0AH, 0DH, "# 		      #  			            #$";|
    SHOW15      DB    0AH, 0DH, "#                     #                                     #$";|
    SHOW16      DB    0AH, 0DH, "#                     #                                     #$";|
    SHOW17      DB    0AH, 0DH, "#                     #                                     #$";|
    SHOW18      DB    0AH, 0DH, "#############################################################$";|
    SHOW19      DB    0AH, 0DH, "#  INPUT ESC TO EXIT                            T&P&G       #$";|
    SHOW20      DB    0AH, 0DH, "#############################################################$";|

;---音乐乐谱数据---------------------------------------------------------------------------------------------
INFORMUSIC      DB     0AH, 0DH, "PLAY MUSIC...$"

;==================《找朋友》的频率表和时间节拍表========================
MUS_FREG1 DW 3 dup (392,440),392				;为演奏的乐曲定义一个频率表
          DW 392,523,494,440,392,330	 
	  DW 2 DUP (392), 330, 349, 2 DUP (392), 330
          DW 262,349,330,294,262,294,262,-1		;-1作为乐曲的结束符
MUS_TIME1 DW 6 DUP(50),100,4 DUP (50),100,100	;为演奏乐曲定义一个节拍时间表
	  DW 2 DUP(6 DUP(50),100)

;====================《月儿弯弯》的频率表和时间节拍表=====================
MUS_FREG2 DW 392,3 dup (262),330,2 dup (262)
          DW 2 dup (392,330),392
          DW 392,3 dup (262),330,2 dup (262)
          DW 2 dup (392,330),294
          dw 392,392,440,440
          dw 392,330,294,330,440
          dw 392,392,3 dup (262),294,330,440
          DW 392,33,294,262,-1
MUS_TIME2 DW 2 DUP(6 dup(50),100,75,25,50,50,200)
          DW 75,25,100,200
          DW 4 dup(50),200
          dw 75,25,6 dup(50)
          dw 75,25,100,200
;===================《小蜜蜂》的频率表和时间节拍表======================== 
MUS_FREG3 DW 392,330,330,349,294,294
          DW 262,294,330,349,3 dup(392)
          DW 392,330,330,349,294,294
          DW 262,330,392,392,330
          DW 5 dup(294),330,349
          dw 5 dup(330),349,392
          dw 392,330,330,349,294,294
          dw 262,330,392,392,262,-1
MUS_TIME3 DW 2 dup(50,50,100)
          DW 6 dup(50),100
          DW 2 dup(50,50,100)
          DW 4 dup(50),200
          DW 2 dup(6 dup(50),100)
          dw 2 dup(50,50,100)
          DW 4 DUP(50),200
;===================其他乐曲的频率表和时间节拍表
MUS_FREG4  dw 330,294,262,294,3 dup (330)     ;频率表
               dw 3 dup (294),330,392,392
               dw 330,294,262,294,4 dup (330)
               dw 294,294,330,294,262,-1
MUS_TIME4  dw 6 dup (25),50                   ;节拍表
               dw 2 dup (25,25,50)
               dw 12 dup (25),100
DATA ENDS   

STACK 	SEGMENT  stack
		DB 200 DUP (?)
STACK   ENDS

;==================显示字符串的宏SHOWSTR=========================================
SHOWSTR  MACRO   STR
	 PUSH    DX
	 PUSH    AX
	 MOV     DX, OFFSET STR
	 MOV 	 AH, 09H
	 INT 	 21H
	 POP 	 AX
	 POP     DX
	 ENDM

;================音乐地址宏ADDRESS==============================================
ADDRESS MACRO  MUS_ADDA, MUS_ADDB
	LEA     SI, MUS_ADDA
	LEA     BP, DS:MUS_ADDB
	ENDM

;================设置光标位置宏指令===========================================
CURSOR MACRO ROW, CLM
	MOV 	AH, 02H
	MOV 	BH, 00H
	MOV 	DH, ROW
	MOV 	DL, CLM
	INT 	10H
	ENDM

;===============屏幕显示方式设置宏指令SETCRT================================
SETCRT 	MACRO
	MOV 	AH, 0
	MOV 	AL, 2
	INT 	10H
	ENDM

;===============清屏宏指令============================================
CLEAR MACRO 	
	MOV 	AH, 06H
	MOV 	AL, 0
	INT 	10H
	ENDM

;===============代码段定义===============================================
CODE 	SEGMENT 
	 ASSUME DS:DATA, SS:STACK, CS:CODE
START:
	MOV 	AX, DATA
	MOV 	DS, AX
	MOV 	AX, STACK
	MOV 	SS, AX
	MOV 	SP, 200

	SETCRT  
	CLEAR
	CALL    DISPLAY
	CALL    PLAYMUSIC
	CURSOR 22,2
	CALL 	EXIT

DISPLAY PROC NEAR
	SHOWSTR SHOW00
	SHOWSTR SHOW01
	SHOWSTR SHOW02
	SHOWSTR SHOW03
	SHOWSTR SHOW04
	SHOWSTR SHOW05
	SHOWSTR SHOW06
	SHOWSTR SHOW07
	SHOWSTR SHOW08
	SHOWSTR SHOW09
	SHOWSTR SHOW10
	SHOWSTR SHOW11
	SHOWSTR SHOW12
	SHOWSTR SHOW13
	SHOWSTR SHOW14
	SHOWSTR SHOW15
	SHOWSTR SHOW16
	SHOWSTR SHOW17
	SHOWSTR SHOW18
	SHOWSTR SHOW19
	SHOWSTR SHOW20
	RET
DISPLAY	ENDP
	
PLAYMUSIC PROC NEAR
	PUSH  	CX
	MOV 	CX, 1
	CURSOR 5,2
	SHOWSTR INFORMUSIC
	
LOOPLAY:
	ADDRESS MUS_FREG1, MUS_TIME1
	CALL 	MUSIC
	ADDRESS MUS_FREG2, MUS_TIME2
	CALL 	MUSIC
	ADDRESS MUS_FREG3, MUS_TIME3
	CALL 	MUSIC
	ADDRESS MUS_FREG4, MUS_TIME4
	CALL 	MUSIC
	INC 	CX
	LOOP 	LOOPLAY
	RET
PLAYMUSIC ENDP

GENSOUND 	PROC 	NEAR
		PUSH 	AX
		PUSH 	BX
		PUSH 	CX
		PUSH 	DX                 
		PUSH 	DI

		MOV 	AL, 0B6H
		OUT 	43H, AL
		MOV 	DX, 12H
		MOV 	AX, 348CH
		DIV 	DI
		OUT 	42H, AL

		MOV 	AL, AH
		OUT 	42H, AL

		IN 	AL, 61H
		MOV 	AH, AL
		OR 	AL, 3
		OUT 	61H, AL
WAIT1:
 		MOV 	CX, 3314
		CALL 	WAITF
DELAY1:
		DEC 	BX
		JNZ 	WAIT1
		MOV 	AL, AH
		OUT 	61H, AL

		POP 	DI
		POP 	DX
		POP 	CX
		POP 	BX
		POP 	AX
		RET
GENSOUND 	ENDP

WAITF 	PROC  NEAR
		PUSH  	AX
WAITF1:
	IN 	AL, 61H
	AND 	AL, 10H
	CMP 	AL, AH
	JE 	WAITF1
	MOV 	AH, AL
	LOOP 	WAITF1
	POP 	AX
	RET 	
WAITF 	ENDP

MUSIC 	PROC  	NEAR
	XOR 	AX, AX
FREG:	
	MOV 	DI, [SI]
	CMP 	DI, -1
	JE 	END_MUS
	MOV 	BX, DS:[BP]
	CALL 	GENSOUND
	ADD 	SI, 2
	ADD 	BP, 2
	JMP 	FREG
END_MUS:
	RET
MUSIC  	ENDP

EXIT: 	MOV 	AH, 4CH
	INT 	21H

CODE ENDS
	END START


