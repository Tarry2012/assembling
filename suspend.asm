DATA 	SEGMENT
;-------------主界面------------------------------------------------------------------------------------------------
    SHOW00 	DB    0AH, 0DH, "################################################################$";|
    SHOW01      DB    0AH, 0DH, "# 			  THE  	       SHOW                    #$";|
    SHOW02      DB    0AH, 0DH, "################################################################$";|
    SHOW03      DB    0AH, 0DH, "#                         #                            	       #$";|
    SHOW04      DB    0AH, 0DH, "#  		          #			               #$";|
    SHOW05      DB    0AH, 0DH, "#		          #                 	               #$";|
    SHOW06      DB    0AH, 0DH, "#            	          #                                    #$";|
    SHOW07      DB    0AH, 0DH, "#          	          #                                    #$";|
    SHOW08      DB    0AH, 0DH, "################################################################$";|
    SHOW09      DB    0AH, 0DH, "#                    	 #  				       #$";|
    SHOW10      DB    0AH, 0DH, "#                        #  				       #$";|
    SHOW11      DB    0AH, 0DH, "#                        #  				       #$";|
    SHOW12      DB    0AH, 0DH, "#                        #                                     #$";|
    SHOW13  	DB    0AH, 0DH, "#                        # 	                               #$";|
    SHOW14 	DB    0AH, 0DH, "# 		         #  			               #$";|
    SHOW15      DB    0AH, 0DH, "#                        #                                     #$";|
    SHOW16      DB    0AH, 0DH, "#                        #                                     #$";|
    SHOW17      DB    0AH, 0DH, "#                        #                                     #$";|
    SHOW18      DB    0AH, 0DH, "################################################################$";|
    SHOW19      DB    0AH, 0DH, "#    INPUT ESC TO EXIT                  TQY & PJ & GY          #$";|
    SHOW20      DB    0AH, 0DH, "################################################################$";|

;---音乐乐谱数据---------------------------------------------------------------------------------------------
INFORMUSIC      DB     "PLAY MUSIC...$"

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

;==================打印星星的数据=========
BUFSTAR1 	DB "* $"
BUFSTAR2 	DB " $"

;=================显示时间的数据==========
BUFTIME1 	DB "Current time is : $"
HOUR    	DB 20H,20H,':','$'
MINUTE  	DB 20H,20H,':','$'
SECOND  	DB 20H,20H,':','$'
MSECOND 	DB 20H,20H,'$'
INFOR   	DB "Press any key to exit.....$"

;================快排数据===========
inf0 		db 'Please input  data(0~255):', 0ah, 0dh, '$'
buf 		db 250,250 dup (0) 				;保存输入数据字符串的缓冲区
table 		db 100 dup (0) 					;分析输入的字符串,分解为数字,保存在这个表中
buf2 		db 250 dup (0) 					;将数组转换成可以显示的字符串,每个数字用逗号分隔,保存在这里
								;方便显示
len 		db 0 						;保存数组长度
inf1 		db 'Befor quick sort:$'
inf2 		db 'After quick sort:$'
inf4 		db 13,10,'Find another num(Y/N)?$'
inf_end 	db 'Press key to play music...$'

;===============获得中断时间的数据=========
firstsec	db 20h, 20h, '$'
secondsec   	db 20h, 20h, '$'
testsec 	db 20h, 20h, '$'
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
	PUSH 	AX
	PUSH 	BX
	PUSH 	DX
	MOV 	AH, 02H
	MOV 	BH, 00H
	MOV 	DH, ROW
	MOV 	DL, CLM
	INT 	10H
	POP 	DX
	POP 	BX
	POP 	AX
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
;===============将寄存器中存放的二进制数转换为ASCII码并存放在内存单元的宏
TIMER1 MACRO REG,ADR
       PUSH AX
       PUSH BX
       LEA SI,ADR
       MOV AL,REG
       MOV AH,00
       MOV BL,10
       DIV BL
       ADD AL,30H
       MOV [SI],AL
       ADD AH,30H
       INC SI
       MOV [SI],AH
       POP BX
       POP AX
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
	
;	CALL  	SHOWTIME
;	CALL 	PRINTSTAR
;	CALL 	RANDOMSORT
        mov  ah, 4ch
	int   21h

;==========随即数快排子程序=========
RANDOMSORT  PROC NEAR
	call mainsort 	;调用主函数

	cursor 18, 29
 	lea dx,inf_end 	;提示按任意键结束
 	mov ax,0900H
 	int 21H
 	mov ax,0700H
 
	int 21H
	mov ax,4c00H
	int 21H

mainsort proc near

	CURSOR  10, 27
	lea dx,inf0 		;提示输入字符串,并接受输入,存在buf
	mov ah,09H
	int 21H
	cursor 11,27 
	lea dx,buf
	mov ah,0aH
	int 21H

	call SaveToArray 	;调用函数,处理输入的字符串,整理为数字数组
	CURSOR  13, 27
	lea dx,[inf1] 		;提示语句
	mov ah,09H
	int 21H
	lea si,[table] 		;传递两个参数,调用函数在屏幕上显示数组
	lea di,[buf2]
	call DisplayArray
	lea di,[table] 		;下面5句使di指向数组的最后一个数字
	mov al,[len]
	mov ah,0
	add di,ax
	dec di
	call QuickSort 		;调用快速排序函数,直接整理数组里面的元素
	CURSOR  14, 27
	lea dx,[inf2] 		;提示语句
	mov ah,09H
	int 21H
	lea si,[table] 		;调用函数,显示数组
	lea di,[buf2]
	call DisplayArray
	ret
mainsort endp

;函数功能:对于si和di范围内的数字进行整理,使左边的所有数都不大于右边的任意一个,返回中间位置的指针
;输入参数:si 指向待整理范围的第一个数字,而且也以这个数字作为分界,比这个数字大的放右边,比他小的放左边
; di 指向待整理范围的最后一个数字
;输出参数:dx 指向整理后的中间数字,在他左边的数字都不大于他右边的任意一个数字
Partition proc near

	push si
	push di
	push ax
	mov al,[si] 	;保存第一个数字,作为比较大小的标准
	dec si 		;si后移,因为下面循环的第一步是si前移,所以要先后移一位,不然会以后第一个数
	inc di 		;di前移,原因如上
P_l1:
P_l2:
	dec di 		;di前移,如果[di]比标准数字大,则继续循环,这个循环结束后,
			;di指向从右边开始第一个比标准数字小的数字(如果存在的话)
	cmp [di],al
	ja P_l2
P_l3:
	inc si 		;si后移,如果[si]比标准数字小,则继续循环,这个循环结束后,
			;di指向从右边开始第一个比标准数字大的数字(如果存在的话)
	cmp [si],al
	jb P_l3
	cmp si,di 	;如果此时si>=di,那么跳出循环,整理结束
	jnb P_s1
	mov ah,[di] 	;这三句是交换[si]和[di]
	xchg [si],ah
	mov [di],ah
	jmp P_l1 	;如果si还是小于di,那么继续开始循环,知道整理结束
P_s1:
	mov dx,di 	;现在di指向中间元素,赋值个dx,作为返回值
	pop ax
	pop di
	pop si
	ret

Partition endp

;函数功能:对si和di范围内的数组进行排序
;输入参数:si 指向排序范围的第一个数字
; di 指向排序范围的最后一个数字
;输出参数:无
QuickSort proc near

	push si 	;寄存器入栈,保护现场
	push di
	push dx
	cmp si,di 	;如果si>=di,则直接返回
	jge return
	call Partition 	;调用函数,整理数组,返回值dx,在dx左边的数字都不大于右边的数字
	push di 	;保存di
	mov di,dx 	;对于si和bx范围内的数组,递归调用本函数
	call QuickSort
	pop di 		;取出di
	mov si,dx 	;对dx+1和di范围内的数组,递归调用本函数
	inc si
	call QuickSort
return:
	pop dx 		;寄存器出栈,恢复现场
	pop di
	pop si
	ret

QuickSort endp

;函数功能:把si指向的数组,转变成一个显示的字符串,并输出
;输入参数:si 指向需要处理的数组
; di 保存转换后字符串的位置
;输出参数:无(直接在屏幕上显示字符串)
DisplayArray proc near

	push ax 	;寄存器入栈
	push bx
	push cx
	push dx
	push di
	push si
	
	mov cl,[len] 	;设置循环次数等于数组元素个数
	mov ch,0
	push di 	;di指向的是存放转换后字符串的地方,这个地方最后赋值给dx,可以显示
DA_l1: 			;因为每个数字最多为三位,所以下面直接进行处理,不写通用的转换程序
	mov al,[si] 	;当前处理的数字放入ax
	mov ah,0
	mov bl,100 	;16位除8位的除法
	div bl
	cmp al,0 	;如果商为0,跳转到处理余数的部分
	jz DA_s1
	add al,'0' 	;商不为0,则加上30H,放入di指向的位置,di后移
	mov [di],al 
	inc di
	mov al,ah 	;把余数除10,然后ax加上3030H,在用16位长度放入[di]中
	mov ah,0
	mov bl,10
	div bl
	add ax,3030H
	mov word ptr [di],ax
	add di,2
	mov byte ptr [di],',' ;后面加逗号
	inc di 		;di后移
	inc si 		;已经一个数字处理结束,si后移
	jmp DA_s3 	;跳到loop的地方
DA_s1:
	mov al,ah 	;数字不是三位数,把余数放入al,再除10
	mov ah,0
	mov bl,10
	div bl
	cmp al,0 	;比较商是否为0,如果为0,说明是一位数,跳转到下面
	jz DA_s2
	add al,'0' 	;如果商不为0,则是两位数,商加30H,放入[di],di后移
	mov [di],al
	inc di
DA_s2:
	add ah,'0' 	;把余数加上30H,放入[di],di后移
	mov [di],ah
	inc di
	mov byte ptr [di],',' ;放入逗号
	inc di
	inc si 		;一个数字处理完成,si后移
DA_s3:
	loop DA_l1  	;继续处理下一个数字
	dec di  	;现在字符串最后一位是逗号,把这个逗号换成'$'就好了
	mov byte ptr [di],'$'
	pop dx 		;把指向字符串第一个字符的位置出栈,赋值给bx
	mov ah,09H 	;调用中断输出
	int 21H
	pop si 		;寄存器出栈
	pop di
	pop dx
	pop cx
	pop bx
	pop ax
	ret
	
DisplayArray endp

;函数功能:把输入的字符串整理为数组
;输入参数:无
;输出参数:cx 整理出来的数组长度,数组存放在table里面
SaveToArray proc near

	mov cl,[buf+1] 		;输入字符串的长度
	mov ch,0
	lea si,[buf+2] 		;指向输入字符串的起始位置
	lea di,[table] 		;指向存放整理后数字的位置
	mov bh,0 		;bh存放的是数组长度
SaveToArray_l1:
	cmp byte ptr [si],'0' 	;如果这个字符不在0~9范围内,说明上一个数字输入结束
	jb EndOfANum
	cmp byte ptr [si],'9'
	ja EndOfANum
	mov al,[di] 		;如果这个字符是数字字符,那么把原来的值乘10,加上这个数字,减去30H
	mov bl,10
	mul bl
	mov bl,[si]
	sub bl,'0'
	add al,bl
	mov [di],al 		;保存回去
	jmp short continue 	;继续下一次循环
EndOfANum: 			;上一个数字输入完毕
	cmp byte ptr [di],0 	;如果现在[di]的值为0,那不进行处理,因为已经规定,数字不为0
	jz continue
	inc di 			;如果[di]不为0,则di后移一位
	inc bh 			;数组长度加1
continue:
	inc si 			;si后移一位
	loop SaveToArray_l1 	;循环
	inc bh 			;最后找到的一个数字,还没有计算进数组长度,所以这里加了1
	mov [len],bh 		;数组长度保存到[len]
	mov cl,bh 		;数组长度保存到cx
	mov ch,0
	ret
	
SaveToArray endp
	RET
RANDOMSORT ENDP

;===========显示主屏幕的子程序DISPLAY=====================
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
;	SHOWSTR SHOW16
;	SHOWSTR SHOW17
	SHOWSTR SHOW18
	SHOWSTR SHOW19
	SHOWSTR SHOW20
	RET
DISPLAY	ENDP
	
;============循环播放音乐的子程序PLAYMUSIC
PLAYMUSIC PROC NEAR
	PUSH  	CX
	CURSOR 5,5
	SHOWSTR INFORMUSIC
	
	ADDRESS MUS_FREG1, MUS_TIME1
	CALL 	MUSIC
;	ADDRESS MUS_FREG2, MUS_TIME2
;	CALL 	MUSIC
;	ADDRESS MUS_FREG3, MUS_TIME3
;	CALL 	MUSIC
;	ADDRESS MUS_FREG4, MUS_TIME4
;	CALL 	MUSIC
	POP 	CX
	RET
PLAYMUSIC ENDP

;=================通用发生子程序GENSOUND==============
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

;============获得任意的延迟时间===========
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

;==========播放音乐子程序=============
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
 	CALL    KBTEST
	JMP 	FREG
END_MUS:
	RET
MUSIC  	ENDP

;===========打印星星的子程序=========
PRINTSTAR PROC NEAR
LOOP1: 
	MOV CX,7
	MOV AX,10
        MOV DX,2
LOOP2: 	
	CURSOR  AX,DX
        SHOWSTR   BUFSTAR1
        CALL DELAY
        CURSOR  AX,DX
        SHOWSTR   BUFSTAR2
        INC AX
        ADD DX,3
        DEC CX
 ;       MOV AH,0BH

        JNZ LOOP2
        JMP LOOP1


DELAY   PROC NEAR

        PUSH CX
        PUSH AX

	MOV  CX,0FFFFH   ; about 100ms

DELAY_LOOP:
        IN  AL, 61H
        AND  AL, 10H
        CMP  AL, AH
        JE   DELAY_LOOP
        MOV  AH,AL
        LOOP DELAY_LOOP

        POP AX
        POP CX
        RET
DELAY  ENDP
	RET
PRINTSTAR ENDP

SHOWTIME PROC NEAR

       CURSOR 5,35
       SHOWSTR BUFTIME1

LOOPR:
	MOV AH,2CH
        INT 21H

        PUSH CX
        MOV CH,DL
        TIMER1 CH,MSECOND
        MOV CH,DH
        TIMER1 CH,SECOND
        POP CX
        TIMER1 CL,MINUTE
        TIMER1 CH,HOUR
        CURSOR 7,35
        SHOWSTR HOUR
        SHOWSTR MINUTE
        SHOWSTR SECOND
        SHOWSTR MSECOND

        CALL KBTEST
        JMP LOOPR
	
	RET
SHOWTIME ENDP

;============检测键盘是否有输入且输入是否为esc
kbtest proc near
;	push ax
 	mov ah,1
 	int 16h
 	jnz kbtest_1
 	mov ax,0ffh
 	stc
 	ret ;没有内容自动返回
kbtest_1:
 	mov ah,7
 	int 21h
 	mov ah,0
 	cmp al,0
 	jnz kbtest_2  ;输入的是ASCII码(AL)
 	mov ah,7
 	int 21h       ;取扩展ASCII码(非字符码的功能键)
 	mov ah,1
 	clc
kbtest_2:
	cmp al, 1BH
	jz  exit
;	pop ax
	ret
kbtest endp
;==========退出程序===============
EXIT: 	
	SETCRT
        CLEAR
        MOV AH,4CH
        INT 21H

CODE ENDS
	END START


