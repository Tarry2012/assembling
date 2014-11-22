DATA         SEGMENT
;-------------主界面------------------------------------------------------------------------------------------------
    ATTR1      DB    02H                            ; the color of '#', black and the green on |
    ATTR2      DB    0EH                            ; the color of '*', red           green    |
    ATTR3      DB    04H                            ;                                          |
    ATTR4      DB    0AH                            ;                                          | 
    SHOW00     DB              "############################################################$";|
    SHOW01     DB    0AH, 0DH, "#* * * * * * * * * THE *  *  *  SHOW * * * * * * * * * * * #$";| 
    SHOW02     DB    0AH, 0DH, "############################################################$";|
    SHOW03     DB    0AH, 0DH, "#                      #                                   #$";|
    SHOW04     DB    0AH, 0DH, "#                      #                                   #$";|
    SHOW05     DB    0AH, 0DH, "#                      #                                   #$";|
    SHOW06     DB    0AH, 0DH, "#                      #                                   #$";|
    SHOW07     DB    0AH, 0DH, "#                      #                                   #$";|
    SHOW08     DB    0AH, 0DH, "############################################################$";|
    SHOW09     DB    0AH, 0DH, "#                      #                                   #$";|
    SHOW10     DB    0AH, 0DH, "#                      #                                   #$";|
    SHOW11     DB    0AH, 0DH, "#                      #                                   #$";|
    SHOW12     DB    0AH, 0DH, "#                      #                                   #$";|
    SHOW13     DB    0AH, 0DH, "#                      #                                   #$";|
    SHOW14     DB    0AH, 0DH, "#                      #                                   #$";|
    SHOW15     DB    0AH, 0DH, "#                      #                                   #$";|
    SHOW16     DB    0AH, 0DH, "############################################################$";|
    SHOW17     DB    0AH, 0DH, "# * PRESS ANY KEY TO EXIT * * * * * *TQY & PJ & GY * * * * #$";|
    SHOW18     DB    0AH, 0DH, "############################################################$";|

;---音乐乐谱数据---------------------------------------------------------------------------------------------
INFORMUSIC      DB     "PLAY MUSIC...$"

;==================《找朋友》的频率表和时间节拍表========================
MUS_FREG1 DW 3 DUP (392,440),392                                ;为演奏的乐曲定义一个频率表
          DW 392,523,494,440,392,330	 
	  DW 2 DUP (392), 330, 349, 2 DUP (392), 330
          DW 262,349,330,294,262,294,262,-1		;-1作为乐曲的结束符
MUS_TIME1 DW 6 DUP(50),100,4 DUP (50),100,100	;为演奏乐曲定义一个节拍时间表
	  DW 2 DUP(6 DUP(50),100)

;==================打印星星的数据=========
BUFSTAR1 	DB "* $"
BUFSTAR2 	DB " $"

;=================显示时间的数据==========
BUFTIME1 	DB "Current time is : $"
HOUR    	DB 20H,20H,':','$'
MINUTE  	DB 20H,20H,':','$'
SECOND  	DB 20H,20H,':','$'
MSECOND 	DB 20H,20H,'$'

;================快排数据===========
COUNT EQU 5
    INF0  DB 'Please input data(0~255)''$'
    BUF   DB 250,0,250 DUP (0)             ;保存输入数据字符串的缓冲区
    TABLE DB 100 DUP (0)                   ;分析输入的字符串,分解为数字,保存在这个表中
    BUF2  DB 250 DUP (0)                   ;将数组转换成可以显示的字符串,每个数字用逗号分隔,保存在这里,方便显示
    LEN   DB 0                             ;保存数组长度
    INF1  DB 'Befor quick sort:$'
    INF2  DB 'After quick sort:$'
    ARRAY DB 11 DUP (0)


;===============获得中断时间的数据=========
FIRSTSEC        DB 20H, 20H, '$'
SECONDSEC       DB 20H, 20H, '$'
TESTSEC         DB 20H, 20H, '$'

DATA ENDS                              

STACK 	SEGMENT  stack
		DB 200 DUP (?)
STACK   ENDS

;==================显示字符串的宏SHOWSTR=========================================
SHOWSTR  MACRO   STR
         PUSH    DX
         PUSH    AX
         MOV     DX, OFFSET STR
         MOV     AH, 09H
         INT     21H
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
	MOV 	AL, 3
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
        CALL    display
      ;   CALL DISPLAY1
LOOPSTART:
        MOV AH,2CH              ;读取此时时间
        INT 21H
        PUSH DX
   
LOOPMUSIC:
	CALL    PLAYMUSIC	 ;播放音乐
        MOV     AH, 2CH          ;再次读取时间
        INT     21H
        POP     BX
        PUSH    BX
        SUB     DH, BH           ;将两次秒数相减
        JB      BELOWZERO1       ;如果秒数小于零
        JMP     ABOVEZERO1       ;否则
                          
BELOWZERO1:               ;如果小于零，将第二秒数差 加60与14比较
         ADD    AH, 60            
         CMP    DH, 14
         JB     LOOPMUSIC     ;如果小与28，再次播放音乐
         JMP    EXENEXT1      ;否则跳到下一个程序
   
ABOVEZERO1:
        CMP     DH, 14
        JB      LOOPMUSIC
     
EXENEXT1:
        POP     BX
        AND     BX, 0H
        MOV     AH, 2CH
        INT     21H
        PUSH    DX
   
LOOPTIME:
	CALL  	SHOWTIME 	;显示时间
        MOV     AH,2CH
        INT     21H
        POP     BX
        PUSH    BX
        SUB     DH,BH
        JB      BELOWZERO2
        JMP     ABOVEZERO2
    
BELOWZERO2:
        ADD     DH, 60
        CMP     DH, 14
        JB      LOOPTIME
        JMP     EXENEXT2
     
ABOVEZERO2:
        CMP     DH, 14
        JB      LOOPTIME
     
EXENEXT2:
        POP     BX
        AND     BX, 0H
        MOV     AH, 2CH
        INT     21H
        PUSH    DX
     
LOOPSTAR:
	CALL 	PRINTSTAR       ;打印星星
        MOV     AH, 2CH
        INT     21H
        POP     BX
        PUSH    BX
        SUB     DH,BH
        JB      BELOWZERO3
        JMP     ABOVEZERO3
    
BELOWZERO3:
        ADD     DH, 60
        CMP     DH, 14
        JB      LOOPSTAR
        JMP     EXENEXT3
    
ABOVEZERO3:
        CMP     DH, 14
        JB      LOOPSTAR
   
EXENEXT3:
        POP     BX
        AND     BX, 0H
        CALL    RANDOMSORT      ;随即数快排
 

        SETCRT
        CLEAR
        MOV     AH, 4CH
        INT     21H
   
;==========随即数快排子程序=========
RANDOMSORT  PROC NEAR
	
    
         CALL    MAINSORT     ;调用主函数
         MOV     AH, 0BH
         INT     21H
         CMP     AL, 00H      ;如果键盘有键入，退出程序
         JNZ     EXITRANDOM
         CALL    DELAYBIG     ;延迟转到音乐子程序
         CALL    DELAYBIG     ;延迟转到音乐子程序
         CALL    DELAYBIG     ;延迟转到音乐子程序
         MOV     CX, 5
         LEA     BX, ARRAY
    
CLEARARRAY:
         MOV    AX, 0
         MOV    [BX], AX
         ADD    BX, 2
         LOOP   CLEARARRAY
         JMP    LOOPSTART      ;转到音乐子程序
      

;退出程序，退出前清屏
EXITRANDOM:
         SETCRT
         CLEAR
         CALL  DISPLAY
         MOV   AH, 4CH
         INT   21H
    
GETARRAY PROC NEAR
    
       
        LEA     BX, ARRAY
        MOV     AH, 2CH     ;读系统时间
        INT     21H
        MOV     [BX],DL       ;取秒值
        ADD     BX, 1
        CALL    DELAYBIG
    
        MOV     AH,2CH       ;读系统时间
        INT     21H
        MOV     [BX],DL       ;取秒值
        ADD     BX, 1
        CALL    DELAYBIG
    
        MOV     AH,2CH         ;读系统时间
        INT     21H
        MOV     [BX],DL        ;取秒值
        ADD     BX, 1
        CALL    DELAYBIG

        MOV     AH,2CH          ;读系统时间
        INT     21H
        MOV     [BX],DL          ;取秒值
        ADD     BX, 1
        CALL    DELAYBIG

  
        MOV     AH,2CH           ;读系统时间
        INT     21H
        MOV     [BX],DL           ;取秒值
        ADD     BX, 1
        CALL    DELAYBIG

        MOV     AH,2CH            ;读系统时间
        INT     21H
        MOV     [BX],DL            ;取秒值
        ADD     BX, 1
        CALL    DELAYBIG
        

        MOV     AH,2CH             ;读系统时间
        INT     21H
        MOV     [BX],DL            ;取秒值
        ADD     BX, 1
        CALL    DELAYBIG

        MOV     AH,2CH             ;读系统时间
        INT     21H
        MOV     [BX],DL            ;取秒值
        ADD     BX, 1
        CALL    DELAYBIG

        MOV     AH,2CH              ;读系统时间
        INT     21H
        MOV     [BX],DL             ;取秒值
        ADD     BX, 1
        CALL    DELAYBIG

        MOV     AH,2CH              ;读系统时间
        INT     21H
        MOV     [BX],DL              ;取秒值
        ADD     BX, 1

        RET
GETARRAY ENDP
DELAYBIG PROC NEAR

        CALL DELAY
        CALL DELAY
        CALL DELAY
        CALL DELAY
        CALL DELAY
        CALL DELAY
        CALL DELAY
        CALL DELAY
        CALL DELAY
        CALL DELAY
        CALL DELAY
        CALL DELAY
        CALL DELAY
        CALL DELAY     
        RET
DELAYBIG ENDP    
MAINSORT PROC NEAR
        CURSOR  14, 33       ;设置光标
        LEA     DX, [INF1]   ;提示语句
        MOV     AH, 09H
        INT     21H
	
        MOV AX, DATA
        MOV DS, AX
        CALL GETARRAY
        MOV CX,COUNT
        MOV LEN,COUNT
        LEA SI,[ARRAY]       ;传递两个参数,调用函数在屏幕上显示数组
        LEA DI,[BUF2]
	
        CALL DISPLAYARRAY
        LEA DI,[ARRAY]       ;下面5句使di指向数组的最后一个数字 
        MOV AL,[LEN]
        MOV AH,0
        ADD DI,AX
        DEC DI
    
        CALL QUICKSORT       ;调用快速排序函数,直接整理数组里面的元素
        CURSOR 15,33         ;设置光标
        LEA DX,[INF2]        ;提示语句
        MOV AH,09H
        INT 21H
        LEA SI,[ARRAY]       ;调用函数,显示数组
        LEA DI,[BUF2]
        CALL DISPLAYARRAY
        RET
MAINSORT ENDP
DELAY PROC 
        MOV CX,0FFFFH

L3: 
	LOOP L3
        RET

DELAY ENDP

;函数功能:对于si和di范围内的数字进行整理,使左边的所有数都不大于右边的任意一个,返回中间位置的指针
;输入参数:si 指向待整理范围的第一个数字,而且也以这个数字作为分界,比这个数字大的放右边,比他小的放左边
; di 指向待整理范围的最后一个数字
;输出参数:dx 指向整理后的中间数字,在他左边的数字都不大于他右边的任意一个数字

PARTITION PROC NEAR

        PUSH SI
        PUSH DI
        PUSH AX
        MOV AL,[SI]   ;保存第一个数字,作为比较大小的标准
        DEC SI        ;si后移,因为下面循环的第一步是si前移,所以要先后移一位,不然会以后第一个数
        INC DI        ;di前移,原因如上
P_L1:
P_L2:
        DEC DI         ;di前移,如果[di]比标准数字大,则继续循环,这个循环结束后,di指向从右边开始第一个比标准数字小的数字(如果存在的话)
        CMP [DI],AL
        JA  P_L2
P_L3:
        INC SI         ;si后移,如果[si]比标准数字小,则继续循环,这个循环结束后,di指向从右边开始第一个比标准数字大的数字(如果存在的话)
        CMP [SI],AL
        JB P_L3
        CMP SI,DI      ;如果此时si>=di,那么跳出循环,整理结束
        JNB P_S1
 
        MOV AH,[DI]       ;这三句是交换[si]和[di]
        XCHG [SI],AH
        MOV [DI],AH
        JMP P_L1          ;如果si还是小于di,那么继续开始循环,知道整理结束
P_S1:
        MOV DX,DI         ;现在di指向中间元素,赋值个dx,作为返回值
        POP AX
        POP DI
        POP SI
       RET
PARTITION  ENDP
;==========排序=============
;函数功能:对si和di范围内的数组进行排序
;输入参数:si 指向排序范围的第一个数字
; di 指向排序范围的最后一个数字
;输出参数:无
QUICKSORT PROC NEAR

        PUSH SI        ;寄存器入栈,保护现场
        PUSH DI
        PUSH DX
        CMP SI,DI      ;如果si>=di,则直接返回
        JGE RETURN
        CALL PARTITION  ;调用函数,整理数组,返回值dx,在dx左边的数字都不大于右边的数字
        PUSH DI         ;保存di
        MOV DI,DX       ;对于si和bx范围内的数组,递归调用本函数
        CALL QUICKSORT
        POP DI          ;取出di
        MOV SI,DX       ;对dx+1和di范围内的数组,递归调用本函数
        INC SI
        CALL QUICKSORT
RETURN:
        POP DX          ;寄存器出栈,恢复现场
        POP DI
        POP SI

     RET
QUICKSORT ENDP

;=====函数功能:把si指向的数组,转变成一个显示的字符串,并输出=====
;=====输入参数:si 指向需要处理的数组======
;===== di 保存转换后字符串的位置======
;=====输出参数:无(直接在屏幕上显示字符串)=====
DISPLAYARRAY PROC NEAR

        PUSH AX      ;寄存器入栈
        PUSH BX
        PUSH CX
        PUSH DX
        PUSH DI
        PUSH SI
        MOV CL,[LEN]   ;设置循环次数等于数组元素个数
        MOV CH,0
        PUSH DI        ;di指向的是存放转换后字符串的地方,这个地方最后赋值给dx,可以显示
    
DA_L1:                  ;因为每个数字最多为三位,所以下面直接进行处理,不写通用的转换程序
        MOV AL,[SI]     ;当前处理的数字放入ax
        MOV AH,0
        MOV BL,100      ;16位除8位的除法
        DIV BL
        CMP AL,0        ;如果商为0,跳转到处理余数的部分
        JZ DA_S1
        ADD AL,'0'       ;商不为0,则加上30H,放入di指向的位置,di后移
        MOV [DI],AL
        INC DI
        MOV AL,AH        ;把余数除10,然后ax加上3030H,在用16位长度放入[di]中
        MOV AH,0
        MOV BL,10
        DIV BL
        ADD AX,3030H
        MOV WORD PTR [DI],AX
        ADD DI,2
        MOV BYTE PTR [DI],','  ;后面加逗号
        INC DI                 ;di后移
        INC SI                 ;已经一个数字处理结束,si后移
        JMP DA_S3              ;跳到loop的地方
  
DA_S1:
         MOV AL,AH             ;数字不是三位数,把余数放入al,再除10
         MOV AH,0
         MOV BL,10
         DIV BL
         CMP AL,0              ;比较商是否为0,如果为0,说明是一位数,跳转到下面
         JZ DA_S2
         ADD AL,'0'            ;如果商不为0,则是两位数,商加30H,放入[di],di后移
         MOV [DI],AL
         INC DI
DA_S2:
        ADD AH,'0'             ;把余数加上30H,放入[di],di后移
        MOV [DI],AH
        INC DI
        MOV BYTE PTR [DI],','  ;放入逗号
        INC DI
        INC SI                 ;一个数字处理完成,si后移
DA_S3:
        LOOP DA_L1             ;继续处理下一个数字
        DEC DI
        MOV BYTE PTR [DI],'$'  ;现在字符串最后一位是逗号,把这个逗号换成'$'就好了
        POP DX                 ;把指向字符串第一个字符的位置出栈,赋值给dx
        MOV AH,09H             ;调用中断输出
        INT 21H
        POP SI                 ;寄存器出栈
        POP DI
        POP DX
        POP CX
        POP BX
        POP AX
RET 
DISPLAYARRAY ENDP

        RET
RANDOMSORT ENDP
;===========显示主屏幕的子程序DISPLAY=====================    
DISPLAY PROC NEAR
    MOV CX, 13H              ;主屏幕行数
    MOV AX, 0B81FH           ;显存的起始位置
    MOV ES, AX               
    MOV BX, OFFSET SHOW00
 
ROW1:
    PUSH CX
    MOV  CX,60             ;每行的列数
    MOV  SI,0H             ;数据段的起始位置
COL1:
    MOV AL,[BX]
    MOV ES:[SI],AL
    CMP AL,23H             ;当为‘#’时显示绿色
    JE COLOR11
    CMP AL,2AH             ;当为‘*’时显示黄色
    JE COLOR21
    JMP COLOR31            ;剩下的显示红色
 
COLOR11:
    MOV DI, 0H
    JMP LOOP01

COLOR21:
    MOV DI, 1H
    JMP LOOP01
  
COLOR31:
    MOV DI, 2H
    JMP LOOP01
 
COLOR41:
     MOV DI, 3H
 
LOOP01:
    MOV AH, [DI]            ;取来字符和属性
    MOV ES:[SI+1], AH        ;直接写屏
    INC BX
    ADD SI, 2              ;每次写两个字节，显示一个字符
    LOOP COL1
    POP CX

    MOV AX, ES
    ADD AX, 0AH
    MOV ES, AX
    ADD BX, 3
    LOOP ROW1
    RET
DISPLAY ENDP
	
;============循环播放音乐的子程序PLAYMUSIC
PLAYMUSIC PROC NEAR
	PUSH  	CX
        CURSOR  7,12              ;设置光标
	SHOWSTR INFORMUSIC  	;打印提示语句
	
	ADDRESS MUS_FREG1, MUS_TIME1 ;将音乐
	CALL 	MUSIC      ;调用播放音乐程序
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
	JE 	END_MUS      ;音乐播放完毕，跳出子程序
	MOV 	BX, DS:[BP]
	CALL 	GENSOUND
	ADD 	SI, 2
	ADD 	BP, 2
        MOV     AH, 0BH      ;按任意键退出总程序
        INT     21H
        CMP     AL,00H
        JNZ     EXITMUSIC
    
	JMP 	FREG

;在退出程序前，清屏
EXITMUSIC:
        SETCRT
        CLEAR
        MOV AH,4CH
        INT 21H
    
END_MUS:
	RET
MUSIC  	ENDP

;===========打印星星的子程序=========
PRINTSTAR PROC NEAR

;按任意键退出总程序
EXITSTAR MACRO
        PUSH AX
        MOV AH,0BH
        INT 21H
        CMP AL,00H
        JNZ EXITT
        POP AX
        ENDM
        AND CX,0         ;用cx控制子程序一次执行时间
LOOP1: 

	MOV BX,7
        MOV AL,12       
        MOV DL, 9       
LOOP2: 	
	CURSOR  AL,DL    	;设置光标
     ;   SHOWSTR BUFSTAR1                
        CALL COLOR_START         ;打印语句
        CALL DELAYSTAR  	;延迟打印星星
        CALL DELAYSTAR
        EXITSTAR                 ;键入任意键，退出
        CALL DELAYSTAR
        CALL DELAYSTAR
        EXITSTAR                 ;键入任意键，退出
        CALL DELAYSTAR
        CALL DELAYSTAR
        EXITSTAR                 ;键入任意键，退出
        EXITSTAR
        CALL DELAYSTAR
        CURSOR  AL,DL
        SHOWSTR   BUFSTAR2
        INC AL
        ADD DL,3
	INC CX
	CMP CX, 0fH 		   ;当cx等于0fH时 		
        JZ END_STAR                ;退出打印星星子程序
        DEC BX
        JNZ LOOP2
        JMP LOOP1

;退出前，清屏
EXITT:
        SETCRT
        CLEAR
        MOV AH,2CH
        INT 21H

;延迟子程序
DELAYSTAR  PROC NEAR

        PUSH CX
        MOV  CX, 0FFFFH

L3STAR: 	
	LOOP L3STAR
        POP CX
        RET
DELAYSTAR  ENDP
COLOR_START  PROC  NEAR
        PUSH AX
        PUSH BX
        PUSH CX 
        MOV AH,9                 ;打印品红色的星星
        MOV AL,2AH               ;‘*’的ASCII码
        MOV BL,0DH               ;品红色
        MOV CX ,1                ;循环次数
        INT 10H
        POP CX
        POP BX
        POP AX
         
 
          RET
COLOR_START ENDP

END_STAR:
	
	RET
PRINTSTAR ENDP

;==============显示时间的子程序======
SHOWTIME PROC NEAR

       CURSOR 7,40      ;设置光标
       SHOWSTR BUFTIME1 ;打印语句

    
        AND  BX, 0       ;用bx控制子程序一次运行时间
LOOPR:
        MOV AH, 2CH
        INT 21H
        PUSH CX
        MOV CH,DL
        TIMER1 CH,MSECOND
        MOV CH,DH
        TIMER1 CH,SECOND
        POP CX
        TIMER1 CL,MINUTE
        TIMER1 CH,HOUR
        CURSOR 9,40     ;设置光标
        SHOWSTR HOUR
        SHOWSTR MINUTE
        SHOWSTR SECOND
        SHOWSTR MSECOND

        INC BX 
        CMP BX,0FFH         ;当bx为0ffh时
        JE  END_TIME        ;跳出子程序
        MOV AH, 0BH
        INT 21H
        CMP AL,00H
        JNZ EXITA           ;有键入时，退出总程序
        JMP LOOPR           ;否则继续显示当前时间
    
;退出前，清屏	
EXITA:
        SETCRT
        CLEAR
        MOV AH,4CH
        INT 21H
     

END_TIME:
	
	RET
SHOWTIME ENDP

CODE ENDS
	END START


