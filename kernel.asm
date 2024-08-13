;TITLE Homework 7 Program 1

; Description: Kernel for PongOS - loaded and executed by the bootloader.asm code
;			Everything was written from scratch - we included our own graphics routines, our own interrupt handlers, our own text routines and fonts
;			Game includes scoring in Hex, random initial ball direction (after the first game), pause and resume, graphics mode
;			This pong implementation is mostly event-driven - the only thing that is done inside of a "forever" loop is drawing the screen
;			Everything else - paddle movement, ball movement, keypresses are handled by hardware events by way of our own interrupt
;												service routines for the timer system and keyboard.
; James Coleman - kernel.ASM
; Revision date: 12/12/08

org 10000h

main:
	xor ah, ah
	mov al, 13h
	int 10h	; Have BIOS setup Mode 13h graphics
	
	cli
	mov ax, 0a000h	; setup extened segment to point to graphics memory
	mov es, ax
	sti
	
	call SetupISRs
	call SetupKeyboard
	
	.mainLoop:
		call NewGame
		loop .mainLoop
		
	.endLoop:
		hlt
		jmp .endLoop

; Function: Patch the Interrupt Vector Table with pointers to our code
; Receives: N/A
; Returns: N/A
; Requires: AX, ES
; Written by: James Coleman
SetupISRs:
	cli
	push es
	mov ax, 0
	mov es, ax
	
	lea ax, [TimerISR]
	mov [es:1ch * 4], ax
	mov [es:1ch * 4 + 2], cs
	
	mov ax, [es:09h * 4]
	mov [keyISRoriginal], ax
	mov ax, [es:09h * 4 + 2]
	mov [keyISRoriginal + 2], ax
	lea ax, [KeypressISR]
	mov [es:09h * 4], ax
	mov [es:09h * 4 + 2], cs
	
	pop es
	sti
	ret

; Function: Sets up the keyboard controller
; Receives: N/A
; Returns: N/A
; Requires: AX
; Written by: James Coleman
SetupKeyboard:
	cli
	
	; Setup keypress repeat
	;mov al, 0f3h
	;out 60h, al
	;mov al, 00000111b
	;out 60h, al	; Delay of 1/4 second and middleground repeat rate
	
	mov al, 0f8h
	out 60h, al	; set all keys generate up and down signals
	
	sti
	ret

; ****************************** Interrupt Service Routines *******************

; Function: Timer ISR - handles movement of coordinates
; Receives: N/A
; Returns: N/A
; Requires: N/A
; Written by: James Coleman
TimerISR:
	pushf
	push ax
	push bx
	push cx
	push si
	push di
	cli
	
	mov ax, 1000h
	mov ds, ax
	
	mov bl, [kbdKeyFlags]
	shr bl, 5
	jc .isPaused
	
	; This needs to handle the ball movements (at least adjusting the coordinates)
	; 1. Add dX and dY to curX and curY to compute nX and nY - Move the ball
	mov ax, [ballNX]
	add ax, [ballDX]
	mov [ballNX], ax
	mov ax, [ballNY]
	add ax, [ballDY]
	mov [ballNY], ax
	
	call CheckBallCollisions
	call MovePaddles
	
	.isPaused:	; Don't move anything
	;mov al, 20h
	;out 20h, al	; acknowledge end of interrupt to PIC - Shouldn't be necessary on this int, because it is software called
	
	sti
	pop di
	pop si
	pop cx
	pop bx
	pop ax
	popf
	iret
	
; Function: Keyboard ISR - handles all keypresses
; Receives: N/A
; Returns: N/A
; Requires: N/A
; Written by: James Coleman
KeypressISR:
	pushf
	push ax
	push bx
	cli
	
	mov ax, 1000h
	mov ds, ax	; make sure that data segment is set up correctly
	in	al, 60h	; get scan code from keyboard
	mov bl, 01h	; flag for key down (use or to enable a single bit)
	cmp al, 80h	; break scan code from a release instead of the make code from a keypress
	jb .HandleKeyDown
	mov bl, 0feh	; flag for key up (use and to disable a single bit)
	and al, 01111111b	; get actual scan code for key released
	
	.HandleKeyUp:
	cmp al, 1eh
	jne .LeftPaddleUp
		rol bl, 1; Left Paddle Down - bit 1
		and [kbdKeyFlags], bl
	.LeftPaddleUp:
	cmp al, 10h
	jne .RightPaddleDown
		and [kbdKeyFlags], bl; Left Paddle Up - bit 0
	.RightPaddleDown:
	cmp al, 26h
	jne .RightPaddleUp
		rol bl, 3; Right Paddle Down - bit 3
		and [kbdKeyFlags], bl
	.RightPaddleUp:
	cmp al, 18h
	jne .EndKBDCode	; unused key
		rol bl, 2; Right Paddle Up - bit 2
		and [kbdKeyFlags], bl
	jmp .EndKBDCode
	
	.HandleKeyDown:
	cmp al, 1eh
	jne .LeftPaddleUpD
		shl bl, 1	; Left Paddle Down - bit 1
		or [kbdKeyFlags], bl
		jmp .OtherKeyChecks
	.LeftPaddleUpD:
	cmp al, 10h
	jne .RightPaddleDownD
		or [kbdKeyFlags], bl	; Left Paddle Up - bit 0
		jmp .OtherKeyChecks
	.RightPaddleDownD:
	cmp al, 26h
	jne .RightPaddleUpD
		shl bl, 3	; Right Paddle Down - bit 3
		or [kbdKeyFlags], bl
		jmp .OtherKeyChecks
	.RightPaddleUpD:
	cmp al, 18h
	jne .OtherKeyChecks	; not a paddle movement key
		shl bl, 2	; Right Paddle Up - bit 2
		or [kbdKeyFlags], bl
		; fall through to .OtherKeyChecks on purpose
		
	.OtherKeyChecks:
	cmp al, 39h
	jne .EndKBDCode
		shl bl, 4	; Spacebar was pressed - bit 4
		xor [kbdKeyFlags], bl
		push dx
		rdtsc
		mov [beginGameClockTicks], ax
		pop dx
	
	.EndKBDCode:
	in al, 61h	; get current control byte
	or al, 80h	; set the enable bit to ackowledge reading of the keyboard
	out 61h, al
	and al, 7fh
	out 61h, al
	mov al, 20h
	out 20h, al	; acknowledge end of interrupt to PIC
	
	pop bx
	pop ax
	popf
	sti
	iret

; ****************************** Game Routines *******************
; ****************************************************************

; Function: Setup initial variable values for a new game
; Receives: N/A
; Returns: N/A
; Requires: BX, AX
; Written by: James Coleman
SetupGame:
	mov word [ballCurX], 160
	mov word [ballNX], 160
	mov word [ballCurY], 100
	mov word [ballNY], 100
	
	mov word [paddleLCurX], 2
	mov ax, [topWallY]
	mov bx, [bottomWallY]
	sub bx, ax
	add bx, [paddleYSize]
	shr bx, 1	; divide by 2
	sub bx, [paddleYSize]
	mov word [paddleLCurY], bx
	mov word [paddleLNY], bx
	mov word [paddleRCurX], 317
	mov word [paddleRCurY], bx
	mov word [paddleRNY], bx
	
	mov word [gameOver], 0
	mov word [kbdKeyFlags], 10h
	
	; Draw background on the screen
	mov ax, [paddleXSize]
	shl ax, 1	; multiply by two to account for both paddles
	mov bx, 320
	sub bx, ax
	push bx	; X-Size
	mov ax, [topWallY]
	mov bx, [bottomWallY]
	sub bx, ax
	inc bx
	push bx
	push word [paddleXSize]	;	Offset from left of screen for the X-Coord
	push word [topWallY]	; Y-Coord
	mov bh, 1
	call PutRect
	
	; Draw paddle backgrounds
	push word [paddleXSize]
	mov ax, [topWallY]
	mov dx, [bottomWallY]
	sub dx, ax
	inc dx
	push dx
	push word 0
	push word [topWallY]
	mov bh, 0
	call PutRect
	push word [paddleXSize]
	push dx
	push word [paddleRCurX]
	push word [topWallY]
	mov bh, 0
	call PutRect
	
	mov cx, 190	; Y-Coord
	mov dx, 5	; X-Coord
	mov bl, 7
	lea si, [msgToBegin]
	call PutString
	
	mov ax, [beginGameClockTicks]
	and ax, 0fh
	push dx
	
	; Get random direction for ball
	xor dx, dx
	mov bx, 4
	div bx	; setup for cases below
	
	mov bx, 4
	cmp dl, 0
	je .setBallAngle1	; +,+
	mov bx, 3
	cmp dl, 1
	je .setBallAngle1	; -,+
	mov bx, 2
	cmp dl, 2
	je .setBallAngle1	; -,-
	mov bx, 1
	cmp dl, 3
	je .setBallAngle1	; +,-
	
	.setBallAngle1:
	mov ax, 1
	cmp dl, 0
	je .setBallAngle2	; +,+
	mov ax, 2
	cmp dl, 1
	je .setBallAngle2	; -,+
	mov ax, 3
	cmp dl, 2
	je .setBallAngle2	; -,-
	mov ax, 4
	cmp dl, 3
	je .setBallAngle2	; +,-
	
	.setBallAngle2:
	cmp dl, 0
	je .setBallDir	; +,+
	neg ax
	cmp dl, 1
	je .setBallDir	; -,+
	neg bx
	cmp dl, 2
	je .setBallDir	; -,-
	neg ax
	cmp dl, 3
	je .setBallDir	; +,-
	
	.setBallDir:
	pop dx
	mov word [ballDX], ax
	mov word [ballDY], bx
		
	ret

; Function: Main game loop - runs until a game is over
; Receives: N/A
; Returns: N/A
; Requires: AX
; Written by: James Coleman
PlayGame:
	.gameLoop:	; mainly needs to handle drawing the screen
		sti
		call RedrawBall
		call RedrawRightPaddle
		call RedrawLeftPaddle
		call RedrawRightScore
		call RedrawLeftScore
		call DisplayKeyFlags	; Debug view
		hlt
		mov al, [gameOver]
		cli
		shr al, 1
		jnc .gameLoop
		sti
	ret

; Function: Creates a new game
; Receives: N/A
; Returns: N/A
; Requires: N/A
; Written by: James Coleman
NewGame:
	call SetupGame
	call PlayGame
	ret

; Function: Adjusts the coordinates of the left and right paddles
; Receives: N/A
; Returns: N/A
; Requires: AX, BX, DX
; Written by: James Coleman
MovePaddles:
	mov ax, [paddleDY]	; absolute value to move paddle by on the Y-axis
	mov dx, ax
	neg dx
	mov bl, [kbdKeyFlags]	; setup for switch cases
	mov cx, [bottomWallY]	; largest Y coordinate
	inc cx
	sub cx, [paddleYSize]	; offset the largest Y by the size of the paddle
	mov di, [topWallY]
	
	.MLeftPaddleUp:
	shr bl, 1
	jnc .MLeftPaddleDown
		add [paddleLNY], dx
		cmp word [paddleLNY], di
		ja .MLeftPaddleDown
		mov word [paddleLNY], di
	.MLeftPaddleDown:
	shr bl, 1
	jnc .MRightPaddleUp
		add [paddleLNY], ax
		cmp [paddleLNY], cx
		jna .MRightPaddleUp
		mov [paddleLNY], cx
	.MRightPaddleUp:
	shr bl, 1
	jnc .MRightPaddleDown
		add [paddleRNY], dx
		cmp word [paddleRNY], di
		ja .MRightPaddleDown
		mov word [paddleRNY], di
	.MRightPaddleDown:
	shr bl, 1
	jnc	.EndMovePaddles
		add [paddleRNY], ax
		cmp [paddleRNY], cx
		jna .EndMovePaddles
		mov [paddleRNY], cx
		
	.EndMovePaddles:
	ret

; Function: Displays the status of the current paddle control keys flags
; Receives: N/A
; Returns: N/A
; Requires: BH, SI, DI
; Written by: James Coleman
DisplayKeyFlags:
	push word 4	; xsize
	push word 1	; ysize
	push word 158	; x
	push word 4	; y
	mov bh, 15
	call PutRect
	push word 4	; xsize
	push word 1	; ysize
	push word 158	; x
	push word 6	; y
	mov bh, 15
	call PutRect
	
	; PutPixel: BH - Attribute Byte; DI - X Coord; SI - Y Coord
	mov si, 5
	mov bh, 0
	
	mov bl, [kbdKeyFlags]	; setup for switch cases
	.LUp:
	shr bl, 1
	jnc .LDn
		mov bh, 15
	.LDn:
		mov di, 158
		call PutPixel
		xor bh, bh
	shr bl, 1
	jnc .RUp
		mov bh, 15
	.RUp:
	mov di, 159
	call PutPixel
	xor bh, bh
	shr bl, 1
	jnc .RDn
		mov bh, 15
	.RDn:
		mov di, 160
		call PutPixel
		xor bh, bh
	shr bl, 1
	jnc	.endDispKFlags
		mov bh, 15
	.endDispKFlags:
		mov di, 161
		call PutPixel
	ret

; ****************************** Graphics Routines *******************
; 		Color 255 is transparency
; ********************************************************************

; Function: Draws the current score onto the screen
; Receives: N/A
; Returns: N/A
; Requires: N/A
; Written by: James Coleman
RedrawLeftScore:
	mov ax, [leftScore]
	cmp ax, [oldLeftScore]
	je .endRedrawScoreLeft
	push word 24
	push word 10
	push word 0
	push word 0
	mov bh, 0
	mov [oldLeftScore], ax
	call PutRect	; cover up old score
	mov ax, [leftScore]
	mov dx, 0
	mov cx, 0
	mov bl, 4
	call PutHexNumber	; draw current score
	.endRedrawScoreLeft:
	ret

; Function: Draws the current score onto the screen
; Receives: N/A
; Returns: N/A
; Requires: N/A
; Written by: James Coleman
RedrawRightScore:
	mov ax, [rightScore]
	cmp ax, [oldRightScore]
	je .endRedrawScoreRight
	push word 24
	push word 10
	push word 294
	push word 0
	mov bh, 0
	mov [oldRightScore], ax
	call PutRect	; cover up old score
	mov ax, [rightScore]
	mov dx, 294
	mov cx, 0
	mov bl, 4
	call PutHexNumber	; draw current score
	.endRedrawScoreRight:
	ret

; Function: Erases old ball and draws it in its new location on the screen
; Receives: N/A
; Returns: N/A
; Requires: AX, BX, SI, DI
; Written by: James Coleman
RedrawBall:	; Note any access to the N coordinate vars needs to be locked from interrupts
	; erase old ball
	mov bh, 200
	mov di, [ballCurX]
	mov si, [ballCurY]
	cli
	cmp di, [ballNX]	; check to see if coordinates have changed
	jne .needBallErase
	cmp si, [ballNY]
	je .drawBall	; neither X nor Y has changed
	sti
	.needBallErase:
	push word [ballXSize]
	push word [ballYSize]
	push di	; X-Coord
	push si	; Y-Coord
	mov bh, 1
	call PutRect
	
	; redraw at current pos
	.drawBall:
	cli
	mov bx, [ballNX]	; set cur coords equal to new coords
	mov ax, [ballNY]
	sti
	mov [ballCurX], bx
	mov [ballCurY], ax
	
	lea si, [ballBMP]
	push word [ballXSize]
	push word [ballYSize]
	push bx	; X-Coord
	push ax	; Y-Coord
	call PutBMP
	
	ret

; Function: Checks to see if the ball on movement has hit any walls or paddles and adjusts velocity accordingly
; Receives: N/A
; Returns: If player lost, sets flag in [gameOver] accordingly
; Requires: N/A
; Written by: Brad Yinger (minor edits for scoring by James Coleman)
CheckBallCollisions:
	push word ax
	call CheckLeftPaddleCollision
	cmp ax, 0
	jne .walls	; collided (don't check the other paddle)
	call CheckRightPaddleCollision
	cmp ax, 0
	jne .walls
	call CheckLeftWallCollision
	cmp ax, 0
	jne .collisionLeft
	call CheckRightWallCollision
	cmp ax, 0
	jne .collisionRight
	.walls:
	call CheckTopWallCollision
	cmp ax, 0
	jne .done	; collided (don't check other wall)
	call CheckBottomWallCollision
	cmp ax, 0
	je .done
	mov ax, [bottomWallY]
	sub ax, [ballYSize]
	mov [ballNY], ax
	.done:
	pop word ax
	ret
	.collisionLeft:
	inc word [rightScore]
	jmp .endGameCollision
	.collisionRight:
	inc word [leftScore]
	.endGameCollision:
	pop word ax
	or byte [gameOver], 1
	ret
	
; Function: Checks for collision of ball and left paddle and adjusts vectors accordingly
; Recieves: N/A
; Returns: non-zero in ax if collision occurred
; Requires: N/A
; Written by: Brad Yinger
CheckLeftPaddleCollision:
	mov ax, [ballNX]
	add ax, [ballDX]
	sub ax, [paddleLCurX]
	sub ax, [paddleXSize]
	cmp ax, [ballDX]
	jg .exit0
						
	; check y
	mov ax, [ballNY]
	add ax, [ballYSize]
	sub ax, [paddleLCurY]
	js .exit0
	sub ax, [ballYSize]
	cmp ax, [paddleYSize]
	jg .exit0
	
	; collide left paddle
	push word si
	lea si, [ballDX]
	call Collide
	pop word si
	or ax, 1
	ret
	.exit0:
	xor ax, ax
	ret
	
; Function: Checks for collision of ball and right paddle and adjusts vectors accordingly
; Recieves: N/A
; Returns: non-zero in ax if collision occurred
; Requires: N/A
; Written by: Brad Yinger
CheckRightPaddleCollision:
	mov ax, [paddleRCurX]	
	sub ax, [ballNX]
	sub ax, [ballXSize]
	cmp ax, [ballDX]
	jg .exit0
						
	; check y
	mov ax, [ballNY]
	add ax, [ballYSize]
	sub ax, [paddleRCurY]
	js .exit0
	sub ax, [ballYSize]
	cmp ax, [paddleYSize]
	jg .exit0
	
	; collide right paddle
	push word si
	lea si, [ballDX]
	call Collide
	pop word si
	or ax, 1
	ret
	.exit0:
	xor ax, ax
	ret
	
; Function: Checks for collision of ball and top wall and adjusts vectors accordingly
; Recieves: N/A
; Returns: non-zero in ax if collision occurred
; Requires: N/A
; Written by: Brad Yinger
CheckTopWallCollision:
	mov ax, [ballNY]
	add ax, [ballDY]
	cmp ax, [topWallY]
	jg .exit0TW
	
	; collide with top wall
	push word si
	lea si, [ballDY]
	call Collide
	pop word si
	or ax, 1
	ret
	.exit0TW:
	xor ax, ax
	ret

; Function: Checks for collision of ball and bottom wall and adjusts vectors accordingly
; Recieves: N/A
; Returns: non-zero in ax if collision occurred
; Requires: N/A
; Written by: Brad Yinger
CheckBottomWallCollision:
	mov ax, [bottomWallY]
	sub ax, [ballNY]
	sub ax, [ballYSize]
	cmp ax, 0
	jg .exit0BW
	
	; collide with bottom wall
	push word si
	lea si, [ballDY]
	call Collide
	pop word si
	or ax, 1
	ret
	.exit0BW:
	xor ax, ax
	ret
	
; Function: Checks for collision of ball and left wall
; Recieves: N/A
; Returns: non-zero in ax if collision occurred
; Requires: N/A
; Written by: Brad Yinger
CheckLeftWallCollision:
	mov ax, [leftWallX]
	cmp ax, [ballNX]
	jl .exit0LW
	or ax, 1
	ret
	.exit0LW:
	xor ax, ax
	ret
	
; Function: Checks for collision of ball and right wall
; Recieves: N/A
; Returns: non-zero in ax if collision occurred
; Requires: N/A
; Written by: Brad Yinger
CheckRightWallCollision:
	mov ax, [ballNX]
	add ax, [ballXSize]
	cmp ax, [rightWallX]
	jl .exit0RW
	or ax, 1
	ret
	.exit0RW:
	xor ax, ax
	ret
	
; Function: Collides the ball with a surface
; Recieves: the address of the variable to be reflected in si
; Returns: Modifies the variable
; Requires: the surface to be either vertical or horizontal (not angled)
; Written by: Brad Yinger
Collide:
	push word ax
	mov ax, [si]
	shl ax, 1			; double the projection
	neg ax				; add normal vector
	add [si], ax		; add the vector to w/e velocity vector this is
	pop word ax
	ret

; Function: Erases old paddles and draws them in their new locations on the screen
; Receives: N/A
; Returns: N/A
; Requires: AX, BX, CX, SI
; Written by: James Coleman
RedrawRightPaddle:
	mov cx, [paddleRNY]
	
	; Right Paddle Movement
	push word [paddleXSize]
	push word [paddleYSize]
	push word [paddleRCurX]
	push cx
	lea si, [paddleRightBMP]
	call PutBMP	; draw paddle at new location

	mov ax, [paddleRCurY]
	cmp ax, cx
	je .noChgRightPaddleY	; cover up old paddle only if necessary (position has changed)
	push word [paddleXSize]
	mov dx, [paddleDY]
	push dx
	push word [paddleRCurX]
	
	cmp cx, ax
	jns .paddleRMovedDown	; true = positive distance
	; else = negative difference -> paddle moved up on the screen
	add ax, [paddleYSize]
	sub ax, dx
	.paddleRMovedDown:
	push ax	; CurY (which is becoming the old Y value)
	mov bh, 0
	call PutRect

	; set current location equal to the new location
	mov [paddleRCurY], cx
	.noChgRightPaddleY:
	ret

; Function: Erases old paddles and draws them in their new locations on the screen
; Receives: N/A
; Returns: N/A
; Requires: AX, BX, CX, SI
; Written by: James Coleman
RedrawLeftPaddle:
	mov cx, [paddleLNY]
	
	; Left Paddle Movement
	push word [paddleXSize]
	push word [paddleYSize]
	push word 0
	push cx
	lea si, [paddleLeftBMP]
	call PutBMP	; draw paddle at new location
	
	mov ax, [paddleLCurY]
	cmp ax, cx
	je .noChgLeftPaddleY	; cover up old paddle only if necessary (position has changed)
	push word [paddleXSize]
	mov dx, [paddleDY]
	push dx
	push word 0
	
	cmp cx, ax
	jns .paddleLMovedDown	; true = positive distance
	add ax, [paddleYSize]	; else = negative difference -> paddle moved up on the screen
	sub ax, dx
	.paddleLMovedDown:
	push ax	; CurY (which is becoming the old Y value)
	mov bh, 0
	call PutRect
	
	; set current location equal to the new location
	mov [paddleLCurY], cx
	.noChgLeftPaddleY:
	ret

; Function: Draw a pixel on the screen
; Receives: BH - Attribute Byte; DI - X Coord; SI - Y Coord
; Returns: N/A
; Requires: ES Needs to be pointing to the Graphics memory for Mode 13h
; Written by: James Coleman
PutPixel:
	cmp bh, 255
	je .endPutPixel	; transparent pixel, don't draw
	mov ax, 320
	mul si	; result in ax
	add di, ax
	mov [es:di], bh
	.endPutPixel:
	ret

; Function: Draw a bitmap array to the screen
; Receives: Stack - X-size, Y-size, X-coord, Y-coord (pushed in that order), BH = Attribute byte
; Returns: N/A
; Requires: ES Needs to be pointing to the Graphics memory for Mode 13h
; Written by: James Coleman
PutRect:
	push bp
	mov bp, sp
	push cx
	push ax
	
	mov ax, 320
	mov [curRectRowOffset], ax
	mov si, [bp + 4]
	mul si	; Y-coord
	add ax, [bp + 6]	; X-coord
	mov cx, [bp + 10]
	sub word [curRectRowOffset], cx
	mov di, ax	; setup destination index

	mov dx, [bp + 8]	; get Y-size
	xor si, si	; zero out for counting loop
	.drawRectLoop:
		mov cx, [bp + 10]	; get X-size from stack
		.rowRectLoop:
			mov [es:di], bh
			.endRowRectLoop:
			inc di
			loop .rowRectLoop	; loop through the columns in each row
		add di, [curRectRowOffset]	; move to next row in video memory
		inc si
		cmp si, dx
		jb .drawRectLoop	; loop through the rows (Y-size)

	pop ax
	pop cx
	pop bp
	ret 8
	curRectRowOffset dw 317	; variable for PutRect function
	
; Function: Draw a bitmap array to the screen
; Receives: SI - Beginning offset of array, Stack - X-size, Y-size, X-coord, Y-coord (pushed in that order)
; Returns: N/A
; Requires: ES Needs to be pointing to the Graphics memory for Mode 13h, array is in the data segment, DI
; Written by: James Coleman
PutBMP:
	push bp
	mov bp, sp
	push cx
	push ax
	
	mov ax, 320
	mov [curBMPRowOffset], ax
	mov bx, [bp + 4]
	mul bx	; Y-coord
	add ax, [bp + 6]	; X-coord
	mov cx, [bp + 10]
	sub word [curBMPRowOffset], cx
	mov di, ax	; setup destination index
	
	mov dx, [bp + 8]	; get Y-size
	xor bx, bx	; zero out for counting loop
	.drawBMPLoop:
		mov cx, [bp + 10]	; get X-size from stack
		.rowBMPLoop:
			mov ah, [si]
			cmp ah, 255
			je .endRowBMPLoop	; transparent pixel
			mov [es:di], ah
			.endRowBMPLoop:
			inc si
			inc di
			loop .rowBMPLoop	; loop through the columns in each row
		add di, [curBMPRowOffset]	; move to next row on the 
		inc bx
		cmp bx, dx
		jb .drawBMPLoop	; loop through the rows (Y-size)
	.DontDraw:
	
	pop ax
	pop cx
	pop bp
	ret 8
	curBMPRowOffset dw 320	; variable for PutBMP function

; Function: Writes a character to the screen
; Receives: AL = ascii code, BL = color from 0-254, DX = X-Coord, CX = Y-Coord
; Returns: N/A
; Requires: The characters can only range from A-Z and 1-9
; Written by: James Coleman
PutChar:
	push si
	push bx
	xor ah, ah
	mov di, 60
	cmp al, 'A'
	jae .putLetter
	.putNumber:
	sub al, '0'
	add al, 26
	jmp .getFontAddress
	.putLetter:
	sub al, 'A'
	
	.getFontAddress:
	push dx
	mul di
	pop dx
	lea si, [pongFont]
	add si, ax
	
	.getCharColor:
	cmp bl, 255
	je .drawChar
	push si
	push cx
	mov cx, 60
	.setCharColor:
		mov bh, [si]
		cmp bh, 255
		je .noChgCharColor
		mov byte [si], bl
		.noChgCharColor:
		inc si
		loop .setCharColor
	pop cx
	pop si
	
	.drawChar:
	push word 6
	push word 10
	push dx
	push cx
	call PutBMP
	pop bx
	pop si
	ret
	
; Function: Writes a string of characters to the screen
; Receives: SI = offset of string in memory, BL = color from 0-254, DX = X-Coord, CX = Y-Coord
; Returns: N/A
; Requires: The characters can only range from A-Z and 1-9 and is terminated with a null char, AX, BX
; Written by: James Coleman
PutString:
	mov [stringX], dx
	mov [stringY], cx
	.stringCharLoop:
		mov dx, [stringX]
		mov cx, [stringY]
		mov al, [si]
		cmp al, 0
		je .endPutString	; null char signifies end of the string
		cmp al, ' '
		je .stringSpace
		call PutChar
		.stringSpace:
		inc si
		add word [stringX], 6
		jmp .stringCharLoop
	.endPutString:
	ret
	stringX dw 0
	stringY dw 0

; Function: Writes a 16-bit number in hex digits to the screen
; Receives: AX = 16-bit number, BL = color from 0-254, DX = X-Coord, CX = Y-Coord
; Returns: N/A
; Requires: N/A
; Written by: James Coleman
PutHexNumber:
	push bx
	push cx
	push dx
	
	lea si, [digitString]
	mov cx, 4
	.resetHexString:
		mov byte [si], '0'
		inc si
		loop .resetHexString
	
	lea si, [digitString]
	add si, 3	; start at the end of the string
	mov bx, 16	; setup for divides in conversion loop
	mov cx, 4	; max of 4 hex digits in a 16-bit number
	.convertHexStringLoop:
		xor dx, dx
		div bx
		call GetHexChar
		mov [si], dl
		dec si
		loop .convertHexStringLoop
	lea si, [digitString]
	
	; commented out code would be used if you desire to have this method ignore leading zeros
	;mov cx, 4
	;.findFirstHexDigit
	;	cmp byte [si], 0
	;	jne .putHexString
	;	dec cx
	;	cmp cx, 1
	;	je .endPutHexNum
	;	dec si
	;	jmp .findFirstHexDigit
	
	pop dx
	.putHexString:
	;mov ax, 4
	;sub ax, cx	; compute digit offset
	;mov cx, 6	; convert that to pixel offset
	;mul cx
	;add dx, ax	; pixel offset for unused digits
	
	pop cx
	pop bx
	call PutString
	
	.endPutHexNum:
	ret
	digitString db "0000", 0
	
; Function: Converts a 4 bit number to it's hex ASCII equivalent
; Receives: DL - number to convert
; Returns: Char in DL
; Requires: N/A
; Written by: James Coleman
GetHexChar:
	and dl, 0fh
	cmp dl, 9
	ja .hexLetter
	add dl, '0'
	jmp .endGetHexChar
	.hexLetter:
	sub dl, 10
	add dl, 'A'
	.endGetHexChar:
	ret

; DEBUG METHOD - Can place in code to get a visual display of how far the program has progressed before a bug occurs
; Written by: James Coleman
TestPixel:
	pushf
	pusha
	mov bh, 4
	mov di, [testPixelDI]
	add word [testPixelDI], 2
	mov si, 50
	call PutPixel
	popa
	popf
	ret
	testPixelDI dw 50
	
; ****************************** Data Declarations *******************
SECTION .data
keyISRoriginal dd 0	; location in memory of int 9 ISR before modification

;		Pong location variables
; ball starts in the center of the screen
ballCurX dw 160
ballNX dw 160
ballCurY dw 100
ballNY dw 100
ballDX dw 3	; delta-x
ballDY dw 3	; delta-y
;ballDiameter dw 3
; paddles start at the top and outside of the screen
paddleLCurX dw 2	; This is the inside of the paddle, never changes
paddleLCurY dw 0
paddleLNY dw 0
paddleRCurX dw 317	; This is the inside of the paddle, never changes
paddleRCurY dw 0
paddleRNY dw 0
paddleDY dw 4	; delta-x (same for both paddles)

;		Graphics variables declarations
paddleXSize dw 3
paddleYSize dw 21
paddleLeftBMP db 19,19,17,6,6,19,6,6,19,6,6,19,6,6,19,6,6,19,6,6,19,6,6,19,6,6,19,6,6,19,6,6,19,6,6,19,6,6,19,6,6,19,6,6,19,6,6,19,6,6,19,6,6,19,6,6,19,6,6,19,19,19,17
paddleRightBMP db 17,19,19,19,6,6,19,6,6,19,6,6,19,6,6,19,6,6,19,6,6,19,6,6,19,6,6,19,6,6,19,6,6,19,6,6,19,6,6,19,6,6,19,6,6,19,6,6,19,6,6,19,6,6,19,6,6,19,6,6,17,19,19
ballXSize dw 3
ballYSize dw 3
ballBMP db 9,15,9,15,15,15,9,15,9

;		Miscellaneous variables
beginGameClockTicks dw 0
leftScore dw 0
oldLeftScore dw 1
rightScore dw 0
oldRightScore dw 1
gameOver db 1
kbdKeyFlags db 0
leftWallX dw 2
rightWallX dw 317
bottomWallY dw 189
topWallY dw 10
msgToBegin db "    PRESS SPACEBAR TO BEGIN OR PAUSE THE GAME", 0

pongFont db 0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,00,00,0ffh,0ffh,0ffh,0ffh,00,00,0ffh,0ffh,0ffh,0ffh,00,00,0ffh,0ffh,0ffh,00,0ffh,0ffh,00,0ffh,0ffh,00,0ffh,0ffh,00,0ffh,0ffh,00,00,00,00,0ffh,0ffh,00,0ffh,0ffh,00,0ffh,00,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,00,00,00,0ffh,0ffh,0ffh,00,0ffh,0ffh,00,0ffh,0ffh,00,0ffh,0ffh,00,0ffh,0ffh,00,00,00,0ffh,0ffh,0ffh,00,0ffh,0ffh,00,00,0ffh,00,0ffh,0ffh,0ffh,00,0ffh,00,0ffh,0ffh,0ffh,00,0ffh,00,00,00,00,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,00,00,00,00,0ffh,00,00,0ffh,0ffh,00,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,00,0ffh,0ffh,00,0ffh,0ffh,00,00,00,00,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,00,00,00,00,0ffh,0ffh,00,0ffh,0ffh,00,0ffh,0ffh,00,0ffh,0ffh,0ffh,00,0ffh,00,0ffh,0ffh,0ffh,00,0ffh,00,0ffh,0ffh,0ffh,00,0ffh,00,0ffh,0ffh,0ffh,00,0ffh,00,0ffh,0ffh,00,00,0ffh,00,00,00,00,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,00,00,00,00,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,00,00,00,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,00,00,00,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,00,00,00,00,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,00,00,00,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,00,00,00,00,0ffh,0ffh,00,0ffh,0ffh,00,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,00,00,0ffh,00,0ffh,0ffh,0ffh,00,0ffh,00,00,0ffh,0ffh,00,0ffh,0ffh,00,00,00,00,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,00,0ffh,00,0ffh,0ffh,0ffh,00,0ffh,00,0ffh,0ffh,0ffh,00,0ffh,00,00,00,00,00,0ffh,00,0ffh,0ffh,0ffh,00,0ffh,00,0ffh,0ffh,0ffh,00,0ffh,00,0ffh,0ffh,0ffh,00,0ffh,00,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,00,00,00,00,00,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,00,00,00,00,00,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,00,00,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,00,00,00,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,00,0ffh,00,0ffh,0ffh,00,0ffh,0ffh,00,0ffh,00,0ffh,0ffh,0ffh,00,00,00,0ffh,0ffh,0ffh,00,0ffh,00,0ffh,0ffh,0ffh,00,0ffh,0ffh,00,0ffh,0ffh,00,0ffh,0ffh,00,0ffh,0ffh,00,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,00,00,00,00,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,00,00,0ffh,00,00,0ffh,00,00,0ffh,00,00,0ffh,00,00,0ffh,00,00,0ffh,00,00,00,0ffh,00,0ffh,00,0ffh,00,0ffh,00,0ffh,00,0ffh,0ffh,0ffh,00,0ffh,00,0ffh,0ffh,0ffh,00,0ffh,00,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,00,00,0ffh,0ffh,00,0ffh,00,00,0ffh,0ffh,00,0ffh,00,00,0ffh,0ffh,00,0ffh,00,0ffh,00,0ffh,00,0ffh,00,0ffh,00,0ffh,00,0ffh,00,0ffh,0ffh,00,00,0ffh,00,0ffh,0ffh,00,00,0ffh,00,0ffh,0ffh,00,00,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,00,00,00,0ffh,0ffh,00,0ffh,0ffh,00,00,0ffh,00,0ffh,0ffh,0ffh,00,0ffh,00,0ffh,0ffh,0ffh,00,0ffh,00,0ffh,0ffh,0ffh,00,0ffh,00,0ffh,0ffh,0ffh,00,0ffh,00,0ffh,0ffh,00,00,0ffh,0ffh,00,00,00,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,00,00,00,00,0ffh,0ffh,00,0ffh,0ffh,0ffh,00,0ffh,00,0ffh,0ffh,0ffh,00,0ffh,00,0ffh,0ffh,0ffh,00,0ffh,00,00,00,00,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,00,00,00,0ffh,0ffh,00,0ffh,0ffh,00,00,0ffh,00,0ffh,0ffh,0ffh,00,0ffh,00,0ffh,0ffh,0ffh,00,0ffh,00,0ffh,0ffh,0ffh,00,0ffh,00,0ffh,0ffh,0ffh,00,0ffh,00,0ffh,0ffh,00,00,0ffh,0ffh,00,00,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,00,00,00,0ffh,0ffh,0ffh,00,0ffh,0ffh,00,0ffh,0ffh,00,0ffh,0ffh,00,0ffh,0ffh,00,0ffh,0ffh,00,0ffh,0ffh,00,00,00,0ffh,0ffh,0ffh,00,0ffh,0ffh,00,0ffh,0ffh,00,0ffh,0ffh,00,0ffh,0ffh,00,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,00,00,00,00,0ffh,00,0ffh,0ffh,0ffh,00,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,00,00,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,00,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,00,00,00,00,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,00,00,00,00,00,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,00,0ffh,00,0ffh,0ffh,0ffh,00,0ffh,00,0ffh,0ffh,0ffh,00,0ffh,00,0ffh,0ffh,0ffh,00,0ffh,00,0ffh,0ffh,0ffh,00,0ffh,00,0ffh,0ffh,0ffh,00,0ffh,00,0ffh,0ffh,0ffh,00,0ffh,0ffh,00,00,00,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,00,0ffh,00,0ffh,0ffh,00,0ffh,0ffh,00,0ffh,0ffh,00,0ffh,0ffh,00,0ffh,0ffh,00,0ffh,0ffh,00,0ffh,0ffh,00,0ffh,0ffh,0ffh,00,00,0ffh,0ffh,0ffh,0ffh,00,00,0ffh,0ffh,0ffh,0ffh,00,00,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,00,00,0ffh,0ffh,0ffh,0ffh,00,00,0ffh,00,00,0ffh,00,00,0ffh,00,00,0ffh,00,00,0ffh,00,00,00,0ffh,0ffh,00,00,0ffh,00,0ffh,0ffh,00,0ffh,0ffh,00,0ffh,0ffh,00,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,00,0ffh,0ffh,00,0ffh,0ffh,00,0ffh,0ffh,0ffh,00,00,0ffh,0ffh,0ffh,0ffh,00,00,0ffh,0ffh,0ffh,0ffh,00,00,0ffh,0ffh,0ffh,0ffh,00,0ffh,00,0ffh,0ffh,00,0ffh,0ffh,00,0ffh,00,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,00,0ffh,00,0ffh,0ffh,0ffh,00,0ffh,0ffh,00,0ffh,00,0ffh,0ffh,0ffh,00,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,00,00,00,00,00,0ffh,0ffh,0ffh,0ffh,00,00,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,00,00,00,00,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,00,00,00,0ffh,0ffh,00,00,0ffh,00,00,0ffh,00,0ffh,0ffh,0ffh,00,0ffh,00,0ffh,00,0ffh,00,0ffh,00,0ffh,0ffh,0ffh,00,0ffh,00,0ffh,0ffh,0ffh,00,0ffh,00,00,0ffh,00,00,0ffh,0ffh,00,00,00,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,00,00,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,00,00,00,00,00,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,00,00,00,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,00,00,00,00,00,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,00,00,00,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,00,00,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,00,00,00,00,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,00,00,0ffh,0ffh,0ffh,00,0ffh,00,0ffh,0ffh,0ffh,00,0ffh,00,0ffh,0ffh,00,0ffh,0ffh,00,0ffh,0ffh,00,00,00,00,00,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,00,00,00,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,00,00,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,00,00,00,00,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,00,00,00,0ffh,0ffh,00,00,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,00,00,00,0ffh,0ffh,00,0ffh,0ffh,0ffh,00,0ffh,00,0ffh,0ffh,0ffh,00,0ffh,00,0ffh,0ffh,0ffh,00,0ffh,0ffh,00,00,00,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,00,00,00,00,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,00,00,0ffh,0ffh,0ffh,00,0ffh,0ffh,00,0ffh,0ffh,00,0ffh,0ffh,00,0ffh,0ffh,0ffh,00,00,0ffh,0ffh,0ffh,00,0ffh,0ffh,00,0ffh,0ffh,00,0ffh,0ffh,00,0ffh,0ffh,00,0ffh,0ffh,00,0ffh,0ffh,0ffh,00,00,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,00,00,00,0ffh,0ffh,00,0ffh,0ffh,0ffh,00,0ffh,00,0ffh,0ffh,0ffh,00,0ffh,00,0ffh,0ffh,0ffh,00,0ffh,0ffh,00,00,00,00,0ffh,0ffh,0ffh,0ffh,0ffh,00,0ffh,0ffh,0ffh,0ffh,00,00,0ffh,0ffh,00,00,00,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh,0ffh

SECTION .bss	; unitialized data
paddleLeftOldBMP resb 18
paddleRightOldBMP resb 18