;TITLE Homework 7 Program 1

; Description: 
; James Coleman - bootloader.ASM
; Revision date: 11/22/08

org 7c00h

main:
	;mov		ax, 7c00h
	mov ax, cs	; standard program beginning
	mov ds, ax	; load DS with the data seg address
	mov ax, 1000h	; set up extended segment
	mov es, ax
	cli
	mov ax, 9000h
	mov ss, ax	; set up stack
	mov sp, 0ffffh
	sti
	
	;call WriteHex
	;.mtop
	;	mov al, 'a'
	;	mov cx, 26
	;	.top
	;		push cx
	;		push ax
	;		call WriteChar
	;		pop ax
	;		inc al
	;		pop cx
	;		loop .top
	;	jmp .mtop
	
	
	mov al, 10	; sectors to read
	mov bx, 0h	; offset of memory to read to
	call DiskRead
	
	mov ax, 1000h
	mov ds, ax
	push ax
	mov ax, 0
	push ax
	retf	; "jump" to our newly loaded code
	
	;mov bx, 527
	;mov cx, 256
	;.outLoop
	;	mov al, [bx]
	;	inc bx
	;	push cx
	;	push bx
	;	call WriteChar
	;	pop bx
	;	pop cx
	;	loop .outLoop
	
	;mov bx, 0200h
	;mov bx, 7e00h
	;jmp bx
		
	.ploop:
	jmp .ploop



; Function: Reads from the disk and copies into memory
; Receives: al = num sectors to read, es:bx buffer address
DiskRead:
	mov dl, 0	; specify floppy drive
	mov dh, 0	; head 0
	mov ah, 2	; disk read subroutine
	mov cx, 0002h	; cylinder 0, start at sector 2
	int 13h
	ret

; Function: Write a ASCII character to the screen using BIOS
; Receives: AL - char to output
; Returns: N/A
; Requires: N/A
WriteChar:
	call SetCursPos
	mov ah, 9	; subfunction to write char
	mov bh, 0	; video page
	mov cx, 1	; repeat
	mov bl, 0Fh
	int 10h
	ret

SetCursPos:
	mov ah, 2	; subfunction to set cursor position
	mov dh, [cursY]
	mov dl, [cursX]
	mov bh, 0
	int 10h
	inc byte [cursX]
	
	cmp byte [cursX], 80
	je .resetX
	jmp .cursYCheck
	.resetX:
	inc byte [cursY]	; drop down one line
	mov byte [cursX], 0	; go to the beginning of the line
	.cursYCheck:
	cmp byte [cursY], 25
	je .resetY
	jmp .endSetCursPos
	.resetY:
	mov byte [cursY], 0
	.endSetCursPos:
	ret
	
HexChar:
	cmp dl, 10
	jae .HexLetter	; is the char 1-9 or A->
	add dl, 30h
	jmp .endHexChar
	.HexLetter:
		sub dl, 10
		add dl, 'A'
	.endHexChar:
	ret

; Function: Writes a two-digit hex number to the screen
; Receives: AX - binary number to output as hex
; Returns: 
; Requires: That input fit into two hex digits, DX
WriteHex:
	xor dx, dx
	mov bx, 16
	div bx
	mov bl, dl	; second digit is remainder
	mov dl, al
	call HexChar
	mov al, dl
	call WriteChar
	mov dl, bl
	call HexChar
	mov al, dl
	call WriteChar
	ret

; data declarations
cursX db 0
cursY db 0

size equ	$ - main
%if size+2 > 512
  %error "code is too large for boot sector"
%endif
times	(512 - size - 2) db 0
	
; The last two words (check `xxd bootloader.bin` should be 55aa; the old
; `db 0xaa, 0x55` for some reason resulted in inverting that to aa55.	
dw	0xaa55		;2  byte boot signature

;db	"abcdefghijklmnopqrstuvwxyz0123456789-This is our test data from the disk"
