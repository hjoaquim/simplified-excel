; multi-segment executable file template.

data segment
    ; add your data here! 

;----matrix-----
matrix db 16 dup (0)
;---------------

;---Formula Reader--- 
form_buff  db 5 dup (0h),'$'
plus_flag  db 0
minus_flag db 0
div_flag   db 0
mul_flag   db 0
num1       dw 0
num2       dw 0
result_var dw 0 
;--------------------

;---files---
path0 db "C:\emu8086\MyBuild\Contents.bin",0
;----------   

;---menu----------------------------------------    
    menu_str   db "**********MENU**********",'$'
    imp_spr    db "1 - Import Spreadsheet  ",'$'
    show_spr   db "2 - Show Spreadsheet    ",'$'
    edit_spr   db "3 - Edit Spreadsheet    ",'$'
    expr_spr   db "4 - Export Spreadsheet  ",'$'
    about      db "5 - About               ",'$'
    grid_ops   db "6 - Grid Options        ",'$'
    exit       db "7 - Exit                ",'$'
;-----------------------------------------------
    
;----students // about ----- 
abtStr  db "***Work by:***","$"
alumni1 db "Henrique Joaquim - 51006","$"
alumni2 db "Serafim Ciobanu  - 50006","$"
;--------------- 

;----show spreadsheet-----
bakin_s   db "MENU",'$'
formula_s db "FORMULA:",'$'
result_s  db "RESULT:",'$'
nums      db '1','2','3','4'
letters   db 'A','B','C','D'
flag_show_spread db 0
;--------------------------
    


;---edit spreadsheet-----
a_flag db 0
b_flag db 0
c_flag db 0
d_flag db 0
1_flag db 0
2_flag db 0
3_flag db 0
4_flag db 0 
error  db "Note:",'$'
note2  db "- Operations with -1 available!",'$'
note1  db "- If invalid chars selected cell=-1",'$'
;-----------------------

;----grid ops----
op db "Choose one option (keybord):",'$'
on db "1 - Grid ON",'$'
of db "2 - Grid OF",'$'
grid_flag db 0
;----------------  

;---scan num-----
minus_sign db ? 
const_sn   db 10
;----------------

;---export spreadsheet---
a1 db "A1:"
b1 db "B1:"
c1 db "C1:"
d1 db "D1:"
a2 db "A2:"
b2 db "B2:"
c2 db "C2:"
d2 db "D2:"
a3 db "A3:"
b3 db "B3:"
c3 db "C3:"
d3 db "D3:"
a4 db "A4:"
b4 db "B4:"
c4 db "C4:"
d4 db "D4:"
for db "FOR:"

export_buffer db 121 dup (0h)
puts_fname    db "Enter file name: ",'$'
file_name     db "C:\emu8086\MyBuild\" , 50 dup (0h)
exprt_note    db "File name example: example.txt",'$'
un_create     db "Unable to create. Try again later.",0ah,0dh,"Press enter to go back to menu.",'$'
sucess_exp    db "Successfully exported!",0ah,0dh,"Press enter to go back to menu.",'$'
;------------------------

;---import spreadsheet---
wrong_name   db "Invalid File Name. Try again later.",0ah,0dh,"Press enter to go back to menu.",'$'
unable_imp   db "Unable to import. Try again later.",0ah,0dh,"Press enter to go back to menu.",'$'
sucess_imp   db "Successfully imported!",0ah,0dh,"Press enter to go back to menu.",'$'
loading      db "LOADING...",'$'
;------------------------
    
ends                                                      

stack segment
    dw   128  dup(0)
    
ends

code segment
start:
; set segment registers:
    mov ax, data
    mov ds, ax
    mov es, ax 
    
    
;*********************************code here; aka main************************************************
;---Contents.bin management----    
    mov al,0     ;al=0 read
    mov dx,offset path0
    call Fopen
    jnc SkipCreation ; If there is a Contents.bin, the carry flag will be active and it will not create a new one
    
    ; se der carry faz:
    mov ax,0  
    mov cx,0
    call Fcreate     ; Creats a Contenst.bin. The file handle is an output to AX
    mov bx,ax
    
    call Fclose
    jmp main
    
    
  SkipCreation:  
    mov bx, ax ;moving handler
    mov cx,21  ;bytes to copy
    mov dx, offset matrix
    call Fread
    
    call Fclose 
    
;--------------------------------    
    main:
    
    mov ax,13h ;graphic mode
    call setVideoMode
 
    call initMouse
    call showMouse
    call showCursor
    
    
    call menu

    mov ax, 4c00h ; exit to operating system.
    int 21h

;*********************************functions here********************************************
    
menu proc
    push ax
    push bx
    push cx
    push dx
    push di
    push si
    
            
            
    SuperMenuLoop:
    call clearKeyBuffer
    call ClearScreen
    mov dx,0h
    call setCursorPosition
    
    mov dx, offset menu_str 
    mov ah,9       
    int 21h
    
    call enter
    call enter
    
    
    Options_cicle:
        
        add dx,25
        mov ah,9    
        int 21h
        cmp dx, offset exit
        je label1
        call enter
        jmp Options_cicle
    
    ;*******CHOOSING OPTION**** 
        label1: call mouseClicked
         
        cmp cx,0158h
        ja label1     
        
        ;***Import spreadsheet (1) **
        cmp dx, 11h
        jb next1
        cmp dx, 15h
        ja next1
        
        
        call clearScreen
        call import_spread
        jmp SuperMenuLoop
        
        ;***Show spreadsheet (2) ***
        next1:
        
        cmp dx,19h
        jb next2
        cmp dx,1eh
        ja next2
                              
        call clearScreen
        call show_spreadsheet
        jmp SuperMenuLoop
        
        ;***Edit spreadsheet (3) ***
        next2:
        
        cmp dx, 21h
        jb next3
        cmp dx, 26h
        ja next3
        
        call clearScreen
        call edit_spreadsheet
        jmp SuperMenuLoop

        
        ;***Export spreadsheet (4) ***
         
        next3:
        
        cmp dx, 29h
        jb next4
        cmp dx, 2dh
        ja next4 
        
        call clearScreen
        call export_spread
        jmp SuperMenuLoop

        
        ;***About (5) ***
        next4:
        
        cmp dx, 30h
        jb next5
        cmp dx, 36h
        ja next5
        
     
        
        call clearScreen
        call about_func
        jmp SuperMenuLoop  
        
        ;***Grid Options (6) ***
         
        next5:
        
        
        cmp dx, 38h
        jb next6
        cmp dx, 3dh
        ja next6
        
        call clearScreen
        call gridOps
        jmp SuperMenuLoop
        
        ;***Exit (7) ***
        next6:
        
        cmp dx, 41h
        jb label1
        cmp dx, 46h
        ja label1
        
        call exit_func     
        
        pop si
        pop di
        pop dx
        pop cx
        pop bx
        pop ax
ret 
menu endp

;*************************************************************************************************
; Fcreate
;
; Creates a file in a specific mode
; 
; Input: Cx=File attributes 
;             mov cx, 0 ; normal - no attributes.
;             mov cx, 1 ; read-only.
;             mov cx, 2 ; hidden.
;             mov cx, 4 ; system
;             mov cx, 7 ; hidden, system and read-only!
;             mov cx, 16 ; archive 
;        Ds:Dx=ASCIZ filename
;                    
; Output: CF=0 if successful / 1 if error
;         AX=file Handle if successful / Code error if error
;
; Trump Destroys: CF, AX
;
;*************************************************************************************************             
  
  
    Fcreate proc
                                             
        mov Ah, 3Ch
        int 21h

        Ret
               
    Fcreate endp

;************************************************************************************************    
; Fopen
;
; Opens a file in a specific mode
; 
; Input: AL=Access and sharing modes
;             mov al, 0 ; read
;             mov al, 1 ; write
;             mov al, 2 ; read/write 
;        Ds:Dx=ASCIZ filename
;                    
; Output: CF=0 if successful / 1 if error
;         AX=file Handle if successful / Code error if error
;
; Trump Destroys: CF, AX
;
;*************************************************************************************************             
 
    Fopen proc
               
        mov Ah, 3Dh
        int 21h
        
        Ret
               
    Fopen endp  
    
        
;*************************************************************************************************
; Fclose
;
; Closes a file
; 
; Input: Bx: File handle
;                    
; Output: CF=0 if successful / 1 if error
;         AX=file Handle if successful / Code error if error
;
; Trump Destroys: CF, AX
;
;*************************************************************************************************             
 
    Fclose proc
                 
        mov Ah, 3Eh
        int 21h
        
        Ret
               
    Fclose endp 
    
;*************************************************************************************************
; Fread
;
; Reads a string from a file
; 
; Input: Bx: File handle
;        Cx: Number of bytes to read
;        Ds:Dx Buffer for data.
;                    
; Output: CF=0 if successful / 1 if error
;         AX=Number of bytes read, 0 if at End Of File / Code error if error
;
; Trump Destroys: CF, AX 
;
; Note: data is read beginning at current file position, and the file position is updated after
;a successful read the returned AX may be smaller than the request in CX? if a partial read
;occurred
;                               
;*************************************************************************************************             
              
; If ax=0 when proc finished, file has ended              
              
    Fread proc
              
        mov Ah, 3Fh
        int 21h
        
        Ret
               
    Fread endp  
                 
;*************************************************************************************************
; Fwrite
;
; Writes a string in a file
; 
; Input: Bx: File handle
;        Cx: Number of bytes to write
;        Ds:Dx Data to write.
;                    
; Output: CF=0 if successful / 1 if error
;         AX=Number of bytes written / Code error if error
;
; Trump Destroys: CF, AX 
;
; note: if CX is zero, no data is written, and the file is truncated or extended to the current
;position data is written beginning at the current file position, and the file position is
;updated after a successful write the usual cause for AX? < CX? on return is a full disk.
;                               
;*************************************************************************************************             
 
    Fwrite proc
                
        mov Ah, 40h
        int 21h
        
        Ret
               
    Fwrite endp



;****************************************************
; Print Matrix
;
; Input:Nothing
;
; Output:Print Matrix to STDOUT
;
; Destroys:Nuthin  
;
;****************************************************

print_matrix proc
        push ax
        push bx
        push cx
        push dx
        
        mov ax,0
        mov dh,6 ;initial coordinates -> row
        mov dl,4 ;initial coordinates -> column
        mov bx,0 ;counter
        mov cx,0 ;counter
          
        setPos:
               call setCursorPosition
               
               cmp bx,16
               je end_pm
               
               cmp cx,4
               je next_row
               
               mov al,matrix[bx]  ;char to write
               cbw
               call print_num
                 
               add dl,5
               inc cx
               inc bx
               jmp setPos
        
        next_row:
               add dh,2 ; adding to go to the next row 
               mov dl,4 ; getting the inital coordinate
               mov cx,0
               jmp setPos  


        end_pm:
               pop dx
               pop cx
               pop bx
               pop ax
               
 
ret    
print_matrix endp  

;****************************************************
; Enter Function
;
; Input:Nothing
;
; Output:Print cret and new line to stdOUT
;
; Destroys:Nuthin  
;
;****************************************************

enter proc
    push dx
    push ax
             
             mov ah, 2
	         mov dl, 0dh  ;new line ascii code
	         int 21h 
	         
	         mov ah, 2
	         mov dl, 0ah  ;cret ascii code
	         int 21h
	         
    pop ax
    pop dx
ret    
enter endp

;****************************************************
; Initialize Mouse
;
; Input:Nothing
;
; Output: AX = 000h if error // =FFFFh if detected
;         BX = number of mouse buttons 
;
; Destroys:Nuthin  
;
;****************************************************
initMouse proc 
    mov ax, 00h
    int 33h
ret
initMouse endp

;****************************************************
; Show Mouse Pointer
;
; Input:Nothing
;
; Output:Nothing
;
; Destroys:Nuthin  
;
;****************************************************
showMouse proc
    push ax 
    mov ax, 01h
    int 33h
    pop ax
ret
showMouse endp

;****************************************************
; Get Mouse Position and buttons
;
; Input:Nothing
;
; Output: BX= button pressed (1 letf, 2 right, 3 both
;         CX= horizontal position (column)
;         Dx= vertical position (row)
; 
; Destroys:Nuthin  
;
;****************************************************
getMousePos proc
    push ax
    mov ax,03h
    int 33h
    pop ax
ret
getMousePos endp 

;****************************************************
; Mouse Clicked
;
; Input:Nothing
;
; Output: BX= button pressed (1 letf, 2 right, 3 both
;         CX= horizontal position (column)
;         Dx= vertical position (row)
; 
; Destroys:bx 
;
;****************************************************
mouseClicked proc
    push ax
      
    mov bx,0h
    mov ax,0h
             
    mC_loop:
        call getMousePos
        cmp bx,0h
        je  mC_loop 
    
    
    pop ax
ret
mouseClicked endp


;****************************************************
; Set Video Mode
;
; Input: AL- video 
;           - 00h - text mode. 40x25. 16 colors. 8 pages. 
;           - 03h - text mode. 80x25. 16 colors. 8 pages. 
;           - 13h - graphical mode. 40x25. 256 colors. 320x200 pixels, 1 page. 
;
; Output:Print cret and new line to stdOUT
;
; Destroys:Nuthin  
;
;****************************************************
setVideoMode proc
    push ax
    mov ah,00h ; video mode
    int 10h
    pop ax
ret
setVideoMode endp

;****************************************************
; Set Text Mode Cursor
;
; Input: CH = Cursor start line (bits 0-4)
;             opcoes (bits 5-7)  
;        CL = Bottom Cursor (bits 0-4)
;             Bit 5 de CH a 1 esconde cursor 
; Output:Nothing
;
; Destroys:Nuthin  
;
;****************************************************
ShowCursor proc
    push cx
    mov ch,32
    mov ah,1
    int 10h
    pop cx  
ret
ShowCursor endp 

;****************************************************
; Set Cursor Position
;
; Input: DH = row
;        DL = column
;        BH = page number 0-7
; 
; Output:Nothing
;
; Destroys:Nuthin  
;
;****************************************************
setCursorPosition proc
    push ax
    mov ah,2
    int 10h
    pop ax 
ret
setCursorPosition endp


;***********************************************************************************************
; Clear Screen 
; 
; Clears the screen ---> we also set cursor position at (0,0) 
;
; Input: AL = number of lines by which to scroll (00h = clear entire window).
;        BH = attribute used to write blank lines at bottom of window.
;        CH, CL = row, column of window's upper left corner.
;        DH, DL = row, column of window's lower right corner.  
;
; Output: Nothing 
;
; Detroys: Nothing
;
;***********************************************************************************************    
clearScreen proc
    push ax
    
    mov al,00h
    call SetVideoMode
    
    mov al,13h
    call setVideoMode 

    pop ax

ret
clearScreen endp
    
;****************************************************
; Get Video Mode
;
; Input: Nop
; 
; Output:AL= video mode
;        BH= display page
;
; Destroys:AH  
;
;****************************************************    
getVideoMode proc
    mov bx,0
    mov ah, 0fh ;get video mode    
    int 10h
ret
getVideoMode endp

;****************************************************
; About Function  
;       Just printing names and numbers
;
; Input: Nop
; 
; Output:Na
;
; Destroys:Nuthing 
;
;****************************************************
About_func proc 
  push dx
  push ax
    
    
  mov dx, offset abtStr
  mov ah, 09h
  int 21h
  
  call enter
  call enter
   
  mov dx, offset alumni1
  mov ah, 09h
  int 21h
  
  call enter
  
  mov dx, offset alumni2
  mov ah, 09h
  int 21h
  
  about_loop:
  mov ah,7
  int 21h
  
  cmp al, 0ah
  jne retin
  
  cmp al, 27h
  jne about_loop 

retin:
  pop ax
  pop dx
  
  
ret
about_func endp    

;****************************************************
; Show Spreadsheet Function  
;      
; Input: matrix to print
; 
; Output: print matrix to stdout
;
; Destroys:Nuthing 
;
;****************************************************
show_spreadsheet proc
    push ax
    push cx
    push dx
    
    call PGNL
    
    cmp grid_flag,0
    je show_now
    
    call MakeGrid
    
    show_now:
            call print_matrix
            call ptfs 
            
            cmp form_buff[0], 20h
            je look_dx
            
            mov dh,14
            mov dl,13
            call setCursorPosition
           
            mov dx, offset form_buff
            mov ah,9
            int 21h
            
    look_dx:
    call mouseClicked
    
    ;---menu selection--        
    cmp dx,80h
    jb look_dx
    cmp dx,86h
    ja look_dx
    
    cmp cx,4eh
    jb look_dx
    cmp cx,8ch
    ja look_dx
    ;-------------------        

   pop dx 
   pop cx
   pop ax

ret   
show_spreadsheet endp 

;****************************************************
; Put Pixel  
;      
; Input:  AL=pixe value
;         CX=column
;         DX=row
; 
; Output: Nop
;
; Destroys:Nuthing 
;
;****************************************************
putPixel proc
       
       push ax
       push bx
       mov ah,0ch
       mov bh,00
       int 10h
       pop bx
       pop ax
        

ret
putPixel endp

;****************************************************
; GridOps  
;      
; Input: 
; 
; Output: activate/diactivate grid_flag
;
; Destroys:Nuthing 
;
;****************************************************
gridOps proc
    
    push ax
    push dx
    
    call clearKeyBuffer
    mov ax,0
    
    mov dx,offset op
    mov ah,9
    int 21h
    
    call enter
    
    mov dx,offset on
    mov ah,9
    int 21h
    
    call enter
  
    mov dx,offset of
    mov ah,9
    int 21h
    
    call enter
    
    g_o:
    mov ah,1
    int 21h
    
    cmp al, '1'
    je set_gridFlag
    
    cmp al,'2'
    je end_go
    
    jmp g_o 
    
    set_gridFlag: mov grid_flag,1
                  jmp end_sg
                  
    end_go:mov grid_flag,0
    
    end_sg:
        pop dx
        pop ax
ret
gridOps endp

;****************************************************
; Make Grid  
;      
; Input:  AL=pixe value
;         CX=column
;         DX=row
; 
; Output: Nop
;
; Destroys:Nuthing 
;
;****************************************************
MakeGrid proc 
    ;falta ver a flag
    mov al,0fh
    mov dx,45
    mov cx,20
    
    horizontal:
        cmp cx,180
        je see_dx
        
        call PutPixel
        inc cx
        jmp horizontal
        
    see_dx:
        
        cmp dx,105 ; last row
        je gettin
        
        mov cx,20   ;cx initial
        add dx,15   ;next row
        jmp horizontal
    
    gettin:
        mov dx,45  ;dx initial
        mov cx,20     ; cx initial
  
    vertical:  
        cmp dx,105  ; last row
        je see_cx
        
        call PutPixel
        inc dx
        jmp vertical
        
    see_cx:
        cmp cx,180 ; last column
        je end_mg   
        
        add cx,40 ; next column
        mov dx,45 ;;;;40 dx inicial
        jmp vertical

end_mg:
ret
makeGrid endp 

;****************************************************
; PrintGridNumsLetters 
;      
; Input:  Na  
;
; Output: Nop
;
; Destroys:Nuthing 
;
;****************************************************
PGNL proc 
      push ax
      push cx
      push dx
      push si
      
      mov cx,0
      mov si,offset letters
      mov dh,4   ;setting beggining position
      mov dl,5   ;"""
      letterss:
             call setCursorPosition
      
             cmp cx,4
             je back_cx
             
             push dx
             
             mov dl,[si]
             mov ah,2
             int 21h
             inc cx
             inc si
             
             pop dx
             
             add dl,5
             jmp letterss 
            
      back_cx:mov cx,0
              mov si,offset nums
              mov dl,1   ;setting beggining position for nums
              mov dh,6   ;""
      
      numss:call setCursorPosition
            cmp cx,4
            je end_pgnl
            
            push dx
            
            mov dl,[si]
            mov ah,2
            int 21h
            inc cx
            inc si
            
            pop dx
            
            add dh,2
            jmp numss
            
end_pgnl:
        pop si
        pop dx
        pop cx
        pop ax
     
ret
pgnl endp 

;****************************************************
; Put Result and Formula Strings (in show spreadhsheet) 
;      
; Input:  Na  
;
; Output: Nop
;
; Destroys:Nuthing 
;
;****************************************************
ptfs proc
    
    push ax
    push dx 
     
    mov dh,14  ;coordinates we want
    mov dl,5
    call setCursorPosition
    
    mov dx,offset formula_s
    mov ah,9
    int 21h 
    
    mov dh,14  ;coordinates we want
    mov dl,30
    call setCursorPosition
    
    mov dx,offset result_s
    mov ah,9
    int 21h
    
    mov dh,16 ;coordinates we want
    mov dl,5
    call setCursorPosition
    
    mov dx,offset bakin_s
    mov ah,9
    int 21h
    
    pop dx
    pop ax
 
ret
ptfs endp 

;****************************************************
; SCAN NUM
;           Read a signed number from STDIN
;
; Input:
;
; Output: CX = signed number
;         if any error Cx=-1 
;
; Destroys:CX  
;
;****************************************************
scan_num proc 
    
    push ax
    push bx
    push dx
    
   
     mov cx,0 
     mov bx,0
     mov minus_sign, 0
     
     next_digit:  
                cmp cx,128
                ja err_sn 

                cmp cx,128
                je  err_sn
                
                next:
                mov ah,1
                int 21h  ; read from keybord --> stores in AL
                
                cmp al, '-' ; checking if it is a negative number
                je set_minus 
    
                              
                cmp al,0dh    ; when enter we stop reading
                je look_minus
                
                cmp al,30h
                jae ok_ae ;cheking if its a number  '0'<=num=<'9'
   
    err_sn:     cmp cx,128
                jne dont_check_minus
    
                cmp minus_sign,1
                je next
                
                dont_check_minus:    
                mov cx, 0FFFFFFFFh     
                jmp end_sn
                
    ok_ae:
                cmp al,39h
                ja err_sn
                
                push ax ; saving our ax
                mov ax,cx
                mul const_sn ;const_sn=10
                mov cx,ax
                pop ax
                
                sub al,30h ; conver to integer
                
                mov ah,0
                add cx,ax
                jc err_sn
                
                jmp next_digit           
    
    
    set_minus:
                mov minus_sign,1
                jmp next_digit
                
    look_minus:
                cmp minus_sign,0
                je end_sn            
                
                neg cx


    end_sn: 
                pop dx
                pop bx
                pop ax

ret
scan_num endp 

;****************************************************
; PRINT NUM UNS --> print an unsigned number to stdout
;           
;
; Input: AX=int to write
;
; Output: Nop
;
; Destroys:Nuthin
;
;****************************************************
PRINT_NUM_UNS proc
    push ax
    push bx
    push cx
    push dx
    
    cmp ax,0
    je if_0
    
    mov cx,10;"divider"
    mov bx,0 ;counter
    
   div_maker:
             mov dx,0
             div cx   ; ax= ax/10 , dx=remainder (resto)
             push dx  ; saving ech digit to the stack
             inc bl   ; give us the number of divisions we made
             
             cmp ax,0
             jne div_maker
             
   prt_num:       
             pop ax ;gettin it back from the stack
             add al,30h ; int --> char
             
             ;--printing each digit--
             mov ah,2
             mov dl,al ;our digit is placed in al
             int 21h
             ;--printing each digit--
             
             dec bl
             
             cmp bl,0
             jne prt_num 
             jmp end_pnu
             
   if_0:
             mov dl,30h
             mov ah,2
             int 21h          

end_pnu:             
   pop dx
   pop cx
   pop bx
   pop ax
ret             
print_num_uns endp 

;****************************************************
; PRINT NUM --> print a signed number to stdout
;               used with PRINT_NUM_UNS to print signed num
;
; Input: AX=int to write
;
; Output: Nop
;
; Destroys:Nuthin
;
;****************************************************
PRINT_NUM       PROC
        PUSH    DX
        PUSH    AX
        
        
        ; the check SIGN of AX,
        ; make absolute if it's negative:
        CMP     AX, 0
        JNS     positive
        NEG     AX
         
        PUSH AX 
        ;PUTC    '-'
        mov ah,2
        mov dl, '-'
        int 21h
        POP AX
        
positive:
        CALL    PRINT_NUM_UNS
printed:
        POP     AX
        POP     DX
        RET
PRINT_NUM       ENDP

;****************************************************
; Edit spreadhsheet
;                   Enable user to edit spreadsheet 
;      
; Input:  Na  
;
; Output: Nop
;
; Destroys:Nuthing 
;
;****************************************************
edit_spreadsheet proc
    push ax
    push bx
    push cx
    push dx
    
    call PGNL
    
    cmp grid_flag,0
    je show_im
    
    call MakeGrid
    
    show_im: 
            call print_matrix
            call ptfs
            
            cmp form_buff[0], 20h
            je go
            
            mov dh,14
            mov dl,13
            call setCursorPosition
           
            mov dx, offset form_buff
            mov ah,9
            int 21h
            
;printing our notes---------
            go: 
            call enter
            call enter
            call enter
            call enter
                      
            mov dx,offset error 
            mov ah,9
            int 21h
            call enter 
            
            mov dx,offset note1
            mov ah,9
            int 21h 
            call enter
            
            mov dx,offset note2
            mov ah,9
            int 21h            
;--------------------------- 
           
    next_click:
            ;call clear_keybuff
            
            call reset_flags
            call mouseClicked
       
        ;---menu selection--        
        cmp dx,80h
        jb see_form
        cmp dx,86h
        ja see_form
    
        cmp cx,50h
        jb see_form
        cmp cx,8ah
        ja see_form
        
        jmp end_es
        ;------------------- 
             
        see_form:
            cmp cx,50h
            jb see_A
            cmp cx,0fch
            ja see_A
            
            cmp dx,70h
            jb see_A
            cmp dx,76h
            ja see_A
            
            call form_read
   
   
   
    ;wich column?------
    see_A:  
    
            cmp cx,28h
            jb see_B
            cmp cx,76h
            ja see_B
            
            mov a_flag,1
            jmp see_1
            
    see_B:
            cmp cx,07ah
            jb see_C
            cmp cx,0c8h
            ja see_C 
            
            mov b_flag,1
            jmp see_1
            
    see_C:
            cmp cx,0cbh
            jb see_D
            cmp cx,11ah
            ja see_D
            
            mov c_flag,1
            jmp see_1
            
    see_D:
            cmp cx,11ch
            jb next_click
            cmp cx,0168h
            ja next_click
            
            mov d_flag,1

         
     ;----wich row-------------
     see_1: 
            cmp dx,2eh
            jb see_2
            cmp dx,3ah
            ja see_2 
            
            mov 1_flag,1
            jmp choose_op
                
     see_2: 
            cmp dx,3dh
            jb see_3
            cmp dx,49h
            ja see_3 
            
            mov 2_flag,1
            jmp choose_op                
     
     see_3: 
            cmp dx,4bh
            jb see_4
            cmp dx,5ah
            ja see_4 
            
            mov 3_flag,1
            jmp choose_op
            
     see_4: 
            cmp dx,5bh
            jb none
            cmp dx,68h
            ja none
            
            mov 4_flag,1
            jmp choose_op     
                       
     none: jmp next_click    ;if the click isn't in those coordinates above           
;----------------------------------                             
;----------------------------------      
;here we put the new value in the matrix and print the updated matrix
                           
     choose_op:call choseOption  ;bx= matrix element selected
     
     push cx              ; rebember that scan num destroys cx
     push dx              ; setPosMatrix destroys dx, so we save it b4
     
     call setPosMatrix    ;getting the coordinates to write  
     call setCursorPosition 
     
     call makeBlanks      ;using this function to clear up de cell we will write in               
                          ;this function push and pop dx, so we dont lose it, 
                          ;since we'll need it to set the right position in the next step  
                           
     call clearKeyBuffer
     call setCursorPosition 
     call scan_num        ;output=cx
     
     mov matrix [bx], cl  ;moving new val into the matrix
     
     call setCursorPosition
     call makeBlanks      ;explained above             
     
     mov ax,0
     mov al,matrix[bx]
     cbw
     
     call setCursorPosition
     call print_num   ;writing;input=ax
     
     pop dx               ;
     pop cx               ;gettin it back              
;---------------------------------- 
    call res_update                         
;----------------------------------     
    jmp next_click

end_es:          
    pop dx
    pop cx
    pop bx
    pop ax             
ret
edit_spreadsheet endp 


;****************************************************
; ChoseOption
;            Analise flags and decide wich matrix position should be modified 
;      
; Input:  Na  
;
; Output: BX= matrix position we want to change , 0 <= BX =< 15
;
; Destroys:BX
;
;****************************************************
choseOption proc
     
     cmp A_flag,1
     jne co_b
     ;looking for the right num flag
            cmp 1_flag,1
            jne co_a2
            
            mov bx,0
            jmp end_co
     co_A2:
            cmp 2_flag,1
            jne co_a3
            
            mov bx,4
            jmp end_co
            
     co_A3:
            cmp 3_flag,1
            jne co_a4
            
            mov bx,8
            jmp end_co
     co_A4:
            mov bx,12
            jmp end_co
     
     ;----------------------------
     ;---------------------------- 
     
     co_b:
        cmp B_flag,1
        jne co_c
        
     ;looking for the right num flag
            cmp 1_flag,1
            jne co_b2
            
            mov bx,1
            jmp end_co
     
     co_B2:
            cmp 2_flag,1
            jne co_b3
            
            mov bx,5
            jmp end_co
     
     co_B3:
            cmp 3_flag,1
            jne co_b4
            
            mov bx,9
            jmp end_co
     
     co_B4:
            mov bx,13
            jmp end_co
     
     ;----------------------------         
     ;----------------------------   
     
     co_c:
        cmp C_flag,1
        jne co_d
     ;looking for the right num flag
            cmp 1_flag,1
            jne co_c2
            
            mov bx,2
            jmp end_co
     
     co_c2:
            cmp 2_flag,1
            jne co_c3
            
            mov bx,6
            jmp end_co
     
     co_c3:
            cmp 3_flag,1
            jne co_c4
            
            mov bx,10
            jmp end_co
     
     co_c4:
            mov bx,14 
            jmp end_co
     
     ;---------------------------- 
     ;----------------------------    
     
     co_d:
        cmp D_flag,1
        jne end_co
             ;looking for the right num flag
            cmp 1_flag,1
            jne co_d2
            
            mov bx,3
            jmp end_co
     
     co_d2:
            cmp 1_flag,1
            jne co_d3
            
            mov bx,7
            jmp end_co
     
     co_d3:
            cmp 1_flag,1
            jne co_d4
            
            mov bx,11
            jmp end_co
     
     co_d4:
            mov bx,15
     
     ;----------------------------
     ;---------------------------- 
             
end_co: 

     
ret
choseOption endp


;****************************************************
; MakeBlanks
;           We use it to "clear" each cell after we get a new val 
;      
; Input:  Na  
;
; Output: Nop
;
; Destroys:Nuthing 
;
;****************************************************
MakeBlanks proc
        push ax
        push cx
        push dx
        
        mov cx,0
        
        mb_loop:
            cmp cx,4
            je end_mb
            
            mov ah,2
            mov dl,20h
            int 21h
            
            inc cx
            jmp mb_loop
       
end_mb:
        pop dx
        pop cx
        pop ax       
       
ret
makeBlanks endp 

;****************************************************
; Set Position Matrix
;                   sets de mouse in the position we want to change 
;      
; Input:  FLAGS letters/nums 
;
; Output: DX= position to write
;
; Destroys:DX 
;
;****************************************************
SetPosMatrix proc 
            cmp a_flag,1
            jne spm_b
    
            mov dl,4
            jmp spm_1
    
    spm_b:  
            cmp b_flag,1
            jne spm_c
    
            mov dl,9
            jmp spm_1
    
    spm_c:  
            cmp c_flag,1
            jne spm_d
    
            mov dl,14
            jmp spm_1            
                        
    spm_d:  
            mov dl,19
            
    spm_1:        
            cmp 1_flag,1
            jne spm_2
            
            mov dh,6
            jmp end_spm
            
    spm_2:        
            cmp 2_flag,1
            jne spm_3
            
            mov dh,8
            jmp end_spm
            
    spm_3:        
            cmp 3_flag,1
            jne spm_4
            
            mov dh,10
            jmp end_spm
            
    spm_4:        
            mov dh,12                                   


end_spm:   
ret
setPosMatrix endp

;****************************************************
; Formula Reader
;                reads the user input for formula and do the operation
;                result is saved in result_var 
;      
; Input:  FLAGS letters/nums 
;
; Output: DX= position to write
;
; Destroys:DX 
;
;****************************************************
form_read proc
   push ax
   push bx
   push cx
   push dx
   
    call reset_flags  ;signal flags not included
    
    mov plus_flag,0
    mov minus_flag,0
    mov div_flag,0
    mov mul_flag,0 
   
   errin:
   call clearKeyBuffer
   
   mov ax,0
   mov cx,0
   mov dh,14
   mov dl,13
   call setCursorPosition
   call makeBlanks
   call makeBlanks
   call setCursorPosition

   
   readin:
          cmp cx,5 
          je do_operation
          
          mov ah,1  ; w8 for input
          int 21h
          
          cmp cx,0
          je letters_fr
          
          cmp cx,3 
          je letters_fr
          
          cmp cx,1
          je nums_fr
          
          cmp cx,4
          je nums_fr
          
          cmp cx,2
          je operation_sig
          
          
   letters_fr:
             cmp al,0dh
             je dont_want_formula
   
              cmp al,'a'
              jne fr_b
              mov a_flag,1
              inc cx 
              push ax  ; we do this so we can get the val latter do write it in the form_buff
              jmp readin
              
              fr_b: 
              cmp al,'b'
              jne fr_c
              mov b_flag,1
              inc cx 
              push ax ; ""
              jmp readin
              
              fr_c:
              cmp al,'c'
              jne fr_d
              mov c_flag,1
              inc cx 
              push ax ; ""
              jmp readin
              
              fr_d:
              cmp al,'d'
              jne errin
              mov d_flag,1
              inc cx
              push ax
              jmp readin        
   
   nums_fr:
              cmp al,'1'
              jne fr_2
              mov 1_flag,1
              inc cx  
              push ax
              jmp readin
              
              fr_2: 
              cmp al,'2'
              jne fr_3
              inc cx 
              push ax
              mov 2_flag,1
              jmp readin
              
              fr_3:
              cmp al,'3'
              jne fr_4
              mov 3_flag,1
              inc cx 
              push ax
              jmp readin
              
              fr_4:
              cmp al,'4'
              jne errin
              mov 4_flag,1
              inc cx
              push ax
              jmp readin              
             
   operation_sig:   
            call choseOption ; we call this function to get the 1st operand
            push ax 
            mov ax,0
            mov al,matrix[bx]
            cbw
            mov num1,ax
            pop ax
             
            ;clearing flags--- 
            call reset_flags
            ;-----------------    

            cmp al, '+'
            jne op1
            mov plus_flag,1
            inc cx 
            push ax
            jmp readin
            
            op1:
            cmp al, '-'
            jne op2
            mov minus_flag,1
            inc cx
            push ax
            jmp readin
            
            op2:
            cmp al, '*'
            jne op3
            mov mul_flag,1
            inc cx 
            push ax
            jmp readin
            
            op3:
            cmp al, '/'
            jne errin
            mov div_flag,1
            inc cx
            push ax
            jmp readin
            
             
   do_operation:
            call choseOption ; we call this function to get the 2nd operand
            push ax 
            mov ax,0
            mov al,matrix[bx]
            cbw
            mov num2,ax
            pop ax
            
            call calculator
            mov dh,16
            mov dl,30
            call setCursorPosition
            call makeBlanks
            call makeBlanks
            call setCursorPosition

            mov ax,result_var
            call print_num 
            
            ;SAVING FORMULA ON FORM_BUFF
            pop ax
            mov form_buff[4], al
            
            pop ax
            mov form_buff[3], al
            
            pop ax
            mov form_buff[2], al
            
            pop ax
            mov form_buff[1], al
            
            pop ax
            mov form_buff[0], al
            ;---------------------------
     
     dont_want_formula:
           mov di, offset form_buff
           mov bx,5
           call clearStr 
    
     pop dx
     pop cx
     pop bx
     pop ax

ret
form_read endp

;****************************************************
; Result Update
;      
; Input: operation FLAGS ; num1, num2 
;
; Output: 
;
; Destroys:Nutin  
;
;**************************************************** 
res_update proc
    push ax
    push bx
    push cx
    push dx
    
    cmp form_buff[0], 0
    je end_ru
    
    call reset_flags ;signal flags not included
    
    mov plus_flag,0
    mov minus_flag,0
    mov div_flag,0
    mov mul_flag,0 
    
    mov cx,0 ; used as counter here
    mov bx,0
    readin_ru:
            cmp cx,5
            je go_to_op
            
            cmp cx,2
            je oper_sig
            
            
   letters_ru:  

                
               mov al,form_buff[bx]
               call set_flags
               inc cx
               inc bx
               jmp readin_ru
              
   oper_sig:
            push bx         
            call choseOption ; we call this function to get the 1st operand
            push ax 
            mov ax,0
            mov al,matrix[bx]
            cbw
            mov num1,ax
            pop ax
            pop bx
             
            ;clearing flags--- 
            call reset_flags
            ;-----------------    

            cmp form_buff[bx], '+'
            jne ru_op1
            mov plus_flag,1
            inc cx 
            inc bx
            jmp readin_ru
            
            ru_op1:
            cmp form_buff[bx], '-'
            jne ru_op2
            mov minus_flag,1
            inc cx
            inc bx
            jmp readin_ru
            
            ru_op2:
            cmp form_buff[bx], '*'
            jne ru_op3
            mov mul_flag,1
            inc cx
            inc bx
            jmp readin_ru
            
            ru_op3:
            cmp form_buff[bx], '/'
            jne errin
            mov div_flag,1
            inc cx 
            inc bx
            jmp readin_ru
            
    go_to_op: 
           
            call choseOption
            push ax
            mov ax,0 
            
            cmp matrix[bx],128  ;check if signed
            jb not_signed
            
            mov al,matrix[bx] 
            cbw
            
            jmp goto_calculator
            
     not_signed:       
            mov al,matrix[bx]
       
     goto_calculator:
            mov num2,ax
            
            pop ax
           
            call calculator
            mov dh,16
            mov dl,30
            call setCursorPosition
            call makeBlanks
            call makeBlanks
            call setCursorPosition

            mov ax,result_var
            call print_num
             
end_ru:             
     pop dx
     pop cx
     pop bx
     pop ax         
ret
res_update endp

;****************************************************
; Calculator
;      
; Input: num1 ; num2
;        operation FLAGS ; num1, num2 
;
; Output: result_var
;
; Destroys:Nutin  
;
;****************************************************
calculator proc
            
            push dx
            push cx
            push bx
            push ax
           
            cmp plus_flag, 1
            je do_plus
            
            cmp minus_flag,1
            je do_minus
            
            cmp mul_flag,1
            je do_mult
            
            cmp div_flag,1
            je do_div
            
            do_plus:


        mov ax, num1
        add ax, num2
        ;result in ax
        
        jmp last
        
        
        
        do_minus:
        
        mov ax, num1
        sub ax, num2
        ; result in ax.
        
        jmp last
        
        
        
        
        do_mult:
        
        mov ax, num1
        imul num2 ; (dx ax) = ax * num2. 
        ;result in ax.
        ; dx is ignored (calc works with tiny numbers only).
        
        jmp last
        
        
        
        
        do_div:
        ; dx is ignored (calc works with tiny integer numbers only).
        mov dx, 0
        mov ax, num1
        idiv num2  ; ax = (dx ax) / num2.

        last:
        
            mov result_var,ax

        pop ax
        pop bx
        pop cx
        pop dx
        
ret
calculator endp 

;****************************************************
; Exit Function 
;               On the Exit option these memory contents
;               are written onto a file in the program
;               executing directory. If the file exists already, it will be overwritten
;      
; Input: Nop 
;
; Output: Nietz
;
; Trump Destroys:Nutin  
;
;****************************************************
exit_func proc
    push ax
    push dx
    push cx
    
    mov al,1 ;al=1 write
    mov dx,offset path0
    call Fopen
        
    mov bx,ax  ;moving handler
    mov cx,21  ;bytes 2 copy
    mov dx, offset matrix
    call Fwrite 
     
    call Fclose
    
    pop cx
    pop dx
    pop ax
ret
exit_func endp 

;****************************************************
; Set Flags 
;          Set letters and num (this function is case sensitive)
;          
; Input: AL what we want to analise
;
; Output: set flag
;
;         if no flag, AX=-1
;         if flag seted, AX=1
;           
; Trump Destroys:AX
;
;****************************************************                     
set_flags proc
    
;NUMBERS------------------    
    cmp al,'a'
    je a 
    cmp al,'A'
    jne goto_b
    a:
    mov a_flag,1
    jmp end_setFlags
    
    goto_b:
    cmp al,'b'
    je b
    cmp al,'B'
    jne goto_c 
    b:
    mov b_flag,1
    jmp end_setFlags
    
    goto_c:
    cmp al,'c'
    je c
    cmp al,'C'
    jne goto_d
    c:
    mov c_flag,1
    jmp end_setFlags    
    
    goto_d:
    cmp al,'d'
    je d
    cmp al,'D'
    jne goto_1
    d:
    mov d_flag,1
    jmp end_setFlags    

;LETTERS-------------------    
    goto_1:
    cmp al,'1'
    jne goto_2
    
    mov 1_flag,1
    jmp end_setFlags    
    
    goto_2:
    cmp al,'2'
    jne goto_3
    
    mov 2_flag,1
    jmp end_setFlags    
    
    goto_3:
    cmp al,'3'
    jne goto_4
    
    mov 3_flag,1
    jmp end_setFlags
    
       
    goto_4:
    cmp al,'4'
    jne no_flags
    mov 4_flag,1
    
    

end_setFlags: mov ax,1
              jmp sf_end
              
no_flags: mov ax,0FFFFFFFFh
sf_end:    
ret
set_flags endp 

;****************************************************
; reset_flags 
;               just reset the flags in a dumb way, but kinda usefull
;          
; Input: naaap
;
; Output: don't think so
;           
; Trump Destroys:No way
;
;****************************************************
reset_flags proc
    
    mov a_flag,0
    mov b_flag,0
    mov c_flag,0
    mov d_flag,0 ;i know this looks prety primitive but... 
    mov 1_flag,0 ;go complain with AM3 professor!
    mov 2_flag,0
    mov 3_flag,0
    mov 4_flag,0 
    
ret
reset_flags endp    

;****************************************************
; Export Spreadsheet 
;                       export to a text file the spreadsheet values and formula 
;                       
;          
; Input: MATRIX
;
; Output:  Save the positions and respective values into the export_buffer and then in a txt file
;
; Trump Destroys:Na 
;
;****************************************************
export_spread proc
    
     push bx
     push cx
     push di
     push si
     push dx
     
        mov dx,offset loading
        mov ah,9
        int 21h

;CLEARING BUFFERS--------------
 
     lea di,file_name
     add di,19
     mov bx,50
     call clearStr
     
     lea di,export_buffer
     mov bx,121
     call clearStr


;------------------------------
     call clearScreen
     mov dx, offset exprt_note 
     mov ah,9
     int 21h
     
     call enter      
     
     
;GETTING THE FILE NAME---------------------------
     mov dx, offset puts_fname
     mov ah,9
     int 21h 
     
     lea di,file_name
     add di, 19 ; pointing to empty position
     
     
     call read_keybord 
     
     
     ;LAST 4 DIGITS MUST BE .TXT , IF NOT ERROR                  
     dec di
     
     cmp [di],'t'
     jne un_cret
     dec di
     
     cmp [di],'x'
     jne un_cret
     dec di
     
     cmp [di],'t'
     jne un_cret
     dec di
     
     cmp [di],'.'
     jne un_cret
     
     

;-------------------------------------------------     
     
;GETTING THE INFO INTO THE BUFFER------------------ 
     
     inf_2_buf:
     
     mov bx,0
     lea si, a1
     lea di, export_buffer
     
     check_position:
     
     cmp bx,16 ;see if u alredy check every position
     je exprt_form
     
     cmp matrix[bx],0h
     jne cop_2_buf
     
     inc bx
     add si,3
     jmp check_position
     
     cop_2_buf:
                mov cx,3  ; we want to move the cell to the buffer 
                rep movsb ; cell is composed by 3 chars
                
                mov al, matrix[bx]
                cbw   ;num at ax now
                
                call int2ascii
                mov [di],';' ; putc ';'
                
                inc bx ;lez check the next position
                inc di ;pointing it to the buffer empty position 
                ;add si,3 ;next ceel strg
                
                jmp check_position
                
                
    
    
    exprt_form: 
                cmp form_buff[0],0
                je no_form
    
                lea si,for
                mov cx,4
                rep movsb ;copying "for:" to the buffer
                          ;if the form buffer is empty, we copy just 'spa' ... like no prob
                          
                lea si,form_buff ; now we want to copy the formula, so lets put it pointed by si
                
                mov cx,5
                rep movsb ;actually copying
                          ;formula is the last thing 2 copy, after that we can write it 2 file


;----------------------------------------------------
     no_form:
     
     mov cx,0
     mov dx, offset file_name
     call Fcreate
     
     jnc all_good ;if cf=1 error
     un_cret:
     mov ah,9
     mov dx,offset un_create
     int 21h
     
     mov ah,1
     int 21h
     
     jmp export_spread_end
     
     all_good:  
     mov cx,121 ;size of export_buffer 
     mov bx,ax
     mov dx, offset export_buffer
     call Fwrite
    
     call Fclose
     
     mov ah,9
     mov dx, offset sucess_exp
     int 21h
     
     mov ah,1
     int 21h
     
export_spread_end:
     pop dx                     
     pop si
     pop di
     pop cx
     pop bx                                        

ret
export_spread endp


;****************************************************
; Read Keybord 
;               Reads string from keybord
;                       
;          
; Input: DI= offset string we want 2 write in
;
; Output: Nop
;
; Trump Destroys: DI 
;
;****************************************************
read_keybord proc
       push ax
       
     call clearKeyBuffer  
       
     cic_rk:  
       mov ah, 1
       int 21h
              
       cmp al, 0Dh
       je fim_rk
       
       mov [di] , al
       
       inc di 
       jmp cic_rk


       
fim_rk:

pop ax
ret
read_keybord endp

;****************************************************
; INT 2 ASCII
;             converts a num 2 ascii and saves at string
;
; Input: AX=int to convert
;        DI=offset str
;
; Output: num at str
;
; Destroys:Nuthin
;
;****************************************************
int2ascii_uns proc
    push ax
    push bx
    push cx
    push dx
    
    cmp ax,0
    je if_0_i2a
    
    mov cx,10;"divider"
    mov bx,0 ;counter
    
   div_maker_i2a:
             mov dx,0
             div cx   ; ax= ax/10 , dx=remainder (resto)
             push dx  ; saving ech digit to the stack
             inc bl   ; give us the number of divisions we made
             
             cmp ax,0
             jne div_maker_i2a
             
   prt_num_i2a:       
             pop ax ;gettin it back from the stack
             add al,30h ; int --> char
             
             mov [di],al
             
             dec bl
              
             inc di
             cmp bl,0
             jne prt_num_i2a 
             jmp end_pnu_i2a
             
   if_0_i2a:
             mov dl,30h
             mov ah,2
             int 21h          

end_pnu_i2a:
   pop dx
   pop cx
   pop bx
   pop ax             
ret             
int2ascii_uns endp 

;****************************************************
; INT 2 ASCII
;             converts a num 2 ascii and saves at string
;
; Input: AX=int to convert
;        DI=offset str
;
; Output: num at str
;
; Destroys:Nuthin
;
;****************************************************
int2ascii       PROC
        PUSH    DX
        PUSH    AX
       
        
        
        ; the check SIGN of AX,
        ; make absolute if it's negative:
        CMP     AX, 0
        JNS     positive_ia
        NEG     AX
         
        PUSH AX 
        ;PUTC    '-'
        
        mov [di],'-'
        inc di
        
        POP AX
        
positive_ia:
        CALL    int2ascii_uns
printed_ia:
        POP     AX
        POP     DX

        RET
int2ascii       ENDP

;****************************************************
; Clear Keyboord buffer
;             
;
; Input: Nop
;
; Output: None
;
; Destroys:Nuthin
;
;****************************************************
clearKeyBuffer proc
    push ax    
    mov ah,0ch
    mov al,0
    int 21h
    pop ax
ret
clearKeyBuffer endp 

;****************************************************
; Ascii 2 INT
;
; Input:  str at di
;
; Output: CX = signed number
;         if any error Cx=-1
;         DX = number of digits 
;
; Destroys:CX,DX  
;
;****************************************************
ascii2int proc 
    
    push ax
    push bx
    
   
     mov cx,0 
     mov bx,0
     mov minus_sign, 0 
     mov dx,0  ; used as digit counter
     
     next_digit_a2i:  
                cmp cx,128
                ja err_sn_a2i 

                cmp cx,128
                je  err_sn_a2i
                
                next_a2i:
                mov al,[di]
                
                cmp al, '-' ; checking if it is a negative number
                je set_minus_a2i 
    
                              
                cmp al,';'    ; when ';' we stop reading
                je look_minus_a2i
                
                cmp al,30h
                jae ok_ae_a2i ;cheking if its a number  '0'<=num=<'9'
   
    err_sn_a2i: cmp cx,128
                jne dont_check_minus_a2i
    
                cmp minus_sign,1
                je next
                
                dont_check_minus_a2i:    
                mov cx, 0FFFFFFFFh     
                jmp end_sn_a2i
                
    ok_ae_a2i:
                cmp al,39h
                ja err_sn_a2i
                
                push ax ; saving our ax
                mov ax,cx
                mul const_sn ;const_sn=10
                mov cx,ax
                pop ax
                
                sub al,30h ; conver to integer
                
                mov ah,0
                add cx,ax
                jc err_sn_a2i
                inc di ;next position in the buffer
                inc dx
                jmp next_digit_a2i           
    
    
    set_minus_a2i:
                inc di
                inc dx
                mov minus_sign,1
                jmp next_digit_a2i
                
    look_minus_a2i:
                cmp minus_sign,0
                je end_sn_a2i            
                
                neg cx


    end_sn_a2i: 
    
                pop bx
                pop ax

ret
ascii2int endp 

;****************************************************
; Clear Str
;
; Input:  str at di
;         BX=number of bytes to clear
;
; Output:  
;
; Destroys:NUP 
;
;****************************************************
clearStr proc
      push cx
      
      mov cx,0
      
      next_element:
      
      cmp cx,bx
      je end_of_str
      
      mov [di],0h
      
      inc di
      inc cx
      
      jmp next_element
      
      end_of_str:
      pop cx
ret    
clearStr endp

;****************************************************
; Import Spreadsheet
;                       Loads form a text file the spreadsheet values
;
; Input:   file.txt
;
; Output:   matrix and formula with imported values
;           if impossible to import we will call back contents.bin
;
; Destroys:Nuthin  
;
;****************************************************
import_spread proc
        push di
        push ax
        push bx
        push cx
        push dx 
        
        mov dx,offset loading
        mov ah,9
        int 21h
             
;CLEARING BUFFERS--------------
 
     lea di,file_name
     add di,19
     mov bx,50
     call clearStr
     
     lea di,export_buffer
     mov bx,121
     call clearStr
     
     lea di,matrix
     mov bx,16
     call clearStr
     
     lea di,form_buff
     mov bx,5
     call clearStr


;JUST PRINTING NOTE OF FILE NAME EXAMPLE
     call ClearScreen
     mov dx, offset exprt_note  ;"example.txt"
     mov ah,9
     int 21h
     
     call enter      
     
     
;GETTING THE FILE NAME---------------------------
     mov dx, offset puts_fname
     mov ah,9
     int 21h 
     
     lea di,file_name
     add di, 19 ; pointing to empty position
     
     
     call read_keybord 
     
     
     ;LAST 4 DIGITS MUST BE .TXT , IF NOT ERROR                  
     dec di
     
     cmp [di],'t'
     jne w_name
     dec di
     
     cmp [di],'x'
     jne w_name
     dec di
     
     cmp [di],'t'
     jne w_name
     dec di
     
     cmp [di],'.'
     jne w_name

;OPENING AND READING THE FILE ------------------------------------- 

     mov al,0 ;al=0 reading
     mov dx,offset file_name
     call Fopen
     jc un_imp
     
     mov bx,ax ;moving handler
     mov dx,offset export_buffer
     mov cx,121 ;bytes to read
     call Fread
     call Fclose
     
;CHECKING IF IT'S EVERYTHING OK AND MOVING VALUES INTO THE MATRIX AND FORMULA
    
    mov bx,0
    
    se_f:
    cmp export_buffer[bx],'F'
    je got_f
    
    mov al,export_buffer[bx]
    call set_flags
    
    cmp export_buffer[bx],0h ;we dont save '0', so if we find a '0' arround here
    je  end_if_not_shit      ;thats because there is no formula! 
                             ;if there is something different from '0' it's wrong!
                             
    ;if there is something diff from '0' no flag detected so
    ;in the next instruction it will jump off and get the error
    
    cmp ax,0FFFFFFFFh ;seeing if there is no flag seted
    je un_imp
    
    mov ax,0 ;getting the 'x' off
    inc bx
    mov al,export_buffer[bx]
    
    call set_flags
    
    cmp ax,0FFFFFFFFh ;seeing if there is no flag seted
    je un_imp    
    
    inc bx ;right here we're pointing to ':'
    
    push bx ;we wont lose itttt
    
    call choseOption ;output of this one is bx=position we wanna change
     
    mov dx,bx ;saving our value from chose option in dx, since we need bx
    pop bx ;gettin it back
    
    inc bx ; now pointing to the 1st digit (number)
    lea di,export_buffer
    add di,bx
    
    push dx ;we don't wanna lose this value! ascii2int destrys it!
    
    call ascii2int ;return cx=int ; DX= number of digits
    
    add bx,dx ;adding the number of digits
    
    pop dx ;gettin it back, here we have value from choose option (position to change in the matrix)          
    
    push bx ;saving bx
    
    mov bx,dx  ;dx? here it is ready to change the right position in the matrix
    mov matrix[bx],cl  ;so we put that value in bx to do this lines' operation
    
    pop bx ;gettin it back
    inc bx ;we are pointing to ';' b4 this one
    
    ;so let's reset the flags and check it all againn
    call reset_flags
    jmp se_f
    
    got_f: 
    inc bx;pointing to the 'o' of for
    cmp export_buffer[bx],'O'
    jne un_imp
    
    inc bx;pointing to the 'r' of for
    cmp export_buffer[bx],'R'
    jne un_imp
    
    inc bx;pointing to the ':' of for
    cmp export_buffer[bx],':'
    jne un_imp
              
    inc bx;pointing 1st digit of number
    cld
    lea si,export_buffer
    add si,bx ;going to curretn position
    lea di,form_buff
    mov cx,5  ;size of formula
    rep movsb
    
;CHECK IF FORMULA OK-----------------------------
    mov bx,0
    call reset_flags
    mov plus_flag,0
    mov minus_flag,0
    mov mul_flag,0
    mov div_flag,0
    
    check_form:
           
            mov al,form_buff[bx] 
            cmp bx,2
            je cf_2
            
            cmp bx,5
            je  finally
     
;if cx=0,3 form[cx]=a,b,c,d          
;if cx=1,4 form[cx]=1,2,3,4
;if cx=2 form[cx]=-,*,/,+ 

            call set_flags ;this function returns ax=1 if flags seted or ax=-1 if flags not seted
            cmp ax,0FFFFFFFFh
            je  un_imp
            
            inc bx
            jmp check_form
            
     cf_2:
            cmp al,'+'
            je ok_ns
            
cf_minus:   cmp al,'-'
            je ok_ns
            
cf_mul:     cmp al,'*'
            je ok_ns
            
cf_div:     cmp al,'/'
            jne un_imp 
            
ok_ns:      inc bx
            jmp check_form
            
            
            finally:
            ;if you got here the file is completely ok!
            ;let's move on!
           
            jmp end_if_not_shit
    
    
;IF WRONG NAME------------------------------------     
     w_name:
            call ClearScreen
            mov dx, offset wrong_name ;not a nice file name
            mov ah,9                  ;maybe u forgot .txt
            int 21h
            
            jmp end_if_shit

;IF FILE DOESN'T EXIST            
     un_imp: 
            call ClearScreen
            mov dx,offset unable_imp   ;shit happened if you get here
            mov ah,9                   ;maybe file doesn't exist
            int 21h
            
            
end_if_shit: 
            
;if shit happens we want to restore the values the spreadsheet had b4 
;to do that we will open and read the contents.bin
                
            mov al,0 ;al=0 read
            mov dx,offset path0
            call Fopen
            
            mov bx,ax ;moving handler
            mov cx,21 ;bytes to read
            mov dx,offset matrix
            call Fread 
            
            call Fclose
;---------------------------------------------
             
            mov ah,1  ;just givin time so the user can see the message
            int 21h
            jmp just_the_end
            
end_if_not_shit:
            call ClearScreen
            mov dx,offset sucess_imp
            mov ah,9
            int 21h 
            
            mov ah,1
            int 21h
            
just_the_end:           
            pop dx
            pop cx
            pop bx
            pop ax
            pop di


ret
import_spread endp


         
ends
end start ; set entry point and stop the assembler.
