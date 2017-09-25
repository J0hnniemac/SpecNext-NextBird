
;Author : John McManus - john@appyappster.com 
;Resources: Sprites scraped from internet and used UDGeedNext, 
;           http://chuntey.arjunnair.in, code snips from Jonathan Cauldwell 

;0.1 - JMcManus - 25/09/2017 intial release with sprites, and L2 background scrolling Top Pipes only
;0.2 - Added bottom pipes
 
  org 32768

  birdaniIndex db 2;
  bird_x     db     32 ; x 
  bird_y     db     99 ; y 
  



  ;birdaniSeqquence db 0,1,2,3,4,5,4,3,2,1,0;
  birdaniSeqquence db 0,1,0,1,0,1,0,1,0,1,0,1;
  pretim defb 0
  column1_x  defb    48
  column1_y  defb     0

  column1e_x  defb    48
  column1e_y defb     16
  
; StartAddress:
START 

  
  call setupScreen
  ld hl, Sprite1
  ld a, 0
  call loadspritedata
  ld hl, Sprite2
  ld a, 1
  call loadspritedata
  



  ld bc, $243b
  ld a, 21
  out (c), a

  ;OUT 0x253B, 1; REM All sprites visible
  ld bc, $253b
  ld a, 1
  out (c), a


  ;nextreg 21,1
  ;nextreg 21,1
;ld a, 1
  
 call setupbackground
 ;call testcol1
 ;call testcol2
 ;call drawColumnT
 ;topColumns      db 48,80,112,144,176,208,208;
  ld a, 3
  ld  (colLenth), a
  
  ld hl, topColumns
  ld (hl), 48
  

  call pc1
 
  ld a, 2
  ld  (colLenth), a
  
  ld hl, topColumns
  ld (hl), 96
  call pc1

  ld hl, topColumns
  ld (hl), 144
  call pc1

  ld a, 4
  ld  (colLenth), a
  

  ld hl, topColumns
  ld (hl), 192
  call pc1
  ld a, 1
  ld  (colLenth), a
  ld hl, topColumns
  ld (hl), 239
  call pc1







;---------------
 ld a, 2
  ld  (colLenth), a
  
  ld hl, topColumns
  ld (hl), 48
  

  call bc1
 
  ld a, 4
  ld  (colLenth), a
  
  ld hl, topColumns
  ld (hl), 96
  call bc1

  ld hl, topColumns
  ld (hl), 144
  call bc1

  ld a, 3
  ld  (colLenth), a
  

  ld hl, topColumns
  ld (hl), 192
  call bc1
  ld a, 4
  ld  (colLenth), a
  ld hl, topColumns
  ld (hl), 239
  call bc1 
 ;call drawcl1
 call displaymap


  
mainloop:
;  call animatebird
 call wait
 ; call movebird

jr mainloop


wait   ld hl,pretim        ; previous time setting
       ld a,(23672)        ; current timer setting.
       sub (hl)            ; difference between the two.
       cp 6                ; have two frames elapsed yet?
       jr nc,wait0         ; yes, no more delay.









       ret
wait0  ld a,(23672)        ; current timer.
       ld (hl),a           ; store this setting.
       call animatebird
       call movebird
       call scrollL2
       ret






scrollL2:

  ld      bc, $243B    ; select the scroll register
  ld      a,22
  out     (c),a     ; select layer 2 "X" scroll

  ld  a,(ScrollIndex)
  inc a   
  inc a 
  ld  (ScrollIndex),a

  ld      bc, $253B
  out     (c),a   



  ret



ScrollIndex db 0














setupScreen:
  ld a, 56
  ld (23693),a
  call 3503

  ld a, 2
  call 8859
  ret







; in hl = sprite data source
;    a = sprite number 0-255 ?

loadspritedata:
  ld bc, $303b
  out (c), a ; select sprite 0

  ; Send pattern data
  ;ld hl, Sprite1
  ld de, 256
  ld bc, $5b
load_sprite_loop:
  outi  ;reads (hl) and sends to port (c) e.g load the sprite into sprite engine thingy
  inc b ; inc a a outi is decrementing it.

  dec de
  ld a,d
  or e
jr nz, load_sprite_loop


  
  ret 


ret

; in a = sprite to show


showsprite:
  push af
  ;ld a,9
  ld bc, $303b
  out (c), a ; select sprite 3
  ld bc, $57
  ld a, (bird_x) ; xpos >>> 32 on boarder ->>
  out (c), a
  ld a, (bird_y) ; ypos ? --- 32 upper ->>>
  out (c), a
  ld a, 0 ; 7-4 is palette offset, bit 3 is X mirror, bit 2 is Y mirror, bit 1 is rotate flag and bit 0 is X MSB. 
  ; 8 flip x 4 flipy 2 rotate 1 msb
  out (c), a
  pop af
  xor 128
  ;ld a, 137 ; spite visible pattern 0 ??? ;;128 = 0
  out (c), a

  
  ret 

hidesprite:
  push af
  ;ld a,9
  ld bc, $303b
  out (c), a ; select sprite 3
  ld bc, $57
  ld a, (bird_x) ; xpos >>> 32 on boarder ->>
  out (c), a
  ld a, (bird_y) ; ypos ? --- 32 upper ->>>
  out (c), a
  ld a, 0 ; 7-4 is palette offset, bit 3 is X mirror, bit 2 is Y mirror, bit 1 is rotate flag and bit 0 is X MSB. 
  ; 8 flip x 4 flipy 2 rotate 1 msb
  out (c), a
  pop af
  ;xor 128
  ;ld a, 137 ; spite visible pattern 0 ??? ;;128 = 0
  out (c), a

  ret





;sprite databases
;ld hl, bird_y
 ;  inc (hl)

animatebird:
  ;get birdaniIndex
  ld bc,0
  ld a, (birdaniIndex)
  ld c, a
  ld hl, birdaniSeqquence
  add hl,bc ; hl = sprite
  ld a,(hl)
  call hidesprite

  ;sprite is hidden, get next sprite


  ld hl, birdaniIndex
  dec (hl)
  
  jp NZ,indexok ; index > 1
  ld (hl), 2 ;> reset index

indexok:
  ;index is saved, lookup bird
  ld bc,0
  ld a, (birdaniIndex)
  ld c, a
  ld hl, birdaniSeqquence
  add hl,bc ; hl = sprite
  ld a,(hl)

  call showsprite
  halt

  ret





movebird:
  ld bc,63486         ; keyboard row 1-5/joystick port 2.
    in a,(c)            ; see what keys are pressed.
    rra                 ; outermost bit = key 1.
    push af             ; remember the value.
    call nc,mpl         ; it's being pressed, move left.
    pop af              ; restore accumulator.
    rra                 ; next bit along (value 2) = key 2.
    push af             ; remember the value.
    call nc,mpr         ; being pressed, so move right.
    pop af              ; restore accumulator.
    rra                 ; next bit (value 4) = key 3.
    push af             ; remember the value.
    call nc,mpd         ; being pressed, so move down.
    pop af              ; restore accumulator.
    rra                 ; next bit (value 8) reads key 4.
    call nc,mpu         ; it's being pressed, move up.
    ret

mpl:
  ld hl, bird_x
    dec (hl)
    ret
mpr:
  ld hl, bird_x
    inc (hl)
    ret
mpu:
  ld hl, bird_y
    dec (hl)
    ret
mpd:
  ld hl, bird_y
    inc (hl)
    ret



ret






setupbitmap:
    ld  bc, $123b
    out (c),a     ; set bank
    ld  hl,0
    ld  de,64*256 ; 16 k page
nextpixel:
    ;
    ld a,255
    ld (hl),a
    inc hl
    dec de
    ld a,d
    or e
    jp nz, nextpixel


  ret



setupbackground:
  ld a, 1;
  call setupbitmap
  ld a, 65;
  call setupbitmap
  ld a, 129;
  call setupbitmap


  ret





; 7columns
maxColumn       Equ 7
currColumn      db maxColumn
  
topColumns      db 48,80,112,114,176,208,208;
topColumnsLen   db  1, 2,  3,  4,  2,  3,  1;
colCount db  3

colLenth db 3
sColLength db 0  




pc1:  
    ld a,(colLenth)
    ld (sColLength), a ; save start value
    ld ix, Pipex
    dec ix ; start of pipe
    ;ld iy, topColumns ; x position

    ld de ,16; 16 across
    push de ; save de
;    ld  hl,0
    ld a, 0 ; y postion = 0
    ld h, a
    ld a, (topColumns) 
    ld l, a ; x position of column y = 0
 
;hl = start of column
    dec h
pc1i:
    inc h
    ld a, 1;
    ld  bc, $123b
    out (c),a     ; set bank
    ld  de,16 ; 16*16 sprite 
pc1nc:
    ;
    ld a,0 ; black
    ld a, (ix) ; copy pipe pixel
    dec ix ; work back the way 

    ld (hl),a ;place the pixel
    inc hl; next colum
    dec de; count down
    ld a,d ; zero check
    or e ; zero check
    jp nz, pc1nc ; next col
    ld a, l ; 
    sub  16  ;jump down a row
    ld l, a
    pop de
    dec de
    ld a,d
    push de
    or e
    jp nz, pc1i
    pop de

;ret

  
  ld a,(colLenth)
  dec a
  jp z , pcEnd
  ld (colLenth), a




pc2:  

    ld ix, Pipex
    dec ix ; start of pipe
;    ld iy, topColumns ; x position

    ld de ,16; 16 across
    push de ; save de
;    ld  hl,0
  ;  ld a, 0 ; y postion = 0
   ; ld a, l ; 
    ;sub  16  ;jump down a row
    ;ld l, a
    inc h

;hl = start of column
    dec h
pc2i:
    inc h
    ld a, 1;
    ld  bc, $123b
    out (c),a     ; set bank
    ld  de,16 ; 16*16 sprite 
pc2nc:
    ;
    ld a,0 ; black
    ld a, (ix) ; copy pipe pixel
    dec ix ; work back the way 

    ld (hl),a ;place the pixel
    inc hl; next colum
    dec de; count down
    ld a,d ; zero check
    or e ; zero check
    jp nz, pc2nc ; next col
    ld a, l ; 
    sub  16  ;jump down a row
    ld l, a
    pop de
    dec de
    ld a,d
    push de
    or e
    jp nz, pc2i
    pop de



  ld a,(colLenth)
  dec a
  jp z , pcEnd
  ld (colLenth), a

pc3:  

    ld ix, Pipex
    dec ix ; start of pipe
;    ld iy, topColumns ; x position

    ld de ,16; 16 across
    push de ; save de
;    ld  hl,0
  ;  ld a, 0 ; y postion = 0
   ; ld a, l ; 
    ;sub  16  ;jump down a row
    ;ld l, a
    inc h

;hl = start of column
    dec h
pc3i:
    inc h
    ld a, 1;
    ld  bc, $123b
    out (c),a     ; set bank
    ld  de,16 ; 16*16 sprite 
pc3nc:
    ;
    ld a,0 ; black
    ld a, (ix) ; copy pipe pixel
    dec ix ; work back the way 

    ld (hl),a ;place the pixel
    inc hl; next colum
    dec de; count down
    ld a,d ; zero check
    or e ; zero check
    jp nz, pc3nc ; next col
    ld a, l ; 
    sub  16  ;jump down a row
    ld l, a
    pop de
    dec de
    ld a,d
    push de
    or e
    jp nz, pc3i
    pop de












pcEnd:  

    ld ix, Pipe
    dec ix ; start of pipe
 ;   ld iy, topColumns ; x position

    ld de ,16; 16 across
    push de ; save de
;    ld  hl,0
  ;  ld a, 0 ; y postion = 0
   ; ld a, l ; 
    ;sub  16  ;jump down a row
    ;ld l, a
    inc h

;hl = start of column
    dec h
pcEndi:
    inc h
    ld a, 1;
    ld  bc, $123b
    out (c),a     ; set bank
    ld  de,16 ; 16*16 sprite 
pcEndnc:
    ;
    ld a,0 ; black
    ld a, (ix) ; copy pipe pixel
    dec ix ; work back the way 

    ld (hl),a ;place the pixel
    inc hl; next colum
    dec de; count down
    ld a,d ; zero check
    or e ; zero check
    jp nz, pcEndnc ; next col
    ld a, l ; 
    sub  16  ;jump down a row
    ld l, a
    pop de
    dec de
    ld a,d
    push de
    or e
    jp nz, pcEndi
    pop de


ret

;---------bottom bottomsColumns
bc1:  

    ld ix, Pipex
    dec ix ; start of pipe
    ;ld iy, topColumns ; x position

    ld de ,16; 16 across
    push de ; save de
;    ld  hl,0
    ld a, 63 ; y postion = 0
    ld h, a
    ld a, (topColumns) 
    ld l, a ; x position of column y = 0
 
;hl = start of column
    inc h
bc1i:
    dec h
    ld a, 129;
    ld  bc, $123b
    out (c),a     ; set bank
    ld  de,16 ; 16*16 sprite 
bc1nc:
    ;
    ld a,0 ; black
    ld a, (ix) ; copy pipe pixel
    dec ix ; work back the way 

    ld (hl),a ;place the pixel
    inc hl; next colum
    dec de; count down
    ld a,d ; zero check
    or e ; zero check
    jp nz, bc1nc ; next col
    ld a, l ; 
    sub  16  ;jump down a row
    ld l, a
    pop de
    dec de
    ld a,d
    push de
    or e
    jp nz, bc1i
    pop de

;ret

 ;ret 
  ld a,(colLenth)
  dec a
  jp z , bc4
  ld (colLenth), a




bc2:  

    ld ix, Pipex
    dec ix ; start of pipe
;    ld iy, topColumns ; x position

    ld de ,16; 16 across
    push de ; save de
;    ld  hl,0
  ;  ld a, 0 ; y postion = 0
   ; ld a, l ; 
    ;sub  16  ;jump down a row
    ;ld l, a
    ;inc h

;hl = start of column
    inc h
bc2i:
    dec h
    ld a, 129;
    ld  bc, $123b
    out (c),a     ; set bank
    ld  de,16 ; 16*16 sprite 
bc2nc:
    ;
    ld a,0 ; black
    ld a, (ix) ; copy pipe pixel
    dec ix ; work back the way 

    ld (hl),a ;place the pixel
    inc hl; next colum
    dec de; count down
    ld a,d ; zero check
    or e ; zero check
    jp nz, bc2nc ; next col
    ld a, l ; 
    sub  16  ;jump down a row
    ld l, a
    pop de
    dec de
    ld a,d
    push de
    or e
    jp nz, bc2i
    pop de


    ld a,(colLenth)
  dec a
  jp z , bc4
  ld (colLenth), a

  

bc3:  

    ld ix, Pipex
    dec ix ; start of pipe
;    ld iy, topColumns ; x position

    ld de ,16; 16 across
    push de ; save de
;    ld  hl,0
  ;  ld a, 0 ; y postion = 0
   ; ld a, l ; 
    ;sub  16  ;jump down a row
    ;ld l, a
    ;inc h

;hl = start of column
    inc h
bc3i:
    dec h
    ld a, 129;
    ld  bc, $123b
    out (c),a     ; set bank
    ld  de,16 ; 16*16 sprite 
bc3nc:
    ;
    ld a,0 ; black
    ld a, (ix) ; copy pipe pixel
    dec ix ; work back the way 

    ld (hl),a ;place the pixel
    inc hl; next colum
    dec de; count down
    ld a,d ; zero check
    or e ; zero check
    jp nz, bc3nc ; next col
    ld a, l ; 
    sub  16  ;jump down a row
    ld l, a
    pop de
    dec de
    ld a,d
    push de
    or e
    jp nz, bc3i
    pop de

bc4:  

    ld ix, Pipe
    dec ix ; start of pipe
 ;   ld iy, topColumns ; x position

    ld de ,16; 16 across
    push de ; save de
;    ld  hl,0
  ;  ld a, 0 ; y postion = 0
   ; ld a, l ; 
    ;sub  16  ;jump down a row
    ;ld l, a
    inc h

;hl = start of column
    inc h
bc4i:
    dec h
    ld a, 129;
    ld  bc, $123b
    out (c),a     ; set bank
    ld  de,16 ; 16*16 sprite 
bc4nc:
    ;
    ld a,0 ; black
    ld a, (ix) ; copy pipe pixel
    dec ix ; work back the way 

    ld (hl),a ;place the pixel
    inc hl; next colum
    dec de; count down
    ld a,d ; zero check
    or e ; zero check
    jp nz, bc4nc ; next col
    ld a, l ; 
    sub  16  ;jump down a row
    ld l, a
    pop de
    dec de
    ld a,d
    push de
    or e
    jp nz, bc4i
    pop de


ret









  

  ; x = 48, 80, 112, 144,176,208,240
  ;240,208,176,144,80,48
  ; y = 0 ; draw from top down
  ; length = (get a random number)





drawColumnB:
  ; x = 48, 80, 112, 144,176,208,240
  ;240,208,176,144,80,48
  ; y = 64 ; draw from top down ;; check 64
  ; length = (get a random number)





displaymap:

  ld  bc, $123b
    ld  a,2 
    out (c),a  ; make layer 2 visibile.

  ret






Sprite1:
  db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $E3, $49, $49, $49, $49, $49, $49, $E3, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $49, $49, $5B, $5B, $5B, $49, $FF, $FF, $49, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $49, $5B, $5B, $57, $57, $49, $FF, $FF, $FF, $FF, $49, $E3, $E3, $E3;
  db  $E3, $49, $5B, $57, $57, $57, $57, $49, $DA, $FF, $FF, $49, $FF, $49, $E3, $E3;
  db  $49, $57, $57, $57, $57, $57, $57, $49, $DA, $FF, $FF, $49, $FF, $49, $E3, $E3;
  db  $49, $49, $49, $49, $49, $57, $57, $57, $49, $DA, $FF, $FF, $FF, $49, $E3, $E3;
  db  $FF, $FF, $FF, $FF, $FF, $49, $57, $57, $57, $49, $49, $49, $49, $49, $49, $E3;
  db  $FA, $FF, $FF, $FF, $FA, $49, $57, $57, $49, $CC, $CC, $CC, $CC, $CC, $CC, $49;
  db  $49, $49, $49, $49, $49, $56, $56, $49, $CC, $49, $49, $49, $49, $49, $49, $E3;
  db  $E3, $49, $56, $56, $56, $56, $56, $56, $49, $CC, $CC, $CC, $CC, $CC, $49, $E3;
  db  $E3, $E3, $49, $49, $56, $56, $56, $56, $56, $49, $49, $49, $49, $49, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $49, $49, $49, $49, $49, $E3, $E3, $E3, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3;



Sprite2:
  db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $E3, $49, $49, $49, $49, $49, $49, $E3, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $49, $49, $5B, $5B, $5B, $49, $FF, $FF, $49, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $49, $5B, $5B, $57, $57, $49, $FF, $FF, $FF, $FF, $49, $E3, $E3, $E3;
  db  $E3, $49, $5B, $57, $57, $57, $57, $49, $DA, $FF, $FF, $49, $FF, $49, $E3, $E3;
  db  $49, $57, $57, $57, $57, $57, $57, $49, $DA, $FF, $FF, $49, $FF, $49, $E3, $E3;
  db  $49, $57, $57, $57, $57, $57, $57, $57, $49, $DA, $FF, $FF, $FF, $49, $E3, $E3;
  db  $49, $49, $49, $49, $49, $57, $57, $57, $57, $49, $49, $49, $49, $49, $49, $E3;
  db  $FA, $FF, $FF, $FF, $FA, $49, $57, $57, $49, $CC, $CC, $CC, $CC, $CC, $CC, $49;
  db  $FF, $FF, $FF, $FF, $49, $56, $56, $49, $CC, $49, $49, $49, $49, $49, $49, $E3;
  db  $FF, $FF, $FA, $49, $56, $56, $56, $56, $49, $CC, $CC, $CC, $CC, $CC, $49, $E3;
  db  $49, $49, $49, $49, $56, $56, $56, $56, $56, $49, $49, $49, $49, $49, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $49, $49, $49, $49, $49, $E3, $E3, $E3, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3;




PipeEnd:
  db  $69, $8D, $8D, $91, $91, $8D, $8D, $8D, $6D, $6D, $6D, $6D, $6D, $6D, $6D, $49;
  db  $8D, $DE, $DE, $DE, $DE, $DD, $B9, $B9, $B9, $95, $95, $71, $71, $70, $70, $49;
  db  $8D, $DE, $DE, $DE, $D9, $B9, $B9, $95, $95, $75, $71, $71, $70, $4C, $50, $49;
  db  $8D, $DE, $DE, $DE, $D9, $B9, $B9, $B5, $95, $95, $71, $71, $70, $4C, $50, $49;
  db  $8D, $DE, $DE, $DE, $D9, $B9, $B9, $B5, $95, $95, $71, $71, $70, $4C, $50, $49;
  db  $8D, $DE, $DE, $DE, $DD, $B9, $B9, $B9, $95, $95, $75, $71, $70, $50, $50, $49;
  db  $69, $6D, $6D, $6D, $6D, $6D, $6D, $6D, $6D, $6D, $4D, $4D, $4D, $4D, $4D, $69;
  db  $E3, $6D, $6D, $6D, $6D, $6D, $6D, $6D, $6D, $6D, $6D, $4D, $4D, $4D, $6D, $E3;
  db  $E3, $91, $B9, $B9, $DD, $DE, $DE, $B9, $B9, $95, $75, $71, $50, $50, $6D, $E3;
  db  $E3, $91, $B5, $B9, $D9, $DE, $DE, $B9, $B9, $95, $71, $71, $4C, $50, $6D, $E3;
  db  $E3, $91, $B5, $B9, $D9, $DE, $DE, $B9, $B9, $95, $75, $71, $4C, $50, $6D, $E3;
  db  $E3, $91, $B5, $B9, $D9, $DE, $DE, $B9, $B9, $95, $75, $71, $4C, $50, $6D, $E3;
  db  $E3, $91, $B5, $B9, $D9, $DE, $DE, $B9, $B9, $95, $75, $71, $4C, $50, $6D, $E3;
  db  $E3, $91, $B5, $B9, $D9, $DE, $DE, $B9, $B9, $95, $75, $71, $4C, $50, $6D, $E3;
  db  $E3, $91, $B5, $B9, $D9, $DE, $DE, $B9, $B9, $95, $75, $71, $4C, $50, $6D, $E3;
  db  $E3, $91, $B5, $B9, $D9, $DE, $DE, $B9, $B9, $95, $75, $71, $4C, $50, $6D, $E3;



Pipe:
  db  $E3, $91, $95, $B9, $D9, $DE, $DE, $B9, $B9, $95, $75, $71, $4C, $50, $6D, $E3;
  db  $E3, $91, $95, $B9, $D9, $DE, $DE, $B9, $B9, $95, $75, $71, $4C, $50, $6D, $E3;
  db  $E3, $91, $B5, $B9, $D9, $DE, $DE, $B9, $B9, $95, $75, $71, $4C, $50, $6D, $E3;
  db  $E3, $91, $95, $B9, $D9, $DE, $DE, $B9, $B9, $95, $75, $71, $4C, $50, $6D, $E3;
  db  $E3, $91, $B5, $B9, $D9, $DE, $DE, $B9, $B9, $95, $75, $71, $4C, $50, $6D, $E3;
  db  $E3, $91, $95, $B9, $D9, $DE, $DE, $B9, $B9, $95, $75, $71, $4C, $50, $6D, $E3;
  db  $E3, $91, $B5, $B9, $D9, $DE, $DE, $B9, $B9, $95, $75, $71, $4C, $50, $6D, $E3;
  db  $E3, $91, $B5, $B9, $D9, $DE, $DE, $B9, $B9, $95, $75, $71, $4C, $50, $6D, $E3;
  db  $E3, $91, $95, $B9, $D9, $DE, $DE, $B9, $B9, $95, $75, $71, $4C, $50, $6D, $E3;
  db  $E3, $91, $95, $B9, $D9, $DE, $DE, $B9, $B9, $95, $75, $71, $4C, $50, $6D, $E3;
  db  $E3, $91, $B5, $B9, $D9, $DE, $DE, $B9, $B9, $95, $75, $71, $4C, $50, $6D, $E3;
  db  $E3, $91, $B5, $B9, $D9, $DE, $DE, $B9, $B9, $95, $75, $71, $4C, $50, $6D, $E3;
  db  $E3, $91, $B5, $B9, $D9, $DE, $DE, $B9, $B9, $95, $75, $71, $4C, $50, $6D, $E3;
  db  $E3, $91, $B5, $B9, $D9, $DE, $DE, $B9, $B9, $95, $75, $71, $4C, $50, $6D, $E3;
  db  $E3, $91, $B5, $B9, $D9, $DE, $DE, $B9, $B9, $95, $75, $71, $4C, $50, $6D, $E3;
  db  $E3, $91, $B5, $B9, $D9, $DE, $DE, $B9, $B9, $95, $75, $71, $4C, $50, $6D, $E3;

Pipex:
db  $E3

END START 
