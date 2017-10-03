;Author : John McManus - john@appyappster.com 
;Resources: Sprites scraped from internet and used UDGeedNext, 
;           http://chuntey.arjunnair.in, code snips from Jonathan Cauldwell 

;0.1 - JMcManus - 30/09/2017 -rebuild from scratch
;



;Loading background - Russ McNulty code :)

  

  M_GETSETDRV equ $89
  F_OPEN equ $9a
  F_CLOSE equ $9b
  F_READ equ $9d
  F_WRITE equ $9e
  F_SEEK equ $9f
  F_GET_DIR equ $a8
  F_SET_DIR equ $a9
  FA_READ equ $01
  FA_APPEND equ $06
  FA_OVERWRITE equ $0C

  ESXDOS macro command
  rst 8
  db command
  endm





  org 32768
  START 
  
  
  

  call setupScreen
  call loadGameBackground
  call drawpavement
  call showLayer2
  call initPipeSprites

  call initEndPipeSprites

  call loadBird1
  call loadBird2

 


  ;call  initiliseSprites
;  ret
  call makeAllSpritesVisible

  ld hl, capID
  ld a, 50
  ld (hl), a
  ld ix, pipe1Data
  call drawPipe

  

  ld ix, pipe2Data
  call drawPipe
  
 
  
  ld ix, pipe3Data
  call drawPipe
  
  ld ix, pipe4Data
  call drawPipe

  ld ix, pipe5Data
  call drawPipe












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
       ;call animatebird
       ;call movebird
       call scrollL2
       call scrollPipes

       ret






scrollL2:
  ;ret

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


















scrollPipes:

  ld hl, capID
  ld a, 50
  ld (hl), a

  ld ix, pipe1Data
  call movePipe2

 
  ld ix, pipe2Data
  call movePipe2

  ld ix, pipe3Data
  call movePipe2

  ld ix, pipe4Data
  call movePipe2

  ld ix, pipe5Data
  call movePipe2


  ld ix, pipe1Data
  call drawPipe

  ld ix, pipe2Data
  call drawPipe

  ld ix, pipe3Data
  call drawPipe
  ld ix, pipe4Data
  call drawPipe
  ld ix, pipe5Data
  call drawPipe



ret

;--------------------ROUTINES------

movePipe:
 ;ix hold pointer to database.
  ;pipe1Data db 0, 32 , 4 , 2 , 10 ; position 2 bytes, top length, gap, start spriteid


 ld a,(ix+1)
 dec a

 cp 16

 jp nz, notOffScreen

  ld a, 1
  ld (ix), a

  ld a, 32

 notOffScreen:

 ld (ix+1), a


 ret


movePipe2:
 ;ix hold pointer to database.
  ;pipe1Data db 0, 32 , 4 , 2 , 10 ; position 2 bytes, top length, gap, start spriteid

  ld a,(ix)
  cp 0
  jp z, check16


  ld a,(ix+1)
  dec a
  cp 255
  jp nz, saveX
  ld (ix+1), a
  ld a,0
  ld (ix), a
  jp endMovePipe2


saveX:
  
  ld (ix+1), a
  jp endMovePipe2


check16:
  ld a,(ix+1)
  dec a
  cp 16
  jp nz, notOffScreen2

  ld a,1
  ld (ix+0), a
  ld a,32
  ld (ix+1), a
  jp endMovePipe2


 notOffScreen2:

 ld (ix+1), a

endMovePipe2:

 ret



drawPipe:
  ;ix hold pointer to database.
  ;pipe1Data db 0, 32 , 4 , 2 , 10 ; position 2 bytes, top length, gap, start spriteid
  ;local variables
  

  ;seed data
  ld hl, pipeYPos
  ld (hl), 32 ; pipe y position - always starts at 32

  ld hl, pipeXPos ;
  ld a, (ix)
  ld (hl), a ; save xpos from databased


  ld hl, pipeXPos+1 ;
  ld a, (ix+1)
  ld (hl), a ; save xpos from databased


  ld a,(ix+2)
  ld (pipeTopTileCount), a ; counter for top column
  ld a,(ix+3)
  ld (pipeGap), a ; counter for gap
  ld a , (ix+4)
  ld (pipeSpriteToUse), a


topPipeLoop:
  ld a, (pipeSpriteToUse)
  ld bc, $303b
  out (c), a ; 
  ld bc, $57
  ld a, (pipeXPos+1) ; xpos >>> 32 on boarder ->> ;; need to make this 32bit
  out (c), a
  ld a, (pipeYPos) ; ypos ? --- 32 upper ->>>
  out (c), a
  ld a, 0 ; 7-4 is palette offset, bit 3 is X mirror, bit 2 is Y mirror, bit 1 is rotate flag and bit 0 is X MSB. 
  ; 8 flip x 4 flipy 2 rotate 1 msb
  ld a,(pipeXPos) ; if x is on right of screen

  out (c), a

  ld a, (pipeSpriteToUse)
  xor 128
  ;ld a, 137 ; spite visible pattern 0 ??? ;;128 = 0
  out (c), a

  ld a, (pipeYPos)
  add a,16

  ld (pipeYPos), a ; add 16 

  ld hl, pipeSpriteToUse
  inc (hl) ; used next sprite

  ld a, (pipeTopTileCount)
  dec a
  ld (pipeTopTileCount), a

  jp nz, topPipeLoop






;; put a cap on it
  
  ld a, (capID)
  ld hl, capID
  inc (hl)
  push af
 
  ld bc, $303b
  out (c), a ; 
  ld bc, $57
  ld a, (pipeXPos+1) ; xpos >>> 32 on boarder ->>
  out (c), a
  ld a, (pipeYPos) ; ypos ? --- 32 upper ->>>
  out (c), a
  ld a, 4 ; 7-4 is palette offset, bit 3 is X mirror, bit 2 is Y mirror, bit 1 is rotate flag and bit 0 is X MSB. 
  ; 8 flip x 4 flipy 2 rotate 1 msb
  ld a,(pipeXPos) ; if x is on right of screen
  xor 4

  out (c), a
  pop af
  xor 128
  ;ld a, 137 ; spite visible pattern 0 ??? ;;128 = 0
  out (c), a
  ld a, (pipeYPos)
  add a,16
  ld (pipeYPos), a ; add 16 
incGap: 
  ld a, (pipeYPos)
  add a,16
  ld (pipeYPos), a ; add 16 

  ld a, (pipeGap)
  dec a
  ld (pipeGap), a

  jp nz, incGap


;; put a cap on it
  ld a, (capID)
  ld hl, capID
  inc (hl)
  push af
 
  ld bc, $303b
  out (c), a ; 
  ld bc, $57
  ld a, (pipeXPos+1) ; xpos >>> 32 on boarder ->>
  out (c), a
  ld a, (pipeYPos) ; ypos ? --- 32 upper ->>>
  out (c), a
  ld a, 0 ; 7-4 is palette offset, bit 3 is X mirror, bit 2 is Y mirror, bit 1 is rotate flag and bit 0 is X MSB. 
  ; 8 flip x 4 flipy 2 rotate 1 msb
  ld a,(pipeXPos) ; if x is on right of screen

  out (c), a
  pop af
  xor 128
  ;ld a, 137 ; spite visible pattern 0 ??? ;;128 = 0
  out (c), a


  ld a, (pipeYPos)

pipeBottomLoop:

 sub 208
  jp z , donepipe
  ;ld hl, pipeSpriteToUse
  ;inc (hl) ; used next sprite

  ld a, (pipeSpriteToUse)
 ; ld a, 11
  
  push af
 
  ld bc, $303b
  out (c), a ; 
  ld bc, $57
  ld a, (pipeXPos+1) ; xpos >>> 32 on boarder ->>
  out (c), a
  ld a, (pipeYPos) ; ypos ? --- 32 upper ->>>
  out (c), a
  ld a, 0 ; 7-4 is palette offset, bit 3 is X mirror, bit 2 is Y mirror, bit 1 is rotate flag and bit 0 is X MSB. 
  ; 8 flip x 4 flipy 2 rotate 1 msb
  ld a,(pipeXPos) ; if x is on right of screen
  xor 4
  out (c), a
  pop af
  xor 128
  ;ld a, 137 ; spite visible pattern 0 ??? ;;128 = 0
  out (c), a

  




  ld a, (pipeYPos)
  add a,16
  ld (pipeYPos), a ; add 16 
 
  ld hl, pipeSpriteToUse
  inc (hl) ; used next sprite

  jp pipeBottomLoop



donepipe:



  ret 


  

  


setupScreen:
  ld a, 56
  ld (23693),a
  call 3503
  ld a, 5 ;(2=red, 5 =cyan )
  call 8859
  ret





loadGameBackground:

  ld a, '*' ; use current drive
  ld b, FA_READ ; set mode
  ld ix, bmp ; filename in ix
  ESXDOS F_OPEN
  ret c ; return if failed
  ld (handle), a ; store handle
  ld l, 0 ; seek from start of file
  ld bc, 0
  ld de, 1078 ; 54 byte header and 1024 palette

  ESXDOS F_SEEK
  ld a, 3 ; enable write bit and make visible
  writeLoop:
  push af ; store bitmask
  ld bc, $123b ; set port for writing
  out (c), a
  ld a, (handle) ; restore handle
  ld ix, $0 ; read 16k into addr 0
  ld bc, $4000
  ESXDOS F_READ
  pop af ; restore bit mask
  add a, 64 ; next page
  jr nc, writeLoop
  ESXDOS F_CLOSE ; close file
  ld bc, $123b ; reset write bit
  ld a, 2 ; but keep visible
  out (c), a
  ret
  
  bmp: db "nbirdbg.bmp"
  ;bmp: db "bgnew.bmp"
  handle: db 0





  
  ret 





  ;column sprite
  ; 5columns 
  ;total length top to botton 192
  ;192 /16 = 12
  ; nothing on bottom row 11 
  ; smallest gap of 11- 2 = 9
  ; pipe - end = 9 -2 = 7 sprites per colum max, 5 columns = 5 *  7 = 35 
  ; sprite 10-44
  ; 10 pipe end  - sprite 50-59




showSprite:
  push af
 
  ld bc, $303b
  out (c), a ; 
  ld bc, $57
  ld a, 128 ; xpos >>> 32 on boarder ->>
  out (c), a
  ld a, 96 ; ypos ? --- 32 upper ->>>
  out (c), a
  ld a, 0 ; 7-4 is palette offset, bit 3 is X mirror, bit 2 is Y mirror, bit 1 is rotate flag and bit 0 is X MSB. 
  ; 8 flip x 4 flipy 2 rotate 1 msb
  out (c), a
  pop af
  xor 128
  ;ld a, 137 ; spite visible pattern 0 ??? ;;128 = 0
  out (c), a

  
  ret 

;;;column databases


;;load ix with data start for column

  
  




pipeSpriteCount db 40

initPipeSprites:
 ;ld hl, Pipe 
 ;ld a, 10
 ld hl, Pipe 
 ld a, 10
 ;loadSpriteData:
  ld bc, $303b
  out (c), a ; 
nextsprite:
   ld hl, Pipe 
  ld de, 256
  ld bc, $5b
load_sprite_data_loop:
  outi  ;reads (hl) and sends to port (c) e.g load the sprite into sprite engine thingy
  inc b ; inc a a outi is decrementing it.
  dec de
  ld a,d
  or e
  jr nz, load_sprite_data_loop


  ld a,(pipeSpriteCount)
  dec a
  ld (pipeSpriteCount), a
  jr nz, nextsprite
  
  ret 



pipeEndSpriteCount db 10

initEndPipeSprites:
 ;ld hl, Pipe 
 ;ld a, 10
 ld hl, PipeEnd 
 ld a, 50
; loadSpriteData:
  ld bc, $303b
  out (c), a ; 
nextsprite2:
   ld hl, PipeEnd 
  ld de, 256
  ld bc, $5b
load_spriteEnd_data_loop:
  outi  ;reads (hl) and sends to port (c) e.g load the sprite into sprite engine thingy
  inc b ; inc a a outi is decrementing it.
  dec de
  ld a,d
  or e
  jr nz, load_spriteEnd_data_loop


  ld a,(pipeEndSpriteCount)
  dec a
  ld (pipeEndSpriteCount), a
  jr nz, nextsprite2
  
  ret 









loadBird1:
  ld a, 61
  ld hl,Sprite1
  ld bc, $303b
  out (c), a ; 

  ld de, 256
  ld bc, $5b
load_sprite_loopx1:
  outi  ;reads (hl) and sends to port (c) e.g load the sprite into sprite engine thingy
  inc b ; inc a a outi is decrementing it.
  dec de
  ld a,d
  or e
  jr nz, load_sprite_loopx1
 ret


loadBird2:
  ld a, 62
  ld hl,Sprite2
  ld bc, $303b
  out (c), a ; 

  ld de, 256
  ld bc, $5b
load_sprite_loopx2:
  outi  ;reads (hl) and sends to port (c) e.g load the sprite into sprite engine thingy
  inc b ; inc a a outi is decrementing it.
  dec de
  ld a,d
  or e
  jr nz, load_sprite_loopx2


  
  ret 




drawpavement:

paveno db 15
pavex db 0

pave1:  
    
    ld ix, BottomSprite
    
    ;ld iy, topColumns ; x position

    ld de ,16; 16 across
    push de ; save de
;    ld  hl,0
    ld a, (pavex) ; a postion = 0
    ld h, 48
    ld l, a ; x position of column y = 0
 
;hl = start of column
    dec h
pavec1i:
    inc h
    ld a, 1+64+64;
    ld  bc, $123b
    out (c),a     ; set bank
    ld  de,16 ; 16*16 sprite 
pave1nc:
    ;
   ; ld a,0 ; black
    ld a, (ix) ; copy pipe pixel
    inc ix ; work back the way 

    ld (hl),a ;place the pixel
    inc hl; next colum

    
  
    dec de; count down
    ld a,d ; zero check
    or e ; zero check
    jp nz, pave1nc ; next col
    ld a, l ; 
    sub  16  ;jump down a row
    ld l, a
    pop de
    dec de
    ld a,d
    push de
    or e
    jp nz, pavec1i
    pop de



    ld a,(pavex)
    add a,16
    ld (pavex),a

    ld a,(paveno)
    dec a
    ld (paveno),a

    jp nz, pave1


;ret 
rowno db 15
pavelast1:  
    
    ld ix, BottomSprite
    
    ;ld iy, topColumns ; x position

    ld de ,16; 16 across
    push de ; save de
;    ld  hl,0
    ld a, (pavex) 
    ld h, 48
    ld l, a ; x position of column y = 0
 
;hl = start of column
    dec h
pavelastc1i:
    inc h
    ld a, 1+64+64;
    ld  bc, $123b
    out (c),a     ; set bank
    ld  de,16 ; 16*16 sprite 
pavelast1nc:
    ;
   ; ld a,0 ; black
    ld a, (ix) ; copy pipe pixel
    inc ix ; work back the way 

    ld (hl),a ;place the pixel
    inc hl; next colum

    





    dec de; count down
    ld a,d ; zero check
    or e ; zero check
    jp nz, pavelast1nc ; next col



    ld a, l ; 
    sub  16
    ld l, a
    ld de ,16

    ld a,(rowno)
    dec a 
    ld (rowno), a
    jp nz, pavelast1nc



    ld a, (ix) ; copy pipe pixel
    inc ix ; work back the way 
    ld (hl),a ;place the pixel
    inc hl; next colum

 
  


    ld a, (ix) ; copy pipe pixel
    inc ix ; work back the way 
    ld (hl),a ;place the pixel
    inc hl; next colum

   
  

    ld a, (ix) ; copy pipe pixel
    inc ix ; work back the way 
    ld (hl),a ;place the pixel
    inc hl; next colum

    
  

    ld a, (ix) ; copy pipe pixel
    inc ix ; work back the way 
    ld (hl),a ;place the pixel
    inc hl; next colum

    
  

    ld a, (ix) ; copy pipe pixel
    inc ix ; work back the way 
    ld (hl),a ;place the pixel
    inc hl; next colum

   
  

    ld a, (ix) ; copy pipe pixel
    inc ix ; work back the way 
    ld (hl),a ;place the pixel
    inc hl; next colum

    
  

    ld a, (ix) ; copy pipe pixel
    inc ix ; work back the way 
    ld (hl),a ;place the pixel
    inc hl; next colum

   
  

    ld a, (ix) ; copy pipe pixel
    inc ix ; work back the way 
    ld (hl),a ;place the pixel
    inc hl; next colum

    
  

    ld a, (ix) ; copy pipe pixel
    inc ix ; work back the way 
    ld (hl),a ;place the pixel
    inc hl; next colum

  

    ld a, (ix) ; copy pipe pixel
    inc ix ; work back the way 
    ld (hl),a ;place the pixel
    inc hl; next colum

    
  

    ld a, (ix) ; copy pipe pixel
    inc ix ; work back the way 
    ld (hl),a ;place the pixel
    inc hl; next colum

    
  

    ld a, (ix) ; copy pipe pixel
    inc ix ; work back the way 
    ld (hl),a ;place the pixel
    inc hl; next colum

    
  

    ld a, (ix) ; copy pipe pixel
    inc ix ; work back the way 
    ld (hl),a ;place the pixel
    inc hl; next colum

    
  

    ld a, (ix) ; copy pipe pixel
    inc ix ; work back the way 
    ld (hl),a ;place the pixel
    inc hl; next colum

    
  

    ld a, (ix) ; copy pipe pixel
    inc ix ; work back the way 
    ld (hl),a ;place the pixel
    inc hl; next colum

    
  



ret 

showLayer2:

  ld  bc, $123b
    ld  a,2 
    out (c),a  ; make layer 2 visibile.

  ret   





makeAllSpritesVisible:
 
  ld bc, $243b
  ld a, 21
  out (c), a

  ;OUT 0x253B, 1; REM All sprites visible
  ld bc, $253b
  ld a, 1
  out (c), a

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

BottomSprite:
  db  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00;
  db  $DE, $DE, $DE, $DE, $DE, $DE, $DE, $DE, $DE, $DE, $DE, $DE, $DE, $DE, $DE, $DE;
  db  $99, $99, $99, $99, $99, $99, $99, $99, $75, $75, $75, $75, $75, $75, $99, $99;
  db  $99, $99, $99, $99, $99, $99, $99, $75, $75, $75, $75, $75, $75, $99, $99, $99;
  db  $99, $99, $99, $99, $99, $99, $75, $75, $75, $75, $75, $75, $99, $99, $99, $99;
  db  $99, $99, $99, $99, $99, $75, $75, $75, $75, $75, $75, $99, $99, $99, $99, $99;
  db  $99, $99, $99, $99, $75, $75, $75, $75, $75, $75, $99, $99, $99, $99, $99, $99;
  db  $99, $99, $99, $75, $75, $75, $75, $75, $75, $99, $99, $99, $99, $99, $99, $99;
  db  $99, $99, $75, $75, $75, $75, $75, $75, $99, $99, $99, $99, $99, $99, $99, $99;
  db  $4C, $4C, $4C, $4C, $4C, $4C, $4C, $4C, $4C, $4C, $4C, $4C, $4C, $4C, $4C, $4C;
  db  $D5, $D5, $D5, $D5, $D5, $D5, $D5, $D5, $D5, $D5, $D5, $D5, $D5, $D5, $D5, $D5;
  db  $DA, $DA, $DA, $DA, $DA, $DA, $DA, $DA, $DA, $DA, $DA, $DA, $DA, $DA, $DA, $DA;
  db  $DA, $DA, $DA, $DA, $DA, $DA, $DA, $DA, $DA, $DA, $DA, $DA, $DA, $DA, $DA, $DA;
  db  $DA, $DA, $DA, $DA, $DA, $DA, $DA, $DA, $DA, $DA, $DA, $DA, $DA, $DA, $DA, $DA;
  db  $DA, $DA, $DA, $DA, $DA, $DA, $DA, $DA, $DA, $DA, $DA, $DA, $DA, $DA, $DA, $DA;
  db  $DA, $DA, $DA, $DA, $DA, $DA, $DA, $DA, $DA, $DA, $DA, $DA, $DA, $DA, $DA, $DA;



  pretim defb 0
  ScrollIndex db 0
;  pipe1Data db 1, 16 , 2 , 2 , 10 ; position 2 bytes, top length, gap, start spriteid
  pipe1Data db 0, 70 , 2 , 2 , 10 ; position 2 bytes, top length, gap, start spriteid
  pipe2Data db 0, 124 , 3 , 3 , 18 ; position 2 bytes, top length, gap, start spriteid
  pipe3Data db 0, 178, 3 , 2 , 26 ; position 2 bytes, top length, gap, start spriteid
  pipe4Data db 0, 232 ,4 , 2 , 34 ; position 2 bytes, top length, gap, start spriteid
  pipe5Data db 1, 30 ,5 , 2 , 42 ; position 2 bytes, top length, gap, start spriteid
  

  pipeXPos db 0,0
  pipeYPos db 0 
  pipeTopTileCount db 0 
  pipeSpriteToUse db 0
  pipeGap db 0

  
  capID db 50 ; starting cap ID sprite


END START 