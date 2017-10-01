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
  xxxxxxxxx db 0 ; broke data
  pipe1Data db 0, 32 , 1 , 2 , 10 ; position 2 bytes, top length, gap, start spriteid
  pipe2Data db 0, 64 , 2 , 2 , 18 ; position 2 bytes, top length, gap, start spriteid
  pipe3Data db 0, 96 , 3 , 2 , 26 ; position 2 bytes, top length, gap, start spriteid
 ;
  pipe4Data db 0, 128 ,4 , 2 , 34 ; position 2 bytes, top length, gap, start spriteid
  pipe5Data db 0, 160 ,5 , 2 , 42 ; position 2 bytes, top length, gap, start spriteid
 ; pipe5Data db 0, 134 , 7 , 2 , 42 ; position 2 bytes, top length, gap, start spriteid
  

  pipeXPos db 0,0
  pipeYPos db 0 
  pipeTopTileCount db 0 
  pipeSpriteToUse db 0
  pipeGap db 0

  
  capID db 50 ; starting cap ID sprite

  
  di

  call setupScreen
  
  call initPipeSprites

  call initEndPipeSprites

  call loadBird1
  call loadBird2

 


  ;call  initiliseSprites
;  ret
  call makeAllSpritesVisible
  ;ld hl, capID
  ;ld a, 50
  ld (hl), a
  ld ix, pipe1Data
  call drawPipe

  

  ld ix, pipe2Data
  call drawPipe
  ei
  ret
  
  ld ix, pipe3Data
  call drawPipe
  
  ld ix, pipe4Data
  call drawPipe

  ld ix, pipe5Data
  call drawPipe

ei

ret

;--------------------ROUTINES------

drawPipe:
  ;ix hold pointer to database.
  ;pipe1Data db 0, 32 , 4 , 2 , 10 ; position 2 bytes, top length, gap, start spriteid
  ;local variables
  

  ;seed data
  ld hl, pipeYPos
  ld (hl), 32 ; pipe y position
  ld hl, pipeXPos+1
  ld a, (ix+1)
  ld (hl), a


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
  out (c), a
  pop af
  xor 128
  ;ld a, 137 ; spite visible pattern 0 ??? ;;128 = 0
  out (c), a

  ld a, (pipeYPos)
  add a,16
  add a,16
  ld (pipeYPos), a ; add 16 



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
  out (c), a
  pop af
  xor 128
  ;ld a, 137 ; spite visible pattern 0 ??? ;;128 = 0
  out (c), a








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




END START 