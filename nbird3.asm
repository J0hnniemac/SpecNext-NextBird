; Author : John McManus
; Date : 6 September 2017
; Credits       : Too Many to name from https://www.facebook.com/groups/specnext/
;               : ZX Spectrum Next FB Group
;
;               Background art tidy up by Simon Butler - :)
;
;
;
;
; Bitmap Load code from Russ McNulty
M_GETSETDRV     equ $89
F_OPEN          equ $9a
F_CLOSE         equ $9b
F_READ          equ $9d
F_WRITE         equ $9e
F_SEEK          equ $9f
F_GET_DIR       equ $a8
F_SET_DIR       equ $a9
FA_READ         equ $01
FA_APPEND       equ $06
FA_OVERWRITE    equ $0C

ESXDOS macro command
        rst 8
        db command
endm




; Static Variables
PIPE1SPRITESTART        equ 10
PIPE2SPRITESTART        equ 18
PIPE3SPRITESTART        equ 26
PIPE4SPRITESTART        equ 34
PIPE5SPRITESTART        equ 42
BIRDANI1                equ 50
BIRDANI2                equ 51
SCOREID1                equ 52
SCOREID2                equ 53
PIPECAPSTART            equ 0 
SCORE1X                 equ 142
SCORE2X                 equ 150
SCOREY                  equ 38
BIRDX                   equ 64
BIRDY                   equ 96




org 32768
START 


;Initilise All Sprites

restartGame:

        call initPipeSprites
        call initEndPipeSprites
        call initBird1
        call initBird2

        ld hl, No1
        call loadScore1
        ld hl, No1
        call loadScore2




startGame:
        
        ;init standard variables 
        ;show start screen
        call resetPipeDB
        call resetBird
        call resets
        call makeAllSpritesInVisible
        call resetScrollL2
        call setupScreen
        ld ix, bmpStart
        call loadABackground
        call showLayer2

startLoop:
        ld hl,23560         ; LAST K system variable.
        ld (hl),0           ; put null value there.
loopa   ld a,(hl)           ; new value of LAST K.
        cp 0                ; is it still zero?
        jr z,loopa           ; yes, so no key pressed.
       

startGameRunning:
        ld ix, bmpGame
        call loadABackground
        call drawpavement
        call showLayer2

        call showScore  

        call makeAllSpritesVisible
;ret
        call showPipes






        

mainGameLoop:






        
        call wait
        jp mainGameLoop



wait   ld hl,pretim        ; previous time setting
       ld a,(23672)        ; current timer setting.
       sub (hl)            ; difference between the two.
       cp 4                ; have two frames elapsed yet?
       jr nc,wait6         ; yes, no more delay.
             ; yes, no more delay.


       ret



 wait6  ld a,(23672)        ; current timer.
       ld (hl),a           ; store this setting.
       ;call decBirdY
       call scrollPipes
       ;call animatebird
       ;call keypress
       call updateScore
       call scrollL2
       call checkCollisions
        call animatebird
      call decBirdY
       
       call keypress
       
       ret




endGameLoop1:

ewait   ld hl,epretim        ; previous time setting
       ld a,(23672)        ; current timer setting.
       sub (hl)            ; difference between the two.
       cp 1                ; have two frames elapsed yet?
       jr nc,ewait0         ; yes, no more delay.
       jp ewait
ewait0  ld a,(23672)        ; current timer.
       ld (hl),a           ; store this setting.
       

       call moveDeadBird

        
       call changebc











        jp endGameLoop1


epretim db 0


bcol db 15

changebc:

        ld a,(bcol)
        dec a
        jp nz, bcok
        ld a, 15
        bcok:
        call 8859
        ld (bcol),a
ret



moveDeadBird:



        ld a, (bird_y)
        inc a
        ld (bird_y), a
        cp 197
        jp z, endScreen ; hit the bottom


        call animatebird2
        



        ret


endScreen:

; wait for keypress


       ld hl,23560         ; LAST K system variable.
        ld (hl),0           ; put null value there.
loopb   ld a,(hl)           ; new value of LAST K.
        cp 0                ; is it still zero?
        jr z,loopb           ; yes, so no key pressed.
jp startGame
;jp restartGame

; Pseudo-random number generator.
; Steps a pointer through the ROM (held in seed), returning the contents
; of the byte at that location.

random ld hl,(seed)        ; pointer to ROM.
       res 5,h             ; stay within first 8K of ROM.
       ld a,(hl)           ; get "random" number from location.
       xor l               ; more randomness.
       inc hl              ; increment pointer.
       ld (seed),hl        ; new position.
       ret


keypress
  ld a , (lastkey)
  ld b, a
  xor a 
  IN A,(254)     ;
  and 31 
  cp b 
  ret z
;statechange
  ld (lastkey), a

flap 
  
  ld hl, bird_y
    dec (hl)
    dec (hl)
    dec (hl)
    dec (hl)
    dec (hl)
    dec (hl)
    ;dec (hl)
    ;dec (hl)
    ;dec (hl)
    ;dec (hl)
  ;dec (hl)
endflap
  ret





resets:

        ld a,10
        call resetSpritePositions
        ld a,11
        call resetSpritePositions
        ld a,12
        call resetSpritePositions
        ld a,13
        call resetSpritePositions
        ld a,14
        call resetSpritePositions
        ld a,15
        call resetSpritePositions
        ld a,16
        call resetSpritePositions
        ld a,17
        call resetSpritePositions
        ld a,18
        call resetSpritePositions
        ld a,19
        call resetSpritePositions
        ld a,20
        call resetSpritePositions
        ld a,21
        call resetSpritePositions
        ld a,22
        call resetSpritePositions
        ld a,23
        call resetSpritePositions
        ld a,24
        call resetSpritePositions
        ld a,25
        call resetSpritePositions
        ld a,26
        call resetSpritePositions
        ld a,27
        call resetSpritePositions
        ld a,28
        call resetSpritePositions
        ld a,29
        call resetSpritePositions
        ld a,30
        call resetSpritePositions
        ld a,31
        call resetSpritePositions
        ld a,32
        call resetSpritePositions
        ld a,33
        call resetSpritePositions
        ld a,34
        call resetSpritePositions
        ld a,35
        call resetSpritePositions
        ld a,36
        call resetSpritePositions
        ld a,37
        call resetSpritePositions
        ld a,38
        call resetSpritePositions
        ld a,39
        call resetSpritePositions
        ld a,40
        call resetSpritePositions
        ld a,41
        call resetSpritePositions
        ld a,42
        call resetSpritePositions
        ld a,43
        call resetSpritePositions
        ld a,44
        call resetSpritePositions
        ld a,45
        call resetSpritePositions
        ld a,46
        call resetSpritePositions
        ld a,47
        call resetSpritePositions
        ld a,48
        call resetSpritePositions
        ld a,49
        call resetSpritePositions
        
 

ret



resetSpritePositions:

;        ld a,0
        push af
        ld bc, $303b
        out (c), a ; select sprite 3
        ld bc, $57
        ld a, 0 ; xpos >>> 32 on boarder ->>
        out (c), a
        ld a, 0 ; ypos ? --- 32 upper ->>>
        out (c), a
        ld a, 0 ; 7-4 is palette offset, bit 3 is X mirror, bit 2 is Y mirror, bit 1 is rotate flag and bit 0 is X MSB. 
  ; 8 flip x 4 flipy 2 rotate 1 msb
        out (c), a
        pop af
        xor 128
  ;ld a, 137 ; spite visible pattern 0 ??? ;;128 = 0
        out (c), a

  
  ret   






;Initilise Sprite Routine
initPipeSprites:
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





initEndPipeSprites:
        ld hl, PipeEnd 
        ld a, PIPECAPSTART
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


loadScore1:
        ld a, SCOREID1
  ;ld hl,No1
        ld bc, $303b
        out (c), a ; 
        ld de, 256
        ld bc, $5b
load_score_loopx1:
        outi  ;reads (hl) and sends to port (c) e.g load the sprite into sprite engine thingy
        inc b ; inc a a outi is decrementing it.
        dec de
        ld a,d
        or e
        jr nz, load_score_loopx1
        ret

loadScore2:
        ld a, SCOREID2
  ;ld hl,No2
        ld bc, $303b
        out (c), a ; 
        ld de, 256
        ld bc, $5b
load_score_loopx2:
        outi  ;reads (hl) and sends to port (c) e.g load the sprite into sprite engine thingy
        inc b ; inc a a outi is decrementing it.
        dec de
        ld a,d
        or e
        jr nz, load_score_loopx2
        
        ret







;---------------------------------------------------------------------------------------

initBird1:
        ld a, BIRDANI1
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


initBird2:
  ld a, BIRDANI2
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


ld hl,paveno
ld (hl), 15
ld hl,pavex
ld (hl), 0

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


        ld hl, rowno
        ld (hl), 15
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

;-------------Load Backgrounds--------------
loadStartBackground:

  ld a, '*' ; use current drive
  ld b, FA_READ ; set mode
  ld ix, bmpStart ; filename in ix
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
  
  

  loadABackground:
  ld a, 0
  ld (handle), a
  ld a, '*' ; use current drive
  ld b, FA_READ ; set mode
  ;ld ix, bmpGame ; filename in 

  ESXDOS F_OPEN
  ret c ; return if failed
  ld (handle), a ; store handle
  ld l, 0 ; seek from start of file
  ld bc, 0
  ld de, 1078 ; 54 byte header and 1024 palette

  ESXDOS F_SEEK
  ld a, 3 ; enable write bit and make visible
  writeLoop1:
  push af ; store bitmask
  ld bc, $123b ; set port for writing
  out (c), a
  ld a, (handle) ; restore handle
  ld ix, $0 ; read 16k into addr 0
  ld bc, $4000
  ESXDOS F_READ
  pop af ; restore bit mask
  add a, 64 ; next page
  jr nc, writeLoop1
  ESXDOS F_CLOSE ; close file
  ld bc, $123b ; reset write bit
  ld a, 2 ; but keep visible
  out (c), a
  ret






        ret
  

setupScreen:
        ld a, 56
        ld (23693),a
        call 3503
        ld a, 5 ;(2=red, 5 =cyan )
        call 8859

        ret


showLayer2:

        ld  bc, $123b
        ld  a,2 
        out (c),a  ; make layer 2 visibile.

        ret   
showScore:
  ld a, SCOREID1
  push af
  ;ld a,9
  ld bc, $303b
  out (c), a ; select sprite 3
  ld bc, $57
  ld a, SCORE1X ; xpos >>> 32 on boarder ->>
  out (c), a
  ld a, SCOREY ; ypos ? --- 32 upper ->>>
  out (c), a
  ld a, 0 ; 7-4 is palette offset, bit 3 is X mirror, bit 2 is Y mirror, bit 1 is rotate flag and bit 0 is X MSB. 
  ; 8 flip x 4 flipy 2 rotate 1 msb
  out (c), a
  pop af
  xor 128
  ;ld a, 137 ; spite visible pattern 0 ??? ;;128 = 0
  out (c), a


  ld a, SCOREID2
  push af
  ;ld a,9
  ld bc, $303b
  out (c), a ; select sprite 3
  ld bc, $57
  ld a, SCORE2X ; xpos >>> 32 on boarder ->>
  out (c), a
  ld a, SCOREY ; ypos ? --- 32 upper ->>>
  out (c), a
  ld a, 0 ; 7-4 is palette offset, bit 3 is X mirror, bit 2 is Y mirror, bit 1 is rotate flag and bit 0 is X MSB. 
  ; 8 flip x 4 flipy 2 rotate 1 msb
  out (c), a
  pop af
  xor 128
  ;ld a, 137 ; spite visible pattern 0 ??? ;;128 = 0
  out (c), a


  
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


makeAllSpritesInVisible:
 
  ld bc, $243b
  ld a, 21
  out (c), a

  ;OUT 0x253B, 1; REM All sprites visible
  ld bc, $253b
  ld a, 0
  out (c), a

  ret


showPipes:


  ld hl, capID
  ld a, PIPECAPSTART
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



  


  ld hl,playerScore
  ld (hl), 0

  ld hl, No1
  call loadScore2





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


  add a,16
  ld (pipeYPos), a ; add 16 

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




decBirdY

  ld a, (bird_y)

  

  inc a

  ld (bird_y), a
  
  cp 197

  jp z, endGame ; hit the bottom


inc a

  ld (bird_y), a
  
  cp 197

  jp z, endGame ; hit the bottom

inc a

  ld (bird_y), a
  
  cp 197

  jp z, endGame ; hit the bottom
ret 
inc a

  ld (bird_y), a
  
  cp 197

  jp z, endGame ; hit the bottom

inc a

  ld (bird_y), a
  
  cp 197

  jp z, endGame ; hit the bottom


  ret

endGame:
 ;jp startGame
  jp endGameLoop1



scrollPipes:

  ld hl, capID
  ld a, PIPECAPSTART
  ld (hl), a

  ld ix, pipe1Data
  call movePipe2
  ld ix, pipe1Data
  call movePipe2

 
  ld ix, pipe2Data
  call movePipe2
    ld ix, pipe2Data
  call movePipe2

  ld ix, pipe3Data
  call movePipe2
   ld ix, pipe3Data
  call movePipe2

  ld ix, pipe4Data
  call movePipe2
  ld ix, pipe4Data
  call movePipe2

  ld ix, pipe5Data
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

animatebird:
  ;get birdaniIndex
  ld bc,0
  ld a, (birdaniIndex)
  ld c, a
  ld hl, birdaniSeqquence
  add hl,bc ; hl = sprite
  ld a,(hl)
  call hideSprite

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

  call showSprite
  ;halt

  ret



animatebird2:
  ;get birdaniIndex
  ld bc,0
  ld a, (birdaniIndex)
  ld c, a
  ld hl, birdaniSeqquence
  add hl,bc ; hl = sprite
  ld a,(hl)
  call hideSprite

  ;sprite is hidden, get next sprite


  ;ld hl, birdaniIndex
  ;dec (hl)
 ; 
  ;jp NZ,indexok2 ; index > 1
  ;ld (hl), 2 ;> reset index

indexok2:
  ;index is saved, lookup bird
  ld bc,0
  ld a, (birdaniIndex)
  ld c, a
  ld hl, birdaniSeqquence
  add hl,bc ; hl = sprite
  ld a,(hl)

  call showSprite2


  ret





updateScore:


  ;ld hl, No1
  ;call loadScore1
  ;ld hl, No1
  ;call loadScore2



  ld a,(playerScore)
  cp 0
  jp z, zero
ld a,(playerScore)
  cp 1
  jp z, one
ld a,(playerScore)
  cp 2
  jp z, two
ld a,(playerScore)

  cp 3
  jp z, three
ld a,(playerScore)
  cp 4
  jp z, four
ld a,(playerScore)
  cp 5
  jp z, five
ld a,(playerScore)
  cp 6
  jp z, six
ld a,(playerScore)
  cp 7
  jp z, seven
ld a,(playerScore)
  cp 8
  jp z, eight
ld a,(playerScore)
  cp 9
  jp z, nine

  ld hl, No2
  call loadScore1
  jp endScore2
; not 0-9
  jp xone





  






zero:
  ld hl, No1
  call loadScore2
  jp endScore2

one:
  ld hl, No2
  call loadScore2
  jp endScore2

two:
  ld hl, No3
  call loadScore2
  jp endScore2
three:
  ld hl, No4
  call loadScore2
  jp endScore2

four:
  ld hl, No5
  call loadScore2
  jp endScore2


five:
  ld hl, No6
  call loadScore2
  jp endScore2

six:
  ld hl, No7
  call loadScore2
  jp endScore2

seven:
  ld hl, No8
  call loadScore2
  jp endScore2

eight:
  ld hl, No9
  call loadScore2
  jp endScore2

nine:
  ld hl, No10
  call loadScore2
  jp endScore2


xzero:
  ld hl, No1
  call loadScore1
  jp endScore2

xone:
  ld hl, No2
  call loadScore1
  jp endScore2

xtwo:
  ld hl, No3
  call loadScore1
  jp endScore2
xthree:
  ld hl, No4
  call loadScore1
  jp endScore2

xfour:
  ld hl, No5
  call loadScore1
  jp endScore2


xfive:
  ld hl, No6
  call loadScore1
  jp endScore2

xsix:
  ld hl, No7
  call loadScore1
  jp endScore2

xseven:
  ld hl, No8
  call loadScore1
  jp endScore2

xeight:
  ld hl, No9
  call loadScore1
  jp endScore2

xnine:
  ld hl, No10
  call loadScore1
  jp endScore2





endScore2:

call showScore 







  ret

scrollL2:
  ;ret

  ld      bc, $243B    ; select the scroll register
  ld      a,22
  out     (c),a     ; select layer 2 "X" scroll

  ld  a,(ScrollIndex)
  inc a   
  ;inc a 
  ld  (ScrollIndex),a

  ld      bc, $253B
  out     (c),a   



  ret

resetScrollL2:
  ;ret

  ld      bc, $243B    ; select the scroll register
  ld      a,22
  out     (c),a     ; select layer 2 "X" scroll

  ld  a,0
  inc a   
  ;inc a 
  ld  (ScrollIndex),a

  ld      bc, $253B
  out     (c),a   



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
  
  cp 48
  jp nz, skip1
  ld hl,playerScore
  inc (hl)


skip1:
  cp 16
  jp nz, notOffScreen2
  
  

  ld a,1
  ld (ix+0), a
  ld a,32
  ld (ix+1), a


;;; change length
        call random
        and 4
        add a,1
        ld (ix+2), a

       call random
        and 2
        add a,2
        ld (ix+3), a





  jp endMovePipe2


 notOffScreen2:

 ld (ix+1), a

endMovePipe2:

 ret

hideSprite:
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


showSprite:
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
; DataStores
showSprite2:
  push af
  ;ld a,9
  ld bc, $303b
  out (c), a ; select sprite 3
  ld bc, $57
  ld a, (bird_x) ; xpos >>> 32 on boarder ->>
  out (c), a
  ld a, (bird_y) ; ypos ? --- 32 upper ->>>
  out (c), a
  ld a, 2 ; 7-4 is palette offset, bit 3 is X mirror, bit 2 is Y mirror, bit 1 is rotate flag and bit 0 is X MSB. 
  ; 8 flip x 4 flipy 2 rotate 1 msb
  out (c), a
  pop af
  xor 128
  ;ld a, 137 ; spite visible pattern 0 ??? ;;128 = 0
  out (c), a

  
  ret   


;// only 1 set of pipes can be in collision range
checkCollisions:


        ;ld iy, colldb
        ;ld a, 0
        ;ld (iy),a ;reset values
        ;ld (iy+1),a
        

        ld ix, pipe1Data
        ld a,(ix)
        cp 1 ; checking if it is on right of screen
        jp z, skipchk1  ;check column 2
;;load xy info hea
        
        ld a, (ix+1)
        cp 65
        jp c, skipchk1
        cp 80
        jp nc, skipchk1

        



        jp checkGap ; column is in the area where the bird will be
skipchk1:        

        ld ix, pipe2Data
        ld a,(ix)
        cp 1 ; checking if it is on right of screen
        jp z, skipchk2  ;check column 2
;;load xy info hea
        
        ld a, (ix+1)
        cp 65
        jp c, skipchk2
        cp 80
        jp nc, skipchk2

        jp checkGap ; column is in the area where the bird will be
skipchk2:

        ld ix, pipe3Data
        ld a,(ix)
        cp 1 ; checking if it is on right of screen
        jp z, skipchk3  ;check column 2
;;load xy info hea
        
        ld a, (ix+1)
        cp 65
        jp c, skipchk3
        cp 80
        jp nc, skipchk3

        jp checkGap ; column is in the area where the bird will be
skipchk3:        
        ld ix, pipe4Data
        ld a,(ix)
        cp 1 ; checking if it is on right of screen
        jp z, skipchk4  ;check column 2
;;load xy info hea
        
        ld a, (ix+1)
        cp 65
        jp c, skipchk4
        cp 80
        jp nc, skipchk4

        jp checkGap ; column is in the area where the bird will be
skipchk4:        
        ld ix, pipe5Data
        ld a,(ix)
        cp 1 ; checking if it is on right of screen
        jp z, skipchk5  ;check column 2
;;load xy info hea
        
        ld a, (ix+1)
        cp 65
        jp c, skipchk5
        cp 80
        jp nc, skipchk5

        jp checkGap ; column is in the area where the bird will be
skipchk5:        

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

xcheckCollisions:




        ld ix, pipe3Data
        ld a,(ix)
        cp 1 ; checking if it is on right of screen
        jp z, xskipchk2  ;check column 2
;;load xy info hea
        
        ld a, (ix+1)
        add a, 16
        cp 65
        jp c, xskipchk2
        cp 80
        jp nc, xskipchk2

        jp checkGap ; column is in the area where the bird will be
xskipchk2:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
jp cpend

 

checkGap:
        ;jp endGame
        ; see if bird is in the gap
        ld iy, CollisionDB
        ld a, (ix+2)
        ld de, 16
        call Mul8
        ld a, l
        add a,32 ; start on 32
        add a, 16 ; add cap
        ld (iy), a ; ytop 



        ld a, (ix+3)
        ld de, 16
        call Mul8
        ld a, l
        
        ld l,(iy)
        add a,l;


        ld  (iy+1), a ; ybottom





        ld a,(bird_y) ; top of bird
        cp (iy)

        jp c, endGame

        ld a, 0
        dec a ; set c flag

        ld a,(bird_y)
        add a, 12

        cp (iy+1)
        jp nc, endGame



        




cpend:






        ret


Mul8:                            ; this routine performs the operation HL=DE*A
  ld hl,0                        ; HL is used to accumulate the result
  ld b,8                         ; the multiplier (A) is 8 bits wide
Mul8Loop:
  rrca                           ; putting the next bit into the carry
  jp nc,Mul8Skip                 ; if zero, we skip the addition (jp is used for speed)
  add hl,de                      ; adding to the product if necessary
Mul8Skip:
  sla e                          ; calculating the next auxiliary product by shifting
  rl d                           ; DE one bit leftwards (refer to the shift instructions!)
  djnz Mul8Loop
  ret



resetPipeDB:

       ; pipe1Data db 0, 70 , 2 , 4 , PIPE1SPRITESTART ; position 2 bytes, top length, gap, start spriteid
       ; pipe2Data db 0, 124 , 3 , 3 , PIPE2SPRITESTART ; position 2 bytes, top length, gap, start spriteid
       ; pipe3Data db 0, 178, 3 , 2 , PIPE3SPRITESTART ; position 2 bytes, top length, gap, start spriteid
       ; pipe4Data db 0, 232 ,4 , 2 , PIPE4SPRITESTART ; position 2 bytes, top length, gap, start spriteid
       ; pipe5Data db 1, 30 ,5 , 2 , PIPE5SPRITESTART ; position 2 bytes, top length, gap, start spriteid

       ld hl, pipe1Data
       ld (hl), 0
       inc hl
       ld (hl), 70
       inc hl
       ld (hl), 2
       inc hl
       ld (hl), 4
       inc hl
       ld (hl), PIPE1SPRITESTART

       ld hl, pipe2Data
       ld (hl), 0
       inc hl
       ld (hl), 124
       inc hl
       ld (hl), 3
       inc hl
       ld (hl), 3
       inc hl
       ld (hl), PIPE2SPRITESTART

       ld hl, pipe3Data
       ld (hl), 0
       inc hl
       ld (hl), 178
       inc hl
       ld (hl), 3
       inc hl
       ld (hl), 2
       inc hl
       ld (hl), PIPE3SPRITESTART

       ld hl, pipe4Data
       ld (hl), 0
       inc hl
       ld (hl), 232
       inc hl
       ld (hl), 4
       inc hl
       ld (hl), 2
       inc hl
       ld (hl), PIPE4SPRITESTART

       ld hl, pipe5Data
       ld (hl), 1
       inc hl
       ld (hl), 30
       inc hl
       ld (hl), 5
       inc hl
       ld (hl), 2
       inc hl
       ld (hl), PIPE5SPRITESTART


ret

resetBird:
        ld hl,bird_y
        ld (hl), BIRDY

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

No1:
  db  $E3, $E3, $E3, $E3, $00, $00, $00, $00, $00, $00, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $FF, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $FF, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $FF, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $00, $00, $FF, $FF, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $00, $00, $FF, $FF, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $00, $00, $FF, $FF, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $00, $00, $FF, $FF, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $00, $00, $FF, $FF, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $00, $00, $FF, $FF, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $00, $00, $FF, $FF, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $00, $00, $FF, $FF, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $FF, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $FF, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $FF, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $00, $00, $00, $00, $00, $00, $00, $E3, $E3, $E3, $E3;



No2:
  db  $E3, $E3, $E3, $E3, $E3, $00, $00, $00, $00, $00, $00, $E3, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $E3, $00, $00, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $00, $00, $00, $00, $00, $E3, $E3, $E3, $E3;



No3:
  db  $E3, $E3, $E3, $E3, $00, $00, $00, $00, $00, $00, $00, $E3, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $00, $00, $00, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $00, $00, $00, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $E3, $00, $00, $00, $00, $00, $00, $00, $E3, $E3, $E3, $E3;



No4:
  db  $E3, $E3, $E3, $E3, $00, $00, $00, $00, $00, $00, $00, $E3, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $00, $00, $00, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $00, $00, $00, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $E3, $00, $00, $00, $00, $00, $00, $00, $E3, $E3, $E3, $E3;



No5:
  db  $E3, $E3, $E3, $E3, $00, $00, $00, $00, $00, $00, $00, $E3, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $00, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $00, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $00, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $00, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $00, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $00, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $00, $00, $00, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $E3, $00, $00, $00, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $00, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $00, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $00, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $00, $00, $00, $00, $E3, $E3, $E3, $E3;



No6:
  db  $E3, $E3, $E3, $E3, $00, $00, $00, $00, $00, $00, $00, $E3, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $00, $00, $00, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $00, $00, $00, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $E3, $00, $00, $00, $00, $00, $00, $00, $E3, $E3, $E3, $E3;



No7:
  db  $E3, $E3, $E3, $E3, $00, $00, $00, $00, $00, $00, $00, $E3, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $00, $00, $00, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $00, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $E3, $00, $00, $00, $00, $00, $00, $00, $E3, $E3, $E3, $E3;



No8:
  db  $E3, $E3, $E3, $E3, $00, $00, $00, $00, $00, $00, $00, $E3, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $00, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $00, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $00, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $00, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $E3, $00, $00, $00, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $E3, $00, $00, $00, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $00, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $00, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $00, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $00, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $00, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $00, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $E3, $00, $00, $00, $00, $E3, $E3, $E3, $E3;



No9:
  db  $E3, $E3, $E3, $E3, $00, $00, $00, $00, $00, $00, $00, $E3, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $00, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $00, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $E3, $00, $00, $00, $00, $00, $00, $00, $E3, $E3, $E3, $E3;



No10:
  db  $E3, $E3, $E3, $E3, $00, $00, $00, $00, $00, $00, $00, $E3, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $00, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $00, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $00, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $00, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $00, $FF, $FF, $FF, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $E3, $00, $00, $00, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $00, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $00, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $00, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $00, $FF, $FF, $00, $00, $E3, $E3, $E3, $E3;
  db  $E3, $E3, $E3, $E3, $E3, $E3, $E3, $00, $00, $00, $00, $00, $E3, $E3, $E3, $E3;


 
        paveno db 15
        pavex db 0
        rowno db 15
        pretim                  db 0
        lastkey                 db  0
        pipeSpriteCount         db 40
        pipeEndSpriteCount      db 10
        capID db 0 
        pipeXPos db 0,0
        pipeYPos db 0 
        pipeTopTileCount db 0 
        pipeSpriteToUse db 0
        pipeGap db 0


        pipe1Data db 0, 70 , 2 , 4 , PIPE1SPRITESTART ; position 2 bytes, top length, gap, start spriteid
        pipe2Data db 0, 124 , 3 , 3 , PIPE2SPRITESTART ; position 2 bytes, top length, gap, start spriteid
        pipe3Data db 0, 178, 3 , 2 , PIPE3SPRITESTART ; position 2 bytes, top length, gap, start spriteid
        pipe4Data db 0, 232 ,4 , 2 , PIPE4SPRITESTART ; position 2 bytes, top length, gap, start spriteid
        pipe5Data db 1, 30 ,5 , 2 , PIPE5SPRITESTART ; position 2 bytes, top length, gap, start spriteid
  
        playerScore db 0
        seed   equ 23672

        bird_x     db     BIRDX ; x 
        bird_y     db     BIRDY ; y 
        CollisionDB db 0,0
        birdaniSeqquence db BIRDANI1,BIRDANI2,BIRDANI1,BIRDANI2,BIRDANI1,BIRDANI2,BIRDANI1,BIRDANI2,BIRDANI1,BIRDANI2,BIRDANI1,BIRDANI2;
        birdaniIndex db 2;
        ScrollIndex db 0


        bmpStart db "nbstart2.bmp" 
        handle db 0

        bmpGame: db "nbgame2.bmp"
        handle2 db 0

        END START
