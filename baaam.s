.import BaaamFixup
.import SystemClear, SystemAlloc

.macpack apple2

.include "baaam.inc"
;A2FIRM_NO_NAMESPACES = 1
.include "a2firm.inc"

StartPage = $6
StartDiff = $7
TmpPtrA   = $8
TmpPtrB   = $19
TmpPtrC   = $1B


.segment "START"
PreBaaam:
    jmp BaaamRun
Baaam:
BaaamHeader:
    _BaaamHeader EmptyTags, Baaam, BaaamEnd, BaaamFixup, BaaamInit
    .include "baaam-version.inc"

.code
EmptyTags:
    .byte 0
Commands:
    _BaaamHandler "FAKE", Baaam, Baaam+1
    _BaaamHandler "REZ", Baaam, Baaam+1
    _BaaamHandler "REG", Baaam, RegisterModule
    _BaaamHandler "ANOTHERFAKE", Baaam, Baaam+1
    .byte $0
NoCmdMsg:
    scrcode $0D, "BAAAM: UNRECOGNIZED & & COMMAND"
    .byte $0
NoModMsg:
    scrcode $0D, "BAAAM: NO SUCH & HANDLER"
    .byte $0

Eot:
    .byte $0 ; current end of modules table
ModulesTableSize = $40
ModulesTable:
    .res ModulesTableSize

; Running Baaam is exactly the same code as registering a Baaam module
; - just like a module, Baaam starts life at $2000 and gets moved
; into place under the DOS. The only diffeence is that Baaam must
; first ensure that system allocations are cleared (all BASIC.SYSTEM
; allocations are belong to us).
;
; (Baaam can't load directly into its final spot, because until the
; moment we've allocated that memory, it belongs to BASIC as a
; general-purpose buffer. We'd be loading directly atop the buffer
; used to load us, and... thass prolly a bad thing.
BaaamRun:
    ; Discard any existing allocations with BASIC.SYSTEM
    jsr SystemClear
    ; fall through
; Register the module residing at $2000
RegisterModule:
    lda BaaamModuleSize
    jsr SystemAlloc ; make room for the module
    bcc @fine
    jmp MemErr
@fine:
    ; Allocation succeeded.
    ; New start page is in acc, set it as copy destination
    sta ZP::A4H
    sta StartPage
    sbc #(>BaaamModuleStart - 1) ; adjusted for clear carry/set borrow
    sta StartDiff
    ; Set up copy source, too
    lda #>BaaamModuleStart
    sta ZP::A1H
    ; And the end-of-copy
    clc
    adc BaaamModuleSize
    sbc #0 ; minus 1 (last byte is on prev page)
    sta ZP::A2H
    lda #$FF
    sta ZP::A2L
    ldy #0 ; set page starts, but also y must = 0 for Monitor commands.
    sty ZP::A1L
    sty ZP::A4L
    jsr Mon::MOVE
    ; Do reference fix-ups
    ; - Set up a ZP pointer to the offset table
    ;   (we can use the unmoved, $2000-based table for this part)
    lda BaaamModuleFixup
@FixupPtr = ZP::A1
@FixupTmp = ZP::A3
    sta @FixupPtr
    lda #>BaaamModuleStart
    clc
    adc BaaamModuleFixup+1
    sta @FixupPtr+1
    ; Now, walk the table and do the fixups
    ldy #0
@NextFixup:
    lda (@FixupPtr),y
    sta @FixupTmp
    jsr FixupInc
    lda (@FixupPtr),y
    sta @FixupTmp+1
    ; check to see if we got double-zeroes (terminator)
    eor @FixupTmp
@FixupDoneBra = * + 1
    beq @FixupDone ; yes, double-zeroes -> end loop
                   ; branch destination OVERWRITTEN to skip JumpTracks,
                   ;  after the first time branched
    ; No. Do this fixup
    jsr FixupInc
    lda @FixupTmp+1
    clc
    adc StartPage ; holds the new page location
    sta @FixupTmp+1
    lda (@FixupTmp),y ; fetch original, moved, unfixed byte
    adc StartDiff     ; fix it
    sta (@FixupTmp),y ; and save
    bne @NextFixup ; ALWAYS (assuming fixup locations are sane)
@FixupDone:
    ; Fixups done, but... if we got here, we ourselves are
    ;  running from our loaded ($2000) position, and not our
    ;  moved position (we just moved/fixed-up ourself!)
    ; Jump tracks to make sure we run from the moved position, now
    jsr JumpTracks
    ; ...and fix the @FixupDone branch above so it skips this next time
    lda #(FixupReallyDone - (@FixupDoneBra+1))
    sta @FixupDoneBra
    bne DoModuleInit ; (always) -> skip registering "ourself" as a module.
FixupReallyDone:
    ; Fixups done, if we're running from our own moved position.
    ; Register this module's start page
    ; - First, ensure we have space
    ldx Eot
    cpx #ModulesTableSize
    beq MemErr ; no room -> mem err
    lda StartPage
    sta ModulesTable,x ; save module start page
    inx
    stx Eot
DoModuleInit:
    ; Finally, invoke the module's (fixed-up) init rtn
@InitPtr = ZP::A1
    lda BaaamModuleInit
    sta @InitPtr
    bne @ok
    lda BaaamModuleInit+1
    bne @cont
    rts ; init routine is offset 0 / NUL; don't invoke
@ok:
    lda BaaamModuleInit+1
@cont:
    clc
    adc StartPage
    sta @InitPtr+1
    jmp (@InitPtr)
FixupInc:
    inc ZP::A1L
    bne @skip
    inc ZP::A1H
@skip:
    rts

MemErr:
    ; Throw an uncatchable out-of-memory error.
    lsr ZP::ERRFLG ; throw away any existing ONERR
    jmp ASoft::MEMERR ; quit for out-of-memory

JumpTracks:
    ; Ensure we continue running from our moved location,
    ; if we had been running from our
    ; initially-loaded ($2000) position

    ; Get the stack pointer
    tsx
    inx
    inx
    ; Get our return's high byte
    lda Mon::STACK,x
    clc
    adc StartDiff
    sta Mon::STACK,x ; fix it
    rts              ; ...and then rts to it

BaaamInit:
    ; Install our & handler routine
    lda #$4C ; JMP
    ldx #<Ampersand
    ldy #>Ampersand
    sta ASoft::AMP
    stx ASoft::AMP+1
    sty ASoft::AMP+2
    ; Also, scorch the earth, so a call to &&REG without having loaded a
    ; new module doesn't result in Weird Things
    lda #>BaaamModuleStart
    sta ZP::A1H
    sta ZP::A4H
    clc
    adc #>(BaaamEnd - Baaam)
    sta ZP::A2H
    ;;
    lda #$FE
    sta ZP::A2L
    ldy #1
    sty ZP::A4L
    dey
    sty BaaamModuleStart ; seed the ZERO to copy across the rest of the range
    sty ZP::A1L
    jsr Mon::MOVE
    ; fall through
BaaamEntry:
    rts

Ampersand:
    ;jsr ASoft::CHRGOT ; get the next character after the &
    cmp #$AF ; is it another ampersand? (token byte, NOT char)
    beq HandleOurCommand ; yes -> ball's in our court
    ; Not another ampersand, consume and check registry
    jsr GetBarewordOrStr
    jsr FindAndCallModule
    bcc @rts
    jmp NoModule
@rts:
    rts
HandleOurCommand:
    jsr ASoft::CHRGET ; get the next character after the second &
    jsr GetBarewordOrStr
    lda #<Commands
    sta TmpPtrA
    lda #>Commands
    sta TmpPtrA+1
    lda #>Baaam
    sta StartPage
    jsr FindAndCallHandler
    bcc @rts ; -> succeeded and handled
    jmp NoCommand
@rts:
    rts

FindAndCallModule:
@HandlerTblLocPtr = TmpPtrC
@HandlerTblLoc    = TmpPtrA
    ldx #0
    lda #BaaamOffsetHandleTable
    sta @HandlerTblLocPtr
@lpModules:
    lda ModulesTable,x
    beq @fail
    ; Search the handlers for this module
    sta StartPage
    sta @HandlerTblLocPtr+1
    ldy #0
    lda (@HandlerTblLocPtr),y
    sta @HandlerTblLoc
    iny
    lda (@HandlerTblLocPtr),y
    clc
    adc StartPage
    sta @HandlerTblLoc+1
    txa
    pha
        jsr FindAndCallHandler
    pla
    bcc @rts ; found handler!
    tax
    inx
    bcs @lpModules ; we failed to find in this module; try next
@fail:
    sec
@rts:
    rts

; String to search for expected in ZP::INDEX, with length
;   in StrRemain
; HandlersTable expected in TmpPtrA
FindAndCallHandler:
@SearchScan = TmpPtrB
@SearchStr = ZP::INDEX
@HandlerTbl = TmpPtrA
    ldy #0
@cmdLp:
    lda (@HandlerTbl),y ; strlength
    bne @skipFail
    sec
    rts
@skipFail:
    jsr AdvanceA
    tax
    cpx StrRemain  ; same string length?
    bne @skipHandler   ; no -> try next command
    ; copy string ptr in INDEX to TmpPtrB
    lda @SearchStr
    sta @SearchScan
    lda @SearchStr+1
    sta @SearchScan+1
@cmpLp:
    lda (@HandlerTbl),y
    cmp (@SearchScan),y
    bne @skipHandler
    dex
    beq @handlerFound ; -> strings matched!
    jsr AdvanceA
    jsr AdvanceB
    bne @cmpLp ; always
@skipHandler:
    ; not a match, skip to next cmd
    inx
    inx             ; skip the handler routine size...
    txa
    clc
    adc @HandlerTbl ; ... and jump forward in table past the rest of the string
    sta @HandlerTbl
    bcc @noInc
    inc @HandlerTbl+1
@noInc:
    bne @cmdLp ; always
@handlerFound:
    iny
    ; load/invoke command handler
    lda (@HandlerTbl),y
    sta TmpPtrB         ; we were using this to scan search string, but
    iny
    lda (@HandlerTbl),y ;  don't need for that anymore
    clc
    adc StartPage
    sta TmpPtrB+1
    jsr ASoft::CHRGOT
    jsr CallB
    clc ; indicate success
    rts
AdvanceA:
    inc TmpPtrA
    bne @skip
    inc TmpPtrA+1
@skip:
    rts
AdvanceB:
    inc TmpPtrB
    bne @skip
    inc TmpPtrB+1
@skip:
    rts
CallB:
    jmp (TmpPtrB)

NoCommand:
    lda #<NoCmdMsg
    ldy #>NoCmdMsg
    jsr ASoft::STROUT
    jmp ASoft::SYNERR

NoModule:
    lda #<NoModMsg
    ldy #>NoModMsg
    jsr ASoft::STROUT
    jmp ASoft::SYNERR

; Scan TXTPTR and either treat a bareword as if it were a string,
; fixing it up appropriately, or else evaluate the actual string and
; use that.
GetBarewordOrStr:
    jsr ASoft::CHRGOT
    jsr IsLetter
    bcc @handleExpr ; -> not a letter, assume it's a literal or formula
                    ; (and let BASIC handle the err if it's not)
    ; It was a letter. Scan through and see if it's a legal
    ; "bareword" string.
    ldy #1
@scan:
    lda (ZP::TXTPTR),y
    jsr IsBarewordChar
    bcc @nonBwFnd
    iny
    bne @scan
@nonBwFnd:
    cmp #':'
    beq @yesBarewd
    cmp #','
    beq @yesBarewd
    cmp #0
    beq @yesBarewd
@handleExpr:
    jsr ASoft::FRMEVL ; evaluate the string expr
    jsr ASoft::FREFAC ; free the string tmp
    sta StrRemain
    rts
@yesBarewd:
    sty StrRemain
    lda ZP::TXTPTR
    sta ZP::INDEX
    lda ZP::TXTPTR+1
    sta ZP::INDEX+1
    ; But also skip TXTPTR forward, now
    tya
    clc
    adc ZP::TXTPTR
    sta ZP::TXTPTR
    bcc @skipHi
    inc ZP::TXTPTR+1
@skipHi:
    rts

StrRemain:
    .byte $0

IsBarewordChar:
    jsr IsLetter
    bcs @rts
    cmp #'_'
    beq @rts
    cmp #('9' + 1)
    bcs @clc
    cmp #'0'
    bcs @rts
    cmp #'-'
    beq @rts
@clc:
    clc
@rts:
    rts

; Exits with carry set if A contains a letter. Leaves A, X, Y alone.
IsLetter:
    pha
        and #$5F    ; strip high bit, and convert lowercase to upper
                    ; (will also convert digits and many punct to
                    ; control chars, but that doesn't affect our
                    ; letter-only tests)
        cmp #'A'
        bcc @rts    ; lower than 'A'; not a letter. -> exit failure
        cmp #('Z'+1)
        bcs @clc    ; higher than 'Z'; not a letter. -> exit failure
        ; Congrats, we're a letter
        sec
        bcs @rts
@clc:
        clc
@rts:
    pla
    rts

BaaamEnd:
