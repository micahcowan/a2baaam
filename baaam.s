.import BaaamFixup
.import SystemClear, SystemAlloc

.macpack apple2

.include "baaam.inc"
;A2FIRM_NO_NAMESPACES = 1
.include "a2firm.inc"

StartPage = $6
StartDiff = $7

.segment "START"
PreBaaam:
    jmp BaaamRun
Baaam:
BaaamHeader:
    _BaaamHeader AmpersandTag, Baaam, BaaamEnd, BaaamFixup, BaaamInit, BaaamEntry
    .include "baaam-version.inc"

.code
AmpersandTag:
    .byte 1
    scrcode "&"

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
    sta ZP::A2H
    ; minus 1 (last byte is on prev page)
    dec ZP::A2H
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
    ; Finally, invoke the module's (fixed-up) init rtn
@EntryPtr = ZP::A1
    lda BaaamModuleInit
    sta @EntryPtr
    lda BaaamModuleInit+1
    clc
    adc StartPage
    sta @EntryPtr+1
    jmp (@EntryPtr)
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
    ; fall through
BaaamEntry:
    rts

Ampersand:
    jsr ASoft::CHRGET ; get the next character after the &
    cmp #$AF ; is it another ampersand? (token byte, NOT char)
    beq @ours ; yes -> ball's in our court
    ; XXX not another ampersand, consume and check registry
@ours:
    jsr GetHandle
    ; XXX jsr FindInRegistry
    rts

; Scan TXTPTR and either treat a bareword as if it were a string,
; fixing it up appropriately, or else evaluate the actual string and
; use that.
GetHandle:
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
    sta ZP::TXTPTR+1
    sta ZP::INDEX+1
    rts

StrRemain:
    .byte $0

IsBarewordChar:
    ; XXX
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
