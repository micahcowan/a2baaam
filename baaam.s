.import BaaamFixup
.import SystemStart, SystemClear, SystemAlloc

.macpack apple2

.include "baaam.inc"
;A2FIRM_NO_NAMESPACES = 1
.include "a2firm.inc"

Baaam:
BaaamHeader:
    _BaaamHeader "&", (>BaaamEnd - >Baaam + 1), BaaamFixup, BaaamInit, BaaamEntry
    .include "baaam-version.inc"

BaaamEot:
    .byte $0 ; current end of modules table
BaaamModules:
    .res $100

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
    bcs @err
    ; Allocation succeeded.
    ; New start page is in acc, set it as copy destination
    sta ZP::A4H
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
    sty ZP::A2L
    sty ZP::A4L
    jsr Mon::MOVE
    rts
@err:
    ; Throw an uncatchable out-of-memory error.
    lsr ZP::ERRFLG ; throw away any existing ONERR
    jmp ASoft::MEMERR ; quit for out-of-memory

BaaamInit:
    jsr SystemClear
    ; Now allocate space for (protect) ourself
    ; (also the start of general "
    lda #(<BaaamEnd - >Baaam + 1)
    jsr SystemAlloc

BaaamEntry:
    rts

BaaamInterface:
    jmp RegisterModule
    jmp BaaamRun
BaaamEnd:
