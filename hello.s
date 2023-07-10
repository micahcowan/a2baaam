.import HelloFixup

.include "baaam.inc"
.include "a2firm.inc"

HelloStart:
HelloHeader:
    _BaaamHeader HelloTags, HelloStart, HelloEnd, HelloFixup, HelloInit
    .word 1, 0, 0
    .byte 0, 1, 2, 3

HelloTags:
    _BaaamHandler "HELLO", HelloStart, HelloEntry
    .byte $0

HelloInit = 0

HelloEntry:
    cmp #','
    bne @noSkip
    jsr ASoft::CHRGET
@noSkip:
    jsr ASoft::FRMEVL
    jsr ASoft::FRESTR
    ldy #$A0
    sty CharOffset
    ldy #0
    tax
@lp:
    lda CharOffset
    eor #$20
    sta CharOffset
    clc
    adc (ZP::INDEX),y
    jsr Mon::COUT
    iny
    dex
    bne @lp
    jmp Mon::CROUT
CharOffset:
    .byte $82

HelloEnd:
