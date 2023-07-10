; baaam.inc
;
; BAAAM modules should include this, for setting up module headers

; constants for where to find what in the header
BaaamModuleStart = $2000
BaaamHeaderVersion = BaaamModuleStart
BaaamOffsetSize = 1
BaaamOffsetFixup = BaaamOffsetSize + 1
BaaamOffsetInit = BaaamOffsetFixup + 2
BaaamOffsetHandleTable = BaaamOffsetInit + 2

BaaamModuleSize = BaaamModuleStart + BaaamOffsetSize
BaaamModuleFixup = BaaamModuleStart + BaaamOffsetFixup
BaaamModuleInit = BaaamModuleStart + BaaamOffsetInit
BaaamModuleHandleTable = BaaamModuleStart + BaaamOffsetHandleTable

; Sets up a module header, containing everything EXCEPT the module
; version. Your module is expected to immediately follow this with
; ten bytes of version info: major, minor, patchlevel; two
; (little-endian) bytes for each, followed by 4 commit/build ID bytes.
.macro _BaaamHeader handleTablePtr, moduleStart, moduleEnd, fixupTbl, initSub
    .byte $00 ; Baaam header version
    .byte (>moduleEnd - >moduleStart + 1) ; number of pages this module takes up
    .word fixupTbl - moduleStart
    ; If initSub is 0, keep it at 0
    .word (!!initSub) * (initSub - moduleStart)
    .word handleTablePtr - moduleStart
.endmacro

.macro _BaaamPStr str
    .byte .strlen(str)
    .byte str
.endmacro

.macro _BaaamHandler str, start, offset
    _BaaamPStr str
    .word offset - start
.endmacro
