; baaam.inc

; BAAAM modules should include this, for setting up module headers
.ifndef scrcode
    ;.error ".macpack apple2  must be specified before including baaam.inc"
.endif

; Sets up a module header, containing everything EXCEPT the module
; version. Your module is expected to immediately follow this with
; ten bytes of version info: major, minor, patchlevel; two
; (little-endian) bytes for each, followed by 4 commit/build ID bytes.
.macro _BaaamHeader handleStr, size, fixupTbl, initSub, entrySub
    .byte $00 ; Baaam header version
    .byte size ; number of pages this module takes up
    .word fixupTbl
    .word initSub
    .word :+
    .word entrySub

:
    .byte .strlen(handleStr)
    scrcode handleStr
.endmacro

; constants for where to find what in the header
BaaamModuleStart = $2000
BaaamHeaderVersion = BaaamModuleStart
BaaamOffsetSize = 1
BaaamOffsetFixup = BaaamOffsetSize + 1
BaaamOffsetInit = BaaamOffsetFixup + 2
BaaamOffsetHandle = BaaamOffsetInit + 2
BaaamOffsetEntry = BaaamOffsetHandle + 2

BaaamModuleSize = BaaamModuleStart + BaaamOffsetSize
BaaamModuleFixup = BaaamModuleStart + BaaamOffsetFixup
BaaamModuleInit = BaaamModuleStart + BaaamOffsetInit
BaaamModuleHandle = BaaamModuleStart + BaaamOffsetHandle
BaaamModuleEntry = BaaamModuleStart + BaaamOffsetEntry
