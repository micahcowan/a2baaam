;; a2-firm.inc
;;   entry points, soft switches, and ZP var locations
;;   from the Apple ][ monitor/AppleSoft

; If you want names to be dumped directly into the global namespace,
;  then define A2FIRM_NO_NAMESPACES

; Names are taken from (in order:)
;   Apple II and II+ Reference Manual
;   AppleSoft Internal Entry Points, article in
;       The Apple Orchard magazine, first issue
;   https://6502disassembly.com/a2-rom/ 

.ifndef A2FIRM_NO_NAMESPACES
.scope ZP
.endif
CHARAC  = $0D ; AppleSoft
ENDCHR  = $0E ; AppleSoft
VALTYP  = $11 ; AppleSoft
SUBFLG  = $14 ; $00 subscripts allowed, $80=no subscripts
WNDLFT  = $20
WNDWDTH = $21
WNDTOP  = $22
WNDBTM  = $23
CH      = $24
CV      = $25
GBASL   = $26
GBAS    = GBASL
GBASH   = $27
BASL    = $28
BAS     = BASL
BASH    = $29
BAS2L   = $2A
BAS2H   = $2B
H2      = $2C ; Used by PLOT rtns
V2      = $2D ; Used by PLOT rtns
COLOR   = $30
MODE    = $31
INVFLG  = $32
PROMPT  = $33
CSWL    = $36
CSW     = CSWL
CSWH    = $37
KSWL    = $38
KSW     = KSWL
KSWH    = $39
A1L     = $3C
A1H     = $3D
A2L     = $3E
A2H     = $3F
A3L     = $40
A3H     = $41
A4L     = $42
A4H     = $43
A1      = A1L
A2      = A2L
A3      = A3L
A4      = A4L
RNDL    = $4E
RND     = RNDL
RNDH    = $4F
; The following are all AppleSoft-used locations
LINNUM  = $50
TEMPPT  = $52
LASTPT  = $53
INDEX   = $5E
DEST    = $60
TXTTAB  = $67
VARTAB  = $69
ARYTAB  = $6B
STREND  = $6D
FRETOP  = $6F
FRESPC  = $71
MEMSIZ  = $73
MEMSIZE = MEMSIZ
CURLIN  = $75
OLDLIN  = $77
DATLIN  = $7B
DATPTR  = $7D
VARPNT  = $83
DSCTMP  = $9D
FAC     = $9D
FORPNT  = $85
TEMP3   = $8A
TEMP1   = $9e
HIGHDS  = $94
HIGHTR  = $96
TEMP2   = $98
LOWTR   = $9B
ARG     = $A5
STRNG1  = $AB
STRING1 = STRNG1
STRNG2  = $AD
STRING2 = STRNG2
PRGEND  = $AF
TXTPTR  = $B8
RNDFP   = $C9 ; name is mine; AppleSoft Internal Entry Points calls this RND, but I'm using that for RNDL
ERRFLG  = $D8
ERRLIN  = $DA
ERRNUM  = $DE
ERRPOS  = $DC ; TXTPTR save for HNDLERR
ERRSTK  = $DF
HPAG    = $E6
FIRST   = $F0 ; Used by PLOT rtns
SPDBYT  = $F1 ; Speed = delay number
ORMASK  = $F3 ; Mask for flashing output
REMSTK  = $F8 ; Stack ptr saved before each stm
.ifndef A2FIRM_NO_NAMESPACES
.endscope
.endif

.ifndef A2FIRM_NO_NAMESPACES
.scope ASoft
.endif
;;; AppleSoft constant values
; Error codes
ERR_NOFOR       = $00
ERR_SYNTAX      = $10
ERR_NOGOSUB     = $16
ERR_NODATA      = $2A
ERR_ILLQTY      = $35
ERR_OVERFLOW    = $45
ERR_MEMFULL     = $4D
ERR_UNDEFSTAT   = $5A
ERR_BADSUBS     = $6B
ERR_REDIMD      = $78
ERR_ZERODIV     = $85
ERR_ILLDIR      = $95
ERR_BADTYPE     = $A3
ERR_STRLONG     = $B0
ERR_FRMCPX      = $BF
ERR_CANTCONT    = $D2
ERR_UNDEFFUNC   = $E0

CHRGET  = $B1 ; zero page routine
CHRGOT  = $B7 ; zero page routine

AMP          = $03F5
BLTU         = $D393
REASON       = $D3E3
MEMERR       = $D410
ERROR        = $D412
ERROR2       = $D419 ; Name mine. Handle error, ignoring ONERR
INLIN        = $D52C
INLIN2       = $D52E
GDBUFS       = $D539
INCHR        = $D553
RUN          = $D566
FNDLIN       = $D61A
SCRTCH       = $D64B
CLEARC       = $D66C
STKINI       = $D683
STXTPT       = $D697
NEWSTT       = $D7D2
RESTOR       = $D849
ISCNTC       = $D858
CONT         = $D898
SAVE         = $D8B0
LOAD         = $D8C9
VARTIO       = $D8F0
PROGIO       = $D901
GOTO         = $D93E
UNDERR       = $D97C ; undefined statement ERROR
DATA         = $D995
ADDON        = $D998
DATAN        = $D9A3
REMN         = $D9A6
LINGET       = $DA0C
LET          = $DA46
COPY         = $DAB7
CRDO         = $DAFB
STROUT       = $DB3A
STRPRT       = $DB3D
OUTSPC       = $DB57
OUTQST       = $DB5A
OUTDO        = $DB5C
FRMNUM       = $DD67
CHKNUM       = $DD6A
CHKSTR       = $DD6C
CHKVAL       = $DD6D
FRMEVL       = $DD7B
STRTXT       = $DE81
PARCHK       = $DEB2
CHKCLS       = $DEB8
CHKOPN       = $DEBB
CHKCOM       = $DEBE
SYNCHR       = $DEC0
SYNERR       = $DEC9
PTRGET       = $DFE3
BASIC        = $E000
BASIC2       = $E003
ISLETC       = $E07D
AYINT        = $E10C
SUBERR       = $E196 ; illegal subscript error
IQERR        = $E199 ; illegal quantity error
GIVAYF       = $E2F2 ; Float the signed integer in A,Y
SNGFLT       = $E301 ; Float the unsigned integer in Y
ERRDIR       = $E306 ; throw an error iff in direct mode
UNDFNC       = $E30E ; undefined function error
STRINI       = $E3D5
STRSPA       = $E3DD
STRLIT       = $E3E7
STRLT2       = $E3ED
PUTNEW       = $E42A
GETSPA       = $E452
GARBAG       = $E484
CAT          = $E597
MOVINS       = $E5D4
MOVSTR       = $E5E2
FRESTR       = $E5FD
FREFAC       = $E600
FRETMP       = $E604
GTBYTC       = $E6F5
GETBYT       = $E6F8
CONINT       = $E6FB
VALIDX       = $E70F
GETNUM       = $E746
COMBYTE      = $E74C
GETADR       = $E752
FADDH        = $E7A0 ; Add 1/2 to FAC
FSUB         = $E7A7
FSUBT        = $E7AA
FADD         = $E7BE
FADDT        = $E7C1
OVERFLOW     = $E8D5
LOG          = $E941
FMULT        = $E97F
FMULTT       = $E982
CONUPK       = $E9E3 ; AppleSoft Internals name
LOAD_ARG_FROM_YA = $E9E3 ; disassembly project name
MUL10        = $EA39
DIV10        = $EA55
FDIV         = $EA66
FDIVT        = $EA69
ZDIVERR      = $EAE1 ; zero-divison error. Name is mine.
MOVFM        = $EAF9 ; AppleSoft Internals name
LOAD_FAC_FROM_YA = $EAF9 ; disassembly project name
MOV2F        = $EB1E
MOV1F        = $EB21
MOVML        = $EB23
MOVMF        = $EB2B
MOVFA        = $EB53
MOVAF        = $EB63
SGN          = $EB80
SIGN         = $EB82
ABS          = $EBAF
FLOAT        = $EB93 ; Float the signed integer in A.
FCOMP        = $EBB2
QINT         = $EBF2
INT          = $EC23
FIN          = $EC4A
LINPRT       = $ED24
PRTFAC       = $ED2E ; AppleSoft Internals name
PRINT_FAC    = $ED2E ;  disassembly project name
FOUT         = $ED34
SQR          = $EE8D
FPWRT        = $EE97
NEGOP        = $EED0 ; FAC = -FAC
EXP          = $EF09
COS          = $EFEA
SIN          = $EFF1
TAN          = $F03A
ATN          = $F09E
PLOTFNS      = $F1EC
HANDLERR     = $F2E9
RESUME       = $F317
HGR2         = $F3D4
HGR          = $F3DE
HCLR         = $F3EE
BKGND        = $F3F2
HPOSN        = $F40D
HPLOT        = $F453
HLIN         = $F530
HFIND        = $F5CB
DRAW         = $F601
XDRAW        = $F65D
HFNS         = $F6B9
GETARYPT     = $F6D9
SETHCOL      = $F6EC
SHLOAD       = $F775

; packed-form floating-point constants. Names mine.
NUM_QTR         = $F070
NUM_HALF        = $EE64
NUM_MHALF       = $E937 ; -1/2
NUM_ONE         = $E913
NUM_TEN         = $EA50
NUM_SQRHALF     = $E92D ; SQR(.5)
NUM_SQRTWO      = $E932 ; SQR(2)
NUM_LN2         = $E93C ; LN(2)
NUM_LOGE2       = $EEDB
NUM_HALFPI      = $F063
NUM_TAU         = $F06B
NUM_MWORD       = $E0FE ; -32768
NUM_THOUMIL     = $ED14 ; 1,000,000,000
.ifndef A2FIRM_NO_NAMESPACES
.endscope
.endif

.ifndef A2FIRM_NO_NAMESPACES
.scope Mon
.endif
; Non-ZP locations
STACK   = $100
IN      = $200   ; what original Apple ][ monitor listing in techref calls it
BUF     = IN     ; what AppleSoft Internal Entry Points (from the Apple Orchard) calls it
BRKV    = $3F0
SOFTEV  = $3F2
PWREDUP = $3F4
POWEREDUP = PWREDUP
AMPERV  = $3F5
USRADR  = $3FB
NMI     = $3FB
IRQLOC  = $3FE
LINE1   = $400

; Entry points
PLOT    = $F800
HLINE   = $F819
VLINE   = $F828
CLRSCR  = $F832
GBASCALC= $F847
GBCALC  = $F856
SETCOL  = $F864
SCRN    = $F871
PRNTYX  = $F940
PRNTAX  = $F941
PRNTX   = $F944
PRBLNK  = $F94B ; prints 3 spaces
BREAK   = $FA4C
OLDBRK  = $FA59
RESET   = $FA62
PWRUP   = $FAA6
PREAD   = $FB1E
SETTXT  = $FB39
SETGR   = $FB40
SETWND  = $FB4B
TABV    = $FB5B ; STA CV followed by JMP VTAB
BASCALC = $FBC1
BELL1   = $FBD9
VTAB    = $FC22
VTABZ   = $FC24
HOME    = $FC58
SCROLL  = $FC70
CLREOL  = $FC9C
CLEOLZ  = $FC9E
WAIT    = $FCA8
RD2BIT  = $FCFA
RDBIT   = $FCFD
RDKEY   = $FD0C
KEYIN   = $FD1B
GETLN   = $FD6A
CROUT   = $FD8E
PRBYTE  = $FDDA
PRHEX   = $FDE3
COUT    = $FDED
COUT1   = $FDF0
SETMODE = $FE18
SETMDZ  = $FE1D
MOVE    = $FE2C
VFY     = $FE36
SETINV  = $FE80
SETNORM = $FE84
SETKBD  = $FE89
INPORT  = $FE8B
SETVID  = $FE93
OUTPORT = $FE95
WRITE   = $FECD
READ    = $FEFD
READ2   = $FF02
MONZ    = $FF69
BELL    = $FF3A
.ifndef A2FIRM_NO_NAMESPACES
.endscope
.endif

.ifndef A2FIRM_NO_NAMESPACES
.scope Switch
.endif
KBD             = $C000
KBDSTRB         = $C010
TAPEOUT         = $C020
SPKR            = $C030
TXTCLR          = $C050
TXTSET          = $C051
MIXCLR          = $C052
MIXSET          = $C053
LOWSCR          = $C054
SEL_TEXT_P1     = LOWSCR
HISCR           = $C055
SEL_TEXT_P2     = HISCR
LORES           = $C056
HIRES           = $C057
SETAN0          = $C058
CLRAN0          = $C059
SETAN1          = $C05A
CLRAN1          = $C05B
SETAN2          = $C05C
CLRAN2          = $C05D
SETAN3          = $C05E
CLRAN3          = $C05F
TAPEIN          = $C060
PADDL0          = $C064
PTRIG           = $C070
.ifndef A2FIRM_NO_NAMESPACES
.endscope
.endif
