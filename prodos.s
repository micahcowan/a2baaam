.export SystemClear, SystemAlloc, SystemDosCmd, SystemErrOut

SystemClear  = $BEF8 ; FREBUFR
SystemAlloc  = $BEF5 ; GETBUFR
SystemDosCmd = $BE03 ; DOSCMD
SystemErrOut = $BE09 ; ERROUT
