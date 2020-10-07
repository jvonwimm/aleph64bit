$!========================================================================
$!
$! Name      : DBSLIB:[EXEC]DBSCOM.COM
$!
$! Purpose   : run any PROG program of DBSLIB
$!             the program is assumed to read data cards on PROG.CARDS
$!
$! Arguments :
$!   P1 = program name : EPTODA, DAFCAR, SBANK, CHKCRD, CARDAF, EPIOFMT, UPDDAF
$!        OBJ            link, execute
$!        EXE                  execute
$!   P3   DEB   execute in debug mode
$!        NODEB execute in nodebug mode
$!
$! Created  15-MAR-1990    F.Ranjard
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$!
$ ASS 'P1'.CARDS FOR007
$ if P2 .eqs. "EXE" then goto REXE
$ if P2 .eqs. "OBJ" then goto ROBJ
$ROBJ:
$ CERNLIB
$ LINK/'P3' -
  DBSLIB:DBASE_D/LIB/INCLUDE=('P1'),-
  ALE:ALEPHLIB_D/LIB/INCLUDE=(BABEND),-
  ALE:BOS77_D/LIB/INCLUDE=(MDARD,DAFRDS),-
  'LIB$'
$REXE:
$ ASS SYS$COMMAND SYS$INPUT
$ RUN/'P3' 'P1'
$EXIT:
$ DEASS FOR006
$ DEASS FOR007
$ EXIT

