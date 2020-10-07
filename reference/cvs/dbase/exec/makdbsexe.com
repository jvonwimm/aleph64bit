$!========================================================================
$!
$! Name      : DOC:MAKDBSEXE.COM
$!
$! Purpose   : make a .EXE for any program from DBSLIB library
$!
$! Arguments : P1 = program name : EPIOFMT, EPTODA,
$!                                 CARDAF , CHKCRD, DAFCAR,
$!                                 SBANK  , CREIND, HACPAR.
$!             P2 = DEB or NODEB
$!             P3 = additional .FOR or .OBJ
$!
$! Created   3-MAY-1990   
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$  ON CONTROL_Y THEN $ GOTO EXIT
$ CERNLIB GENLIB
$ if P2 .eqs. "" then P2 = "NODEB"
$ ext = ""
$ if P2 .eqs. "DEB" then ext = "_D"
$RFOR: 
$ if P3 .eqs. "" then goto nof
$ FOR/DEB/NOOPT 'P3'
$nof:
$ROBJ:
$ if P3 .eqs. "" then goto nol
$ LINK/'P2'/EXE=DOC:'P1' 'P3',-
  DBSLIB:[000000]DBASE'ext'/LIB/INCLUDE=('P1',DAFRDS,DELAYP),-
  ALE:ALEPHLIB'ext'/LIB/INCLUDE=(BABEND),-
  ALE:BOS77'ext'/LIB/INCLUDE=(MDARD),-
  'LIB$'
$ goto EXIT
$nol:
$ LINK/'P2'/EXE=DOC:'P1' -
  DBSLIB:[000000]DBASE'ext'/LIB/INCLUDE=('P1',DAFRDS,DELAYP),-
  ALE:ALEPHLIB'ext'/LIB/INCLUDE=(BABEND),-
  ALE:BOS77'ext'/LIB/INCLUDE=(MDARD),-
  'LIB$'
$EXIT:
$   EXIT
