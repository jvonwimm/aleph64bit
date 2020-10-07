$!========================================================================
$!
$! Name      : AL1$USER0:[RANJARD.DBASE]UPDATE_NEWADBS.COM
$!
$! Purpose   : make a new ADBSCONS.DAF from the previous one
$!             start from ADAMO.BS created on VXCERN and copied:
$!             - onto DBASE:RUNBANK.BS when ADBSRUN.DDL+BANKxx.DDL
$!               were updated.
$!             - onto DBASE:ADBSCONS.BS when ADBSCONS.DDL was
$!               updated.
$!
$! Arguments : P1 is the directory where the new data base resides
$!             P2 is the filename of the new data base
$!
$! Created   2-MAY-1990
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$!
$! RUNBANK.BS contains .TAB and .COL which will be combined to create
$! TPRO and CPRO banks.
$! ADBSCONS.BS contains .TAB and .COL which will be combined to create
$! TADB and CADB banks.
$! create a NEWADBS.DAF which contains the new TPRO,CPRO,TADB,CADB banks.
$! copy all banks  except TAB2, COL2, .TAB and .COL from current database
$! to NEWADBS.
$! there is a data card file to describe input and output files NEWDAF.CARDS
$!
$ @DBSCOM UPDDAF EXE NODEB
$!
$! the NEWADBS.DAF is the old one with new ADAMO tables.
$! CARDAF must be used in order to get a new version if there are changes
$! into the banks.
$! there is a data card file CARDAF.CARDS
$!
$ @DBSCOM CARDAF EXE NODEB
$!
$! write an EPIO file of the NEWADBS
$!
$ @DBSCOM EPIOFMT EXE NODEB
$!
$EXIT:
$   EXIT
