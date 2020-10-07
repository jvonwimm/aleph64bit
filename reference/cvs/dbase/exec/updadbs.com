$!========================================================================
$!
$! Name      : AL1$USER0:[RANJARD.DBASE]UPDATE_ADBS.COM
$!
$! Purpose   : make a new ADBSCONS.DAF from the previous one
$!             start from ADAMO.BS created on VXCERN and copied
$!             onto DBASE:RUNBANK.BS when ADBSRUN.DDL+BANKxx.DDL
$!             were updated.
$!             onto DBASE:ADBSCONS.BS when ADBSCONS.DDL was
$!             updated.
$!
$! Arguments : P1 is the directory where the new FULLDAF resides
$!             P2 is the version number  of the new data base
$!
$! Created   2-APR-1990
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$!
$! execute one of the following program
$! RUNBANK.BS contains .TAB and .COL which must overwrite TAB2 and COL2
$! of the current FULL data base.
$! ADBSCONS.BS contains .TAB and .COL which must overwrite .TAB and .COL
$! of the current FULL data base.
$! create a NEWADBS.DAF
$! copy all banks  except TAB2 and COL2 from current database to NEWADBS
$! read .TAB and .COL from RUNBANK.BS
$! swap them with TAB2 and COL2 respectively
$! write new COL2 and TAB2 onto NEWADBS
$! read .TAB and .COL from ADBSCONS.BS
$! replace them onto NEWADBS
$! there is a data card file to describe input and output files NEWDAF.CARDS
$!
$ @DBSCOM NEWDAF EXE NODEB
$!
$! the NEWADBS.DAF is the old one with new ADAMO tables.
$! CARDAF must be used in order to get a new version if there are changes
$! into the banks.
$! there is a data card file CARDAF.CARDS
$!
$ @DBSCOM CARDAF EXE NODEB
$!
$! write an EPIO file of the FULLDAF
$! use a data card file SPLITDA.CARDS to define the input and output files
$!
$ @DBSCOM SPLITDA EXE NODEB
$!
$! write a TESTDAF without the ADAMO tables .TAB,.COL,.CHK,.DIR
$! use a data card file EPTODA.CARDS to define input and output files
$!
$ @DBSCOM EPTODA EXE NODEB
$!
$EXIT:
$   EXIT


