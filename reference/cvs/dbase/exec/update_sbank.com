$!========================================================================
$!
$! Name      : AL1$USER0:[RANJARD.DBASE]UPDATE_SBANK.COM
$!
$! Purpose   : create ADD, LBF, HAC for all BANKxx.DDL
$!             prepare SBANK
$!
$! Arguments :
$!   P1 = CSP   means execute CSP,HAC,LBF,copy to IBM,SBANK on VAX
$!      = HAC         execute     HAC,LBF,copy to IBM,SBANK on VAX
$!      = LBF         execute         LBF,copy to IBM,SBANK on VAX
$!      = VAX         execute                         SBANK on VAX
$!   P2 = NEW   means change the default directory to BANKDOC_NEW
$!              The default directory is BANKDOC
$!
$! Created   20-OCT-1988    J.Knobloch
$! Modified  15-MAR-1990    F.Ranjard
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$!
$ if P1 .eqs. "VAX" then goto RVAX
$!
$ IF (f$logical("adamo$root").eqs."") THEN ADAMO
$!
$! set the default directory
$ DEF BKDOCDIR ALEPH$GENERAL:[DOC.BANKDOC]
$ if P2 .eqs. "NEW" then DEF BKDOCDIR ALEPH$GENERAL:[DOC.BANKDOC_NEW]
$!
$ if P1 .eqs. "CSP" then goto RCSP
$ if P1 .eqs. "LBF" then goto RLBF
$ if P1 .eqs. "HAC" then goto RHAC
$!
$! create BANKAL.ADD from all .DDLs residing on BKDOCDIR
$RCSP:
$ CSP/LOG BANKDOC:GENATTR,BKDOCDIR:ADBSRUN.DDL,BKDOCDIR:BANK*.DDL -
  BKDOCDIR:RUNBANK/CREATE/ADD_SIZE=2000000
$ CSP/LOG BANKDOC:GENATTR,BKDOCDIR:ADBSCONS.DDL -
  BKDOCDIR:BANKAL/CREATE/ADD_SIZE=2000000
$!
$! create BANKAL.HAC  hac parameters
$RHAC:
$ HAC/LOG BKDOCDIR:ADBSCONS
$ HAC/LOG BKDOCDIR:RUNBANK
$ COPY ADBSCONS.HAC,RUNBANK.HAC BKDOCDIR:BANKAL.HAC
$!
$! create BANKAL.LBF bank listing and SBANK.LBF
$RLBF:
$ LBF/LINES=99999 BKDOCDIR:ADBSCONS
$ LBF/LINES=99999 BKDOCDIR:RUNBANK
$ copy ADBSCONS.LBF,RUNBANK.LBF,DOC:NODDL.LBF DOC:SBANK.LBF
$!
$! recreate relations between programs and sbank
$! should be run everytime there is a new version of a
$! program or of the database documentation.
$RVAX:
$ set def doc
$ R DOC:CREATE_SBANK_INDEX/NODEB
$ R DOC:CREATE_SBANK_HAC/NODEB
$EXIT:
$ HOME
$ EXIT
