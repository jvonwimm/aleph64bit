$!========================================================================
$!
$! Name      : AL1$USER0:[RANJARD.DBASE]BANKAL.COM
$!
$! Purpose   : create ADD, LBF, HAC, FMT for all .DDL from BKNEW:
$!             prepare SBANK
$!             build BANKAL.*
$!
$! Arguments :
$!
$! Created   20-OCT-1988    J.Knobloch
$! Modified  15-MAR-1990    F.Ranjard
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$!
$ IF (f$logical("adamo$root").eqs."") THEN ADAMO
$ SET DEF ALEPH$GENERAL:[DOC.BANKDOC_NEW]
$ DEF BKDOCDIR ALEPH$GENERAL:[DOC.BANKDOC_NEW]
$! create data dictionnary on BKDOCDIR:BANKAL.ADD
$ CSP/LOG BKDOCDIR:*.DDL,-
  BANKAL/CREATE/ADD_SIZE=2000000
$! create LBF file on BKDOCDIR:BANKAL.LBF
$ LBF BANKAL
$! create HAC parameters on BKDOCDIR:BANKAL.HAC
$ HAC/LOG BANKAL
$! create bank formats on BKDOCDIR:BANKAL.FMT
$ RUN BANKDOC:CRTFMT/NODEB
$EXIT:
$ HOME
$ EXIT
#endif
