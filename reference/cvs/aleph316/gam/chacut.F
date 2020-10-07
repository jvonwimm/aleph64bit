      SUBROUTINE CHACUT(PARC,VALP)
C.----------------------------------------------------------------------
CKEY GAMPACK STORE/ USER
C   J.C.Brient      Creation  1/10/91
C! Store in right format the storeys info. of photon for stack KSTAG
C   Input :
C           PARC    Name of the parameter          CHARACTER*4
C           VALP    new value of the param. PARC   REAL
C
C
C   PARC could be
C               'EZSU'      hard and soft min.storey energy(30 MeV PECO)
C               'DIST'      min. dist. ch.track   stack 1
C               'ECL1'      start cluster stack 1
C               'ECL2'        "      "      "   2
C               'ECL3'        "      "      "   3
C               'ECLU'      min energy st1+st2 for gamma
C               'OK12'      min number of storey stack 1 for gamma
C               'OK23'      min   "         "      "   2  "    "
C
C   Output:
C   Calls: None
C   Called by GAMPEX or USER
C.----------------------------------------------------------------------
#ifndef DOC
#include "comcux.h"
      CHARACTER*(*)  PARC
C
      IF(PARC .EQ. 'EZSU') RECPAR(1) = VALP
      IF(PARC .EQ. 'ECL1') RECPAR(2) = VALP
      IF(PARC .EQ. 'ECL2') RECPAR(3) = VALP
      IF(PARC .EQ. 'ECL3') RECPAR(4) = VALP
      IF(PARC .EQ. 'ECLU') RECPAR(5) = VALP
      IF(PARC .EQ. 'DIST') RECPAR(6) = VALP
      IF(PARC .EQ. 'OK12') RECPAR(7) = VALP
      IF(PARC .EQ. 'OK23') RECPAR(8) = VALP
      RETURN
      END
#endif
