*DK agdimp
      SUBROUTINE AGDIMP(VOLNA,LVLVO,DETNA)
C--------------------------------------------------------------------
C      O.Callot     11-jun-86
C
C! Add the volume named 'VOLNA' at level LVLVO to the list used
C! to generate the IMPA bank, with detector name DETNA.
C
C.  Calls  INTCHA                                      BOS lib
C.  Called by various AGxxxx routines
C---------------------------------------------------------------------
#ifndef DOC
#include "iocom.h"
#include "agcons.h"
      CHARACTER*4 VOLNA,DETNA
      EXTERNAL INTCHA , CHAINT
#include "jqcom.h"
      CHARACTER*4 TDETN, CHAINT
      IDETN (TDETN) = IUCOMP (INTCHA(TDETN), IW(IW(NAIMPA)+1),
     &                          IW(IW(NAIMPA)))
C --------------------------------------------------------------------
C - if NO IMPA data card RETURN
      IF (IW(NAIMPA) .EQ. 0) RETURN
C
C - if IMPA is NOT required for this detector then RETURN
      IF (IDETN(DETNA) .EQ. 0) RETURN
C
C - store the volume name into the list
C
      IF(NAGIMP.EQ.LIMVOL) GO TO 980
      NAGIMP = NAGIMP + 1
      LAGIMP(1,NAGIMP) = INTCHA(VOLNA)
      LAGIMP(2,NAGIMP) = LVLVO
      LAGIMP(3,NAGIMP) = INTCHA(DETNA)
      RETURN
C
  980 WRITE(LOUTIO,1000) VOLNA,DETNA,NAGIMP,(CHAINT(LAGIMP(1,J)),
     &                   CHAINT(LAGIMP(2,J)),CHAINT(LAGIMP(3,J)),
     &                   J=1,NAGIMP)
 1000 FORMAT(/' +++ AGDIMP +++ called for volume ',A4,' and detector ',
     +       A4,'  max.number',I6,' exceded. List follows.'/
     +       15(10X,A4,I6,2X,A4/))
      RETURN
      END
#endif
