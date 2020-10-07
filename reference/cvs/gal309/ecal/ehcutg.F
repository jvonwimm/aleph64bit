*DK ehcutg
      SUBROUTINE EHCUTG (MFLAG)
C --------------------------------------------------------------
C - J.Badier  F.Ranjard - 870817
C! decide wheither Yes/No a geantino is created
C - MFLAG = 0 no creation
C           1 creation
C - called from   ECHIT                           from this .HLB
#ifndef DOC
C --------------------------------------------------
      SAVE
#include "ehpash.h"
#include "tmacrod.h"
#include "tmacrof.h"
C ---------------------------------------------------------------
C
      MFLAG = 0
      IF( TRKELE(8) .GT. EMINLO .AND. ITRKEL(11) .EQ. 2 .AND.
     +    ITRKEL(7) .NE. 0 ) THEN
        RAD = SQRT ( TRKELE(1)**2+TRKELE(2)**2)
        TGTE = ABS ( RAD / TRKELE (3))
        IF( TGTE .GT. .95 .OR. TGTE .LT. .8 ) MFLAG = 1
      ENDIF
C
      END
#endif
