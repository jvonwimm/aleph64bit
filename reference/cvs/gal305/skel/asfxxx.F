*DK asfxxx
      SUBROUTINE ASFXXX
C ----------------------------------------------------------
C J.Hilgart - 140987
C!  Change the E list. Create Fxxx friendly DST format.
C  The Fxxx banks use the SIMDST reconstruction banks w/o
C  loss of too much information.
C
C - Called from    ASCEVE                  from this .HLB
C   CALLS:         FYKINE,FYRELT,FYFSTR,FYFSCO,FYFTOC,    from ALEPHLIB
C                  FYFLTR,FYFLCO
C   CALLS:         BLIST,BDROP           from BOS.OLB
C
C -------------------------------------------------------------
#ifndef DOC
      SAVE
#include "jqcom.h"
#include "iocom.h"
#include "jobcom.h"
C
C Return if no KINE and no VERT banks exist.
      IF (IW(NAKINE).EQ.0 .OR. IW(NAVERT).EQ.0) THEN
         WRITE (LOUTIO,*)' +++++ NO KINE or VERT banks - RETURN'
         GOTO 911
      ENDIF
C
C Fill FKIN and FVER banks
C DROP the initial banks , do NOT perform GARBage collection
      CALL FYKINE ('DROP','NOGA',IFLAG)
      IF (IFLAG .NE. 0) THEN
        CALL ALTELL ('ASFXXX: not enough space',1,'NEXT')
      ENDIF
 911  RETURN
      END
#endif
