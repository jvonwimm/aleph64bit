*DK asctra
      SUBROUTINE ASCTRA
C ------------------------------------------------------------------
C - F.Ranjard - 860926
C!  Close tracking process
C - Copy GEANT kinematic banks to VERT and KINE banks
C - Print tracking summary if required
C - Called by   ASTRAC                          from this .HLB
C - Calls       PRKINE, ASWKIN                  from this .HLB
C               HFILL, TIMED                    from CERNLIB
C ---------------------------------------------------------
#ifndef DOC
      SAVE
#include "iocom.h"
#include "jobcom.h"
#include "jqcom.h"
#include "gcnum.h"
#include "kincom.h"
      DIMENSION NWORD(LST2)
#include "bmacro.h"
C --------------------------------------------------------------------
C
C - build 'VERT' and 'KINE' banks
C
      CALL ASWKIN
C
C - Compact 'IMPA' banks if required
C
      IF (FGALJO) CALL AUBPRS ('KVOLIMPA')
C
C - Fill pseudo histograms NSTAJO : 1 - total # of tracks / event
C                                   2 - long life tracks / event
C                                   3 - max. stack size / event
      NWORD(1)=NGTMUL
      NWORD(2)=NGTRAC
      NWORD(3)=NGSTMA
      DO 1 I =1,LST2
         N = (NWORD(I)+MBINJO(I)-1)/MBINJO(I)
         IF (N.LE.0) THEN
            NSTAJO(LBIN+1,I) = NSTAJO(LBIN+1,I) + 1
         ELSE IF (N.GT.LBIN) THEN
            NSTAJO(LBIN+2,I) = NSTAJO(LBIN+2,I) + 1
         ELSE
            NSTAJO(N,I) = NSTAJO(N,I) + 1
         ENDIF
         NSTAJO(LBIN+3,I) = MAX (NWORD(I),NSTAJO(LBIN+3,I))
 1    CONTINUE
C
C - Debug
C
      IF (FDEBJO) THEN
         CALL TIMED (TIMEJO(4))
         P = ECMSKI
         WRITE(LOUTIO,801) NEVTJO,P,NGVERT,NGTMUL,NGTRAC,TIMEJO(4)
 801     FORMAT (/1X,'+++ASCTRA+++ EVENT#',I5,' P =',F7.3,' GEV',
     &               '  # of vertices =',I5,'  # of tracks =',I6,
     2              '  # of primaries =',I5,'  time used =',F9.3,' sec')
C
         IF(IPRIJO(16).EQ.1) THEN
            CALL PRKINE
            CALL PRIMPA
         ENDIF
      ENDIF
C
      RETURN
      END
#endif
