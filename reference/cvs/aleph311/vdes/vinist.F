       SUBROUTINE VINIST
C-----------------------------------------------------
CKEY VDETDES INDEX / USER
C
C!   initialize electronics channel-strip arrays
C     use connected electronics channels, bank VUEC, and VSTREG
C
C ---------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
C -------------------------------------------------
#include "vduefc.h"
#include "velchn.h"
C -----------------------------
      INTEGER IVIEW
      INTEGER NREG, NDIM
      INTEGER ISRGS(8,3)
#include "bcs.h"
#include "bmacrod.h"
      INTEGER nami, kvuec, nfrstz, nfrstp, IVIEWP, IVIEWZ
      INTEGER NR, I,j, ireg, IWAF, idisc
C ----------------------------------------------------------
      DATA IVIEWP, IVIEWZ /2,1/
#include "bmacro.h"
C ----------------------------------------------------------

C Bank VUEC, connected electronics channels,
       nfrstz = VUECH(1) + VEXCH(1)
       nfrstp = VUECH(2) + VEXCH(2)
C --------------------------------------------------------------
C r-phi view
      IVIEW = iviewp

C initialize full array to 'disconnected'
      DO I = 1,7
        DO IDISC = 1, 1024
          IELCHP(IDISC,I) = -1
        ENDDO
      ENDDO

      CALL VSTREG(IVIEW,NREG,NDIM,ISRGS)

      DO IREG = 1, NREG
         j = 0
         DO i = isrgs(1,IREG), isrgs(2,IREG)
            IELCHP(i+nfrstp-1, ISRGS(3,IREG)) = j + isrgs(4,IREG)
            IELCHP(i+nfrstp-1, ISRGS(5,IREG)) = j + isrgs(6,IREG)
            IELCHP(i+nfrstp-1, ISRGS(7,IREG)) = j + isrgs(8,IREG)
              j =  j + 1
         ENDDO
      ENDDO
C ---------------------------------------------------------------------
C z view
      IVIEW = iviewz

C initialize full array to 'disconnected'
      DO IWAF = 1,7
        DO IDISC = 1, 1024
          IELCHZ(IDISC,IWAF) = -1
        ENDDO
      ENDDO
C
C nominal regions
      CALL VSTREG(IVIEW,NREG,NDIM,ISRGS)
C
      DO IREG = 1, NREG
         j = 0
         DO i = isrgs(1,IREG), isrgs(2,IREG)
            IELCHZ(i+nfrstz-1, ISRGS(3,IREG)) = j + isrgs(4,IREG)
            IELCHZ(i+nfrstz-1, ISRGS(5,IREG)) = j + isrgs(6,IREG)
              j =  j + 1
         ENDDO
      ENDDO
C
 999  CONTINUE
C
      RETURN
      END
#endif
