*DK hccrpl
      SUBROUTINE HCCRPL
C --------------------------------------------
C
C!   Builds the plane hit bank
CKEY HCAL HVPLANE HIT / INTERNAL
C!     Author      : G.Catanesi  89/02/03
C!     Mod. by     : L.Silvestris 24/6/92
C!
C!     Input bank  : JDHCSE   McHcTrachSegment
C!                   JDHCHI   McHcHits
C!     Output bank : 'HPHT'   McHcPlanes
C!
C!         Called by: HCASIG
C -----------------------------------------
#ifndef DOC
      SAVE
#include "bcs.h"
#include "jobcom.h"
#include "iocom.h"
#include "hcnamc.h"
#include "hcsejj.h"
#include "hchijj.h"
#include "hphtjj.h"
#include "hccoun.h"
      INTEGER GTSTUP
C
#include "bmacro.h"
C
C       Loop on hits
C
      NHCHI = LROWS (JDHCHI)
      IF(NHCHI.EQ.0) RETURN
      KHCHI = JDHCHI + LMHLEN
      JHPHT = IW(NAHPHT)
      NHPHT = 0
      KHPHT = JHPHT + LMHLEN
C
C     decode date period
C
      IHC = GTSTUP('HC',IRUN)
C
      DO 50 I=1, NHCHI
         NSEG = IW(KHCHI+1)
         IPOR = ITABL (JDHCSE,NSEG,4)
         IMOD = ITABL (JDHCSE,NSEG,3)
         ILAY = ITABL (JDHCSE,NSEG,2)
C
C  decode plane number for different period..
C  89-90-91 double planes in barrel and endcaps
C  92       double planes in barrel and single planes in endcaps
C
      IF(IHC.GT.2) THEN
         IF (IPOR .NE. 2.OR. ILAY.EQ.1) THEN
             NPL = ILAY
         ELSE
             NPL = ILAY/2 + 1
         ENDIF
      ELSE
         IF (ILAY.EQ.1) THEN
             NPL = ILAY
         ELSE
             NPL = ILAY/2 + 1
         ENDIF
      ENDIF
C
         NADR = IPOR*10000 + IMOD*100 + NPL
C
C - If it is not the 1st entry sum up energy on identical plane
         IF(NHPHT.GT.0) THEN
            DO 10 JROW = 1,NHPHT
               IF(NADR.EQ.ITABL(JHPHT,JROW,1)) THEN
                  KJROW = KROW(JHPHT,JROW)
                  RW(KJROW+2) = RW(KJROW+2) + HCSTEN(IW(KHCHI+4))
                  GOTO 50
               ENDIF
   10       CONTINUE
         ENDIF
C
C - Fill 'HPHT' with new plane
         NHPHT = NHPHT + 1
         IW(KHPHT+JHPHPA) = NADR
         RW(KHPHT+JHPHED) = HCSTEN(IW(KHCHI+4))
C
         KHPHT = KHPHT + LHPHTA
C
   50 KHCHI = KHCHI + LHCHIA
      IW(JHPHT+LMHROW) = NHPHT
C
      IF (FHCDEB) THEN
         KHPHT = JHPHT + LMHLEN
         WRITE (LOUTIO,500) IW(JHPHT+LMHROW)
         WRITE (LOUTIO,510) (IW(K+1),RW(K+2),K=KHPHT,
     &                        KHPHT+NHPHT*LHPHTA-1,LHPHTA)
      ENDIF
      RETURN
C
  500 FORMAT (/1X,'+++HCCRPL+++ HPHT McHcPlanes - ', 5X,I3,' planes'/
     +4(4X,'PoMdPl' ,'   Energy', 6X))
  510 FORMAT (4(I10,F10.5,5X))
      END
#endif
