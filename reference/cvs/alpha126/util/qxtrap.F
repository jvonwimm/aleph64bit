      SUBROUTINE QXTRAP(IALTK,LAY1,LAY2)
C**********************************************************
C
C  Author : A. Bonissent. J. Carr, March 03 1997
C
C! Say if a track intersects active regions of VDET
C
C     IALTK = Alpha track number
C     LAY1, LAY0 = 0 if tracks does not intersect active region
C                  1 if tracks does intersect active region
C
C
C***********************************************************
#ifndef DOC
C
      IMPLICIT NONE
      REAL BFLD
      INTEGER IPREVNT,LAY1,LAY2,IALTK,IER,ITRK,KVTXT,NEX,IVHI
      INTEGER IEXFLAG,IWAF,IWFF,ILAY,IPHI,IVIEW
      INTEGER NLINK
      SAVE IPREVNT
      DATA IPREVNT /0/
C
#include "vtxtjj.h"
#include "qdecl.h"
#include "qcde.h"
#include "qmacro.h"
C
      LAY1 = 0
      LAY2 = 0
      IF(KNEVT.NE.IPREVNT)THEN
        BFLD=QMFLD
        CALL VTRKEX(BFLD,IER)
        IF(IER.GT.0 .AND. IER.NE.5) THEN
           WRITE(*,'(A,3I6)') 
     &     ' ***QXTRAP*** ERROR IN VTRKEX Run Evt Error=',
     &       KRUN,KNEVT,IER
        ENDIF
        IPREVNT = KNEVT
      ENDIF
      ITRK = IALTK-KFCHT+1
C     find number of intersections of track with VDET wafers
      KVTXT=NLINK('VTXT',ITRK)
      IF(KVTXT.GT.0) THEN
         NEX=IW(KVTXT+LMHROW)
C        test if extrapolation near edge of wafer in W
C        ( takes something like 3 sigma outside physical region )
         DO IVHI=1,NEX
           IEXFLAG=ITABL(KVTXT,IVHI,JVTXHF)
           IF(IAND(IEXFLAG,16).GT.0)THEN
             IWAF = ITABL(KVTXT,IVHI,JVTXWI)
             CALL VADEWA(IWAF, ILAY, IWFF, IPHI, IVIEW)
             IF(ILAY.EQ.1)LAY1=1
             IF(ILAY.EQ.2)LAY2=1
           ENDIF
         ENDDO
      ENDIF
      RETURN
      END
#endif
