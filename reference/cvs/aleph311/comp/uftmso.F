      SUBROUTINE UFTMSO(ITRAC,COVMAT)
C----------------------------------------------------------------
C! For FRFT (2) track ITRAC this routine will calculate the
C! contibution to the covariance matrix coming from multiple
C! scattering between the last coordinate and the point
C! of closest approach to the origin
C!
C!
CKEY COMPUTE FIT
C!    AUTHOR: G.Taylor 29/9/92
C!
C!  INPUT:  ITRAC  = FRFT track number of track being fitted
C!  OUTPUT: COVMAT(15) = covariance matrix coming from multiple
C!                scattering and the origin
C!
C----------------------------------------------------------------
#ifndef DOC
      SAVE NFRTL,NFTCL,NFICL,NFVCL,NTPCO,NICCO,NVDCO
#include "bcs.h"
#include "frtljj.h"
#include "ftcljj.h"
#include "ficljj.h"
#include "fvcljj.h"
#include "frftjj.h"
#include "tpcojj.h"
#include "iccojj.h"
#include "vdcojj.h"
C
      DIMENSION COV(5,5),COVMAT(15)
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
#include "bmacro.h"
C

      IF (FIRST) THEN
        FIRST = .FALSE.
        NFRTL = NAMIND('FRTL')
        NFTCL = NAMIND('FTCL')
        NFICL = NAMIND('FICL')
        NFVCL = NAMIND('FVCL')
        NTPCO = NAMIND('TPCO')
        NICCO = NAMIND('ICCO')
        NVDCO = NAMIND('VDCO')
      END IF
C
C  Zero output
C
      CALL VZERO(COVMAT,15)
C
C Find tracks
C
      KFRFT = NLINK('FRFT',0)
      IF (KFRFT .EQ. 0) THEN
       KFRFT = NLINK('FRFT',2)
      END IF
      KFRTL = IW(NFRTL)
      KFTCL = IW(NFTCL)
      KFICL = IW(NFICL)
      KFVCL = IW(NFVCL)

C -- CHECK THAT INDEX BANKS ARE THERE
      IF (KFRFT.EQ.0 .OR.
     1    KFRTL.EQ.0 .OR.
     1    KFTCL.EQ.0 .OR.
     1    KFICL.EQ.0 ) RETURN

      NTPC = ITABL(KFRTL,ITRAC,JFRTNT)
      NITC = ITABL(KFRTL,ITRAC,JFRTNI)
      NVDT = ITABL(KFRTL,ITRAC,JFRTNV)
C -- CHECK THAT COORDINATE BANKS ARE THERE
      IF (NTPC .GT. 0) THEN
         KTPCO = IW(NTPCO)
         IF (KTPCO .EQ. 0) RETURN
      END IF

      IF (NITC .GT. 0) THEN
         KICCO = IW(NICCO)
         IF (KICCO .EQ. 0) RETURN
      END IF

      IF (NVDT .GT. 0) THEN
         KVDCO = IW(NVDCO)
         IF (KVDCO .EQ. 0) RETURN
      END IF

      NPTPC = ITABL(KFRTL,ITRAC,JFRTIT)
      NPITC = ITABL(KFRTL,ITRAC,JFRTII)
      NPVDT = ITABL(KFRTL,ITRAC,JFRTIV)

      RAD=1000.
C
C---> TPC COORDINATES
C
      IF ( NTPCO .GT. 0 ) THEN
        KTPCO=IW(NTPCO)
        DO 10 I = 1, NTPC
          KSTRT     = KROW(KTPCO,IW(KFTCL+LMHLEN+NPTPC+NTPC-I+1))
          RADIUS    = RW(KSTRT+JTPCRV)
          IF(RADIUS.LT.RAD) RAD=RADIUS
   10   CONTINUE
      ENDIF
C
C---> ITC COORDINATES
C
      IF ( NICCO .GT. 0 ) THEN
        KICCO=IW(NICCO)
        DO  20  I = 1, NITC
          KSTRT  = KROW(KICCO,IABS(IW(KFICL+LMHLEN+NPITC+I)))
          RADIUS = RW(KSTRT+JICCRV)
          IF(RADIUS.LT.RAD) RAD=RADIUS
   20   CONTINUE
      ENDIF
C
C---> GET VDET COORDINATES
C
      IF ( NVDCO .GT. 0 ) THEN
        KVDCO=IW(NVDCO)
        DO  30  I = 1, NVDT
          KSTRT = KROW(KVDCO,IW(KFVCL+LMHLEN+NPVDT+NVDT-I+1))
          RADIUS = RW(KSTRT+JVDCR0)
          IF(RADIUS.LT.RAD) RAD=RADIUS
   30   CONTINUE
      ENDIF
C HERE RAD IS THE RADIUS OF THE INNERMOST POINT ON THE TRACK

      RIN  =  ABS( RTABL(KFRFT,ITRAC,JFRFD0)) + 0.00001
      IF (RIN .GT. RAD) RETURN
      CALL UFMS(RAD,RIN,RW(KROW(KFRFT,ITRAC)+JFRFIR), COV)
      COVMAT(1)  = COV(1,1)
      COVMAT(2)  = COV(2,1)
      COVMAT(3)  = COV(2,2)
      COVMAT(4)  = COV(3,1)
      COVMAT(5)  = COV(3,2)
      COVMAT(6)  = COV(3,3)
      COVMAT(7)  = COV(4,1)
      COVMAT(8)  = COV(4,2)
      COVMAT(9)  = COV(4,3)
      COVMAT(10) = COV(4,4)
      COVMAT(11) = COV(5,1)
      COVMAT(12) = COV(5,2)
      COVMAT(13) = COV(5,3)
      COVMAT(14) = COV(5,4)
      COVMAT(15) = COV(5,5)
      RETURN
      END
#endif
